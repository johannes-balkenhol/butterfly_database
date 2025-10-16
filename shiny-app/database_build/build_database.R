## ---- Required libraries ----
library(readxl)
library(DBI)
library(RSQLite)
library(dplyr)
library(httr)
library(jsonlite)
library(lubridate)
library(tidyr)

## ---- Load Excel files ----
db_main  <- read_excel("data/butterfly_database_with_pid.xlsx")
db_color <- read_excel("data/butterfly_color_brightness.xlsx")

## ---- Normalize Basic Tables ----
species_tbl <- db_main %>%
  select(genus, species, genus_and_species, german_name) %>% 
  distinct() %>%
  arrange(genus_and_species) %>%
  mutate(species_id = row_number())

## ---- Location table ----
location_tbl <- db_main %>%
  filter(!is.na(GPS_data)) %>%
  separate(GPS_data, into = c("lat", "lon"), sep = ",", extra = "drop", convert = TRUE) %>%
  mutate(
    lat = round(as.numeric(lat), 6),
    lon = round(as.numeric(lon), 6),
    GPS_data = paste(lat, lon, sep = ", ")  
  ) %>%
  distinct(lat, lon, .keep_all = TRUE) %>%  
  select(location, land, GPS_data, lat, lon, elevation) %>%
  arrange(location, lat, lon) %>%
  mutate(location_id = row_number())

## ---- Specimen table ----
specimen_tbl <- db_main %>%
  left_join(species_tbl, by = c("genus", "species", "genus_and_species", "german_name")) %>%  
  filter(!is.na(GPS_data)) %>%
  separate(GPS_data, into = c("lat", "lon"), sep = ",", extra = "drop", convert = TRUE) %>%
  mutate(
    lat = round(as.numeric(lat), 6),
    lon = round(as.numeric(lon), 6),
    D_M_Y = if (inherits(D_M_Y, "Date")) D_M_Y else as.Date(D_M_Y, origin = "1970-01-01"),
    total_number_of_this_species = as.numeric(total_number_of_this_species)
  ) %>%
  left_join(location_tbl %>% select(lat, lon, location_id),
            by = c("lat", "lon")) %>%
  mutate(
    pictures = paste(genus, species)
  ) %>%
  select(PID, suborder, family, genus, species, genus_and_species, german_name, 
         species_id, location_id,
         D_M_Y, date_accuracy, total_number_of_this_species,
         FFH_directive, red_list_Bavaria, breeding, collector,
         notes_of_the_digitizer, pictures)


## ---- Combined fetch function (GBIF + NCBI + Wikipedia + EOL links) ----
fetch_species_traits <- function(sci_name, species_id) {
  encoded <- URLencode(sci_name)
  
  ### ---- GBIF ----
  gbif_url <- paste0("https://api.gbif.org/v1/species/match?name=", encoded)
  gbif_res <- tryCatch(fromJSON(gbif_url), error = function(e) NULL)
  gbif_id   <- if(!is.null(gbif_res$key)) gbif_res$key else NA
  accepted  <- if(!is.null(gbif_res$scientificName)) gbif_res$scientificName else NA
  rank      <- if(!is.null(gbif_res$rank)) gbif_res$rank else NA
  status    <- if(!is.null(gbif_res$status)) gbif_res$status else NA
  
  ### ---- NCBI fallback ----
  ncbi_id <- NA
  if (is.na(gbif_id)) {
    ncbi_url <- paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=taxonomy&term=", encoded, "&retmode=json")
    ncbi_res <- tryCatch(fromJSON(ncbi_url), error = function(e) NULL)
    if (!is.null(ncbi_res) &&
        "esearchresult" %in% names(ncbi_res) &&
        length(ncbi_res$esearchresult$idlist) > 0) {
      ncbi_id <- ncbi_res$esearchresult$idlist[[1]]
    }
  }
  
  ### ---- Wikipedia ----
  lookup_name <- gsub("[\\(\\),]", "", sci_name)    # remove () and commas
  lookup_name <- gsub(" ", "_", lookup_name)        # replace spaces with _
  wiki_url1 <- paste0("https://en.wikipedia.org/api/rest_v1/page/summary/", lookup_name)
  
  wiki_res <- tryCatch(fromJSON(wiki_url1), error = function(e) NULL)
  if (is.null(wiki_res) || (!is.null(wiki_res$type) && grepl("not_found", wiki_res$type))) {
    # fallback to genus
    genus <- strsplit(sci_name, " ")[[1]][1]
    wiki_url2 <- paste0("https://en.wikipedia.org/api/rest_v1/page/summary/", genus)
    wiki_res <- tryCatch(fromJSON(wiki_url2), error = function(e) NULL)
  }
  wiki_desc <- if(!is.null(wiki_res) && !is.null(wiki_res$extract)) wiki_res$extract else NA
  wiki_page <- if(!is.null(wiki_res) && !is.null(wiki_res$content_urls$desktop$page)) wiki_res$content_urls$desktop$page else NA
  
  ### ---- EOL IDs & links ----
  eol_ids <- c()
  eol_links <- c()
  search_url <- paste0("https://eol.org/api/search/1.0.json?q=", encoded, "&page=1&exact=true")
  
  res <- tryCatch(GET(search_url, add_headers(`User-Agent` = "R-fetch")), error=function(e) NULL)
  if (!is.null(res)) {
    json_res <- tryCatch(fromJSON(content(res, as="text", encoding="UTF-8")), error=function(e) NULL)
    if (!is.null(json_res) && "results" %in% names(json_res) && length(json_res$results) > 0) {
      eol_ids <- json_res$results$id
      eol_links <- paste0("https://eol.org/pages/", eol_ids)
    }
  }
  
  cat("\nSpecies:", sci_name, "\n")
  cat("EOL IDs:", if(length(eol_ids)>0) paste(eol_ids, collapse="; ") else "NA", "\n")
  cat("EOL Links:", if(length(eol_links)>0) paste(eol_links, collapse="; ") else "NA", "\n")
  
  ### ---- Return ----
  data.frame(
    species_id        = species_id,
    genus_and_species = sci_name,
    tax_id            = ifelse(!is.na(gbif_id), gbif_id, ncbi_id),
    gbif_id           = gbif_id,
    ncbi_tax_id       = ncbi_id,
    accepted_name     = accepted,
    taxonomic_rank    = rank,
    taxonomic_status  = status,
    wikipedia_desc    = wiki_desc,
    wikipedia_url     = wiki_page,
    eol_id            = if(length(eol_ids)>0) paste(eol_ids, collapse="; ") else NA,
    eol_links         = if(length(eol_links)>0) paste(eol_links, collapse="; ") else NA,
    stringsAsFactors = FALSE
  )
}



#test_species <- species_tbl[1:5, ] ## Fetch for first 5 species

species_traits_tbl <- do.call(
  rbind,
  mapply(fetch_species_traits,
         sci_name = species_tbl$genus_and_species,
         species_id = species_tbl$species_id,
         SIMPLIFY = FALSE)
)

## Merge back into species table
species_tbl <- species_tbl %>%
  left_join(species_traits_tbl %>%
              select(species_id, tax_id, gbif_id, ncbi_tax_id, accepted_name,
                     taxonomic_rank, taxonomic_status, wikipedia_desc,
                     wikipedia_url, eol_id, eol_links),
            by = "species_id")

## Preview
print(head(species_traits_tbl))
print(head(species_tbl))


## ---- Function to fetch encironmental traits traits ----
fetch_env_traits <- function(lat, lon) {
  url <- paste0(
    "https://archive-api.open-meteo.com/v1/archive?",
    "latitude=", lat, "&longitude=", lon,
    "&start_date=1991-01-01&end_date=2020-12-31",
    "&daily=temperature_2m_mean,precipitation_sum&timezone=GMT"
  )
  
  res <- tryCatch(fromJSON(url), error=function(e) NULL)
  
  if (!is.null(res$daily)) {
    temp_mean <- mean(res$daily$temperature_2m_mean, na.rm = TRUE)
    precip_mean <- mean(res$daily$precipitation_sum, na.rm = TRUE) * 365
    
    return(data.frame(
      lat = lat,
      lon = lon,
      annual_temp = round(temp_mean, 2),
      annual_precip = round(precip_mean, 2),
      stringsAsFactors = FALSE
    ))
  } else {
    return(data.frame(
      lat = lat,
      lon = lon,
      annual_temp = NA,
      annual_precip = NA,
      stringsAsFactors = FALSE
    ))
  }
}

## ---- Extract & deduplicate coordinates ----
coords <- location_tbl %>%
  filter(!is.na(GPS_data)) %>%
  separate(GPS_data, into=c("lat","lon"), sep=",", convert=TRUE) %>%
  distinct(lat, lon, .keep_all = TRUE)   # ✅ ensure unique locations

## ---- Fetch environmental traits ----
env_traits_tbl <- do.call(
  rbind,
  mapply(fetch_env_traits, coords$lat, coords$lon, SIMPLIFY = FALSE)
)

## ---- Join traits back to all locations ----
location_traits_tbl <- coords %>%
  left_join(env_traits_tbl, by = c("lat","lon"))

## ---- Preview ----
print(head(location_traits_tbl))



## Build SQLite Database
con <- dbConnect(RSQLite::SQLite(), "data/butterfly_collection.sqlite")

dbWriteTable(con, "species", species_tbl, overwrite=TRUE)
dbWriteTable(con, "species_traits", species_traits_tbl, overwrite=TRUE)
dbWriteTable(con, "location", location_tbl, overwrite=TRUE)
dbWriteTable(con, "location_traits", location_traits_tbl, overwrite=TRUE)
dbWriteTable(con, "specimen", specimen_tbl, overwrite=TRUE)
dbWriteTable(con, "color_brightness", db_color, overwrite=TRUE)

dbDisconnect(con)




