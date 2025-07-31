################################################################################
## Butterflies Database - Shiny Global Setup
##
## NOTE: For small to medium datasets (< 1M rows), it is efficient to load
## all tables into memory at startup. For very large datasets (> 1M rows),
## consider using SQLite queries on-demand with dbGetQuery() or dplyr::tbl().
################################################################################

################################################################################
## Load Libraries
################################################################################

library(shiny)            ## Shiny interface 
library(shinydashboard)   ## dashboard for Shiny (sidebar, main dashboard)
library(shinyWidgets)     ## widgets for Shiny
library(shinyjs)          ## JavaScript in Shiny
library(shinycssloaders)  ## loading animation
library(shinyalert)       ## alert popup
library(DT)               ## render tables
library(data.table)       ## convert dataframe to datatable
library(dplyr)            ## work with tables
library(tidyr)            ## work with tables
library(leaflet)          ## geographic map
library(igraph)           ## plot family tree
library(ggraph)           ## render family tree
library(plotly)           ## color brightness diagram + loading animation
library(xml2)
library(DBI)
library(RSQLite)

## Add Resource Paths for Images and Map Icons
shiny::addResourcePath("mapicons", "www/mapicons")
shiny::addResourcePath("images", "www/images")

################################################################################
## Connect to SQLite and Load All Tables into Memory
################################################################################

con <- dbConnect(RSQLite::SQLite(), "data/butterfly_collection.sqlite")

species_tbl         <- dbReadTable(con, "species")
species_traits_tbl  <- dbReadTable(con, "species_traits")
location_tbl        <- dbReadTable(con, "location")
location_traits_tbl <- dbReadTable(con, "location_traits")
specimen_tbl        <- dbReadTable(con, "specimen")
db_color            <- dbReadTable(con, "color_brightness")

dbDisconnect(con)

################################################################################
## Build Joined Specimen Table for Shiny
################################################################################
specimen_full <- specimen_tbl %>%
  select(PID, suborder, family, species_id, location_id,
         D_M_Y, date_accuracy, total_number_of_this_species,
         FFH_directive, red_list_Bavaria, breeding, collector,
         notes_of_the_digitizer) %>%
  left_join(
    species_tbl %>%
      select(species_id, genus, species, genus_and_species, german_name),
    by = "species_id"
  ) %>%
  left_join(
    location_tbl %>%
      select(location_id, location, land, lat, lon, elevation),
    by = "location_id"
  ) %>%
  left_join(location_traits_tbl, by = "location_id") %>%
  mutate(
    PID_link = paste0("<a href='?id=", PID, "'>", PID, "</a>"),
    D_M_Y = as.Date(D_M_Y)
  )

# clean up .x and .y suffixes
colnames(specimen_full) <- gsub("\\.x$", "", colnames(specimen_full))
colnames(specimen_full) <- gsub("\\.y$", "", colnames(specimen_full))

# Remove duplicated columns (keep the first occurrence)
specimen_full <- specimen_full[, !duplicated(colnames(specimen_full))]


################################################################################
## Data type formatting and extra columns
################################################################################

# Ensure D_M_Y is Date
specimen_full$D_M_Y <- as.Date(specimen_full$D_M_Y)

# Ensure character columns are actually character (avoids factor/filter mismatch)
char_cols <- sapply(specimen_full, is.character)
specimen_full[char_cols] <- lapply(specimen_full[char_cols], trimws)

# Add pictures column for table_output.R
specimen_full <- specimen_full %>%
  mutate(pictures = paste(genus, species))

# Remove duplicated columns if still present
specimen_full <- specimen_full[, !duplicated(colnames(specimen_full))]

################################################################################
## Prepare Vectors for UI Filters
################################################################################
db_genus        <- unique(specimen_full$genus[order(specimen_full$genus)])
db_species      <- unique(specimen_full$species[order(specimen_full$species)])
db_german_name  <- unique(specimen_full$german_name[order(specimen_full$german_name)])
db_land         <- unique(specimen_full$land[order(specimen_full$land)])


################################################################################
## FAIR-compliant Links
################################################################################
pid_base <- "https://butterflies.uni-wuerzburg.de/collection/"



################################################################################
## format db object
################################################################################
# assign main DB
db <- specimen_full

# create pictures column for compatibility
db <- db %>%
  mutate(pictures = paste(genus, species))

# fix date format (convert numeric to Date if needed)
if (is.numeric(db$D_M_Y)) {
  db$D_M_Y <- as.Date(as.POSIXct(db$D_M_Y, origin = "1970-01-01"))
}

# Prepare UI vectors
db_genus        <- unique(db$genus[order(db$genus)])
db_species      <- unique(db$species[order(db$species)])
db_german_name  <- unique(db$german_name[order(db$german_name)])
db_land         <- unique(db$land[order(db$land)])