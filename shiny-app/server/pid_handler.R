library(httr)
library(jsonlite)

observe({
  query <- parseQueryString(session$clientData$url_search)
  
  if (!is.null(query$id)) {
    selected <- db %>% filter(PID == query$id)
    if (nrow(selected) == 0) {
      showModal(modalDialog(
        title = "Specimen not found",
        "No matching record for this PID.",
        easyClose = TRUE
      ))
      return()
    }
    
    row <- selected[1, ]
    sci_name <- paste(row$genus, row$species)
    wiki_title <- gsub(" ", "_", sci_name)
    wiki_summary <- NULL
    wiki_link <- paste0("https://en.wikipedia.org/wiki/", wiki_title)
    
    # === Wikipedia API ===
    try({
      wiki_url <- paste0("https://en.wikipedia.org/api/rest_v1/page/summary/", wiki_title)
      res <- GET(wiki_url)
      if (status_code(res) == 200) {
        content_json <- fromJSON(content(res, "text", encoding = "UTF-8"))
        if (!is.null(content_json$extract)) wiki_summary <- content_json$extract
      }
    }, silent = TRUE)
    
    # External links
    sci_encoded <- URLencode(sci_name)
    gbif_link <- paste0("https://www.gbif.org/species/search?q=", sci_encoded)
    eol_link  <- paste0("https://eol.org/search?q=", sci_encoded)
    bold_link <- paste0("https://v4.boldsystems.org/index.php/Taxbrowser_Taxonpage?taxon=", sci_encoded, "&searchTax=Search+Taxonomy")
    
    # === Lookup traits ===
    species_traits <- species_traits_tbl %>%
      filter(species_id == row$species_id) %>%
      select(ncbi_tax_id, accepted_name, eol_id, eol_links)
    
    env_traits <- location_traits_tbl %>%
      filter(location_id == row$location_id) %>%
      select(location, land, lat, lon, elevation, annual_temp, annual_precip)
    
    # Fix date
    if (!is.na(row$D_M_Y)) {
      row$D_M_Y <- as.Date(row$D_M_Y, origin = "1970-01-01")
    }
    
    # === Modal with sections ===
    showModal(modalDialog(
      title = paste("Specimen Info:", row$PID),
      easyClose = TRUE,
      size = "l",
      footer = tagList(modalButton("Close")),
      
      tags$div(style="display:flex; gap:20px;",
               tags$div(
                 tags$img(
                   src = paste0("images/", row$pictures, ".png"),
                   style = "max-width:200px; border:1px solid #ccc; padding:3px;"
                 ),
                 tags$h4(sci_name),
                 tags$h5(row$german_name),
                 tags$h5("External Resources:"),
                 tags$ul(
                   tags$li(tags$a(href = gbif_link, "GBIF", target = "_blank")),
                   tags$li(tags$a(href = eol_link, "Encyclopedia of Life", target = "_blank")),
                   tags$li(tags$a(href = bold_link, "BOLD Systems", target = "_blank")),
                   tags$li(tags$a(href = wiki_link, "Wikipedia", target = "_blank"))
                 )
               ),
               
               tags$div(style="flex:1;",
                        if (!is.null(wiki_summary)) {
                          tagList(
                            tags$h5("Wikipedia Summary:"),
                            tags$p(wiki_summary)
                          )
                        },
                        
                        # === Specimen Info ===
                        tags$h4("Specimen Info"),
                        tags$p(strong("PID:"), row$PID),
                        tags$p(strong("Date:"), format(row$D_M_Y, "%Y-%m-%d")),
                        tags$p(strong("Total number:"), row$total_number_of_this_species),
                        tags$p(strong("FFH directive:"), row$FFH_directive),
                        tags$p(strong("Red List Bavaria:"), row$red_list_Bavaria),
                        tags$p(strong("Breeding:"), row$breeding),
                        tags$p(strong("Collector:"), row$collector),
                        if (!is.na(row$notes_of_the_digitizer)) tags$p(strong("Notes:"), row$notes_of_the_digitizer),
                        
                        # === Species Info ===
                        tags$h4("Species Info"),
                        tags$p(strong("Suborder:"), row$suborder),
                        tags$p(strong("Family:"), row$family),
                        tags$p(strong("Genus:"), row$genus),
                        tags$p(strong("Species:"), row$species),
                        tags$p(strong("Genus & Species:"), row$genus_and_species),
                        tags$p(strong("German Name:"), row$german_name),
                        
                        # Commented out advanced taxonomic info
                        # if (nrow(species_traits) > 0) {
                        #   tagList(
                        #     tags$p(strong("NCBI Tax ID:"), species_traits$ncbi_tax_id),
                        #     tags$p(strong("Accepted Name:"), species_traits$accepted_name),
                        #     tags$p(strong("EOL ID:"), species_traits$eol_id),
                        #     tags$p(strong("EOL Links:"), eol_links_tag)
                        #   )
                        # },
                        
                        # === Environmental Info ===
                        if (nrow(env_traits) > 0) {
                          tagList(
                            tags$h4("Environmental Traits"),
                            tags$p(strong("Location:"), env_traits$location),
                            tags$p(strong("Country:"), env_traits$land),
                            tags$p(strong("Latitude:"), env_traits$lat),
                            tags$p(strong("Longitude:"), env_traits$lon),
                            tags$p(strong("Elevation:"), env_traits$elevation),
                            tags$p(strong("Annual Temp:"), env_traits$annual_temp),
                            tags$p(strong("Annual Precipitation:"), env_traits$annual_precip)
                          )
                        }
               )
      )
    ))
  }
})
