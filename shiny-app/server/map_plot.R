## ----------------------------------------------------------------------------
## server/map_plot.R
## This script renders the interactive leaflet map and selected filter text
## ----------------------------------------------------------------------------

observe({
  req(db_table())   # ensures data exists before running
  
  local_table <- db_table()  # get actual dataframe from reactiveVal
  
  # Use lat/lon directly from specimen_full (db)
  db_map <- local_table %>%
    filter(!is.na(lat) & !is.na(lon)) %>%
    mutate(popup_text = paste0(
      "<strong>", family, " ", genus, " ", species, "</strong><br>",
      "Location: ", land, ", ", location, "<br>",
      "Date: ", as.character(D_M_Y), "<br>",
      "Total number: ", total_number_of_this_species, "<br>",
      "German name: ", german_name
    ))
  
  # Define families and icons
  data_folder <- "www/mapicons"
  families <- c(
    "Hesperiidae", "Lycaenidae", "Nymphalidae", "Papilionidae", "Pieridae", "Riodinidae",
    "Brahmaeidae", "Cossidae", "Drepanidae", "Endromidae", "Erebidae", "Euteliidae",
    "Geometridae", "Hepialidae", "Lasiocampidae", "Limacodidae", "Noctuidae", "Nolidae",
    "Notodontidae", "Saturniidae", "Sphingidae"
  )
  
  # Create icon list
  icon_list <- lapply(families, function(fam) {
    icon_path <- file.path(data_folder, paste0(tolower(fam), "_icon.png"))
    if (file.exists(icon_path)) {
      makeIcon(iconUrl = icon_path, iconWidth = 35, iconHeight = 35)
    } else NULL
  })
  
  valid_families <- families[!sapply(icon_list, is.null)]
  butterflyicons <- do.call(iconList, setNames(icon_list[!sapply(icon_list, is.null)], valid_families))
  
  # Legend generator
  rhopalocera <- c("Hesperiidae", "Lycaenidae", "Nymphalidae", "Papilionidae", "Pieridae", "Riodinidae")
  heterocera <- c("Brahmaeidae", "Cossidae", "Drepanidae", "Endromidae", "Erebidae", "Euteliidae",
                  "Geometridae", "Hepialidae", "Lasiocampidae", "Limacodidae", "Noctuidae", "Nolidae",
                  "Notodontidae", "Saturniidae", "Sphingidae")
  
  generate_html_legend <- function(families, title, icon_path) {
    html <- paste0("<b>", title, "</b><br/>")
    html <- paste0(html, paste0(
      "<img src='", icon_path, "/", tolower(families), "_icon.png' ",
      "style='width:25px;height:25px;'>", families, "<br/>", collapse = ""
    ))
    html
  }
  
  mapicon_path <- "mapicons"
  html_legend <- NULL
  if (input$day && input$night) {
    html_legend <- paste0(
      generate_html_legend(rhopalocera, "Rhopalocera", mapicon_path),
      generate_html_legend(heterocera, "Heterocera", mapicon_path)
    )
  } else if (input$day) {
    html_legend <- generate_html_legend(rhopalocera, "Rhopalocera", mapicon_path)
  } else if (input$night) {
    html_legend <- generate_html_legend(heterocera, "Heterocera", mapicon_path)
  }
  
  # Render leaflet map
  if (nrow(db_map) >= 1) {
    output$o_map <- renderLeaflet({
      leaflet(data = db_map) %>%
        addTiles() %>%
        addMarkers(
          ~lon, ~lat,
          icon = ~butterflyicons[family],
          label = ~as.character(pictures),
          popup = ~as.character(popup_text),
          clusterOptions = markerClusterOptions(
            iconCreateFunction = JS("
              function (cluster) {
                var count = cluster.getChildCount();
                var c = ' marker-custom-';
                if (count > 100) {
                  c += 'large';
                } else if (count > 50) {
                  c += 'medium';
                } else {
                  c += 'small';
                }
                return new L.DivIcon({
                  html: '<div><span>' + count + '</span></div>',
                  className: 'marker-cluster' + c,
                  iconSize: new L.Point(40, 40)
                });
              }")
          )
        ) %>%
        addControl(html = html_legend, position = "bottomleft")
    })
  } else {
    output$o_map <- renderLeaflet({
      leaflet() %>% addTiles()
    })
  }
  
  # ------------------------------------------------------------------------
  # Selected filter text outputs (unchanged)
  # ------------------------------------------------------------------------
  output$o_suborder <- if (input$day && input$night) {
    renderText("suborder: Rhopalocera and Heterocera (butterflies and moths)")
  } else if (input$day) {
    renderText("suborder: Rhopalocera (butterflies)")
  } else if (input$night) {
    renderText("suborder: Heterocera (moths)")
  }
  
  output$o_family <- if (!is.null(input$family)) {
    renderText({ HTML(paste("family:", paste(isolate(input$family), collapse = ", "))) })
  }
  
  output$o_genus <- if (!is.null(input$genus)) {
    renderText({ HTML(paste("genus:", paste(isolate(input$genus), collapse = ", "))) })
  }
  
  output$o_species <- if (!is.null(input$species)) {
    renderText({ HTML(paste("species:", paste(isolate(input$species), collapse = ", "))) })
  }
  
  output$o_german_name <- if (!is.null(input$german_name)) {
    renderText({ HTML(paste("german name:", paste(isolate(input$german_name), collapse = ", "))) })
  }
  
  output$o_land <- if (!is.null(input$land)) {
    renderText({ HTML(paste("land:", paste(isolate(input$land), collapse = ", "))) })
  }
  
  output$o_elevation <- renderText({
    if (input$elevation[1] == 0) {
      paste("Elevation from", isolate(input$elevation[1]), "to", isolate(input$elevation[2]), "meters and butterflies with no elevation data.")
    } else {
      paste("Elevation from", isolate(input$elevation[1]), "to", isolate(input$elevation[2]), "meters.")
    }
  })
  
  output$o_date <- renderText({
    paste("Date from", isolate(input$date[1]), "to", isolate(input$date[2]), ".")
  })
  
  output$o_total_number <- renderText({
    paste("Total number of a species from", isolate(input$total_number[1]), "to", isolate(input$total_number[2]), "butterflies.")
  })
  
  output$o_pictures <- renderText({
    if (input$pictures) "Pictures on." else "Pictures off."
  })
  
  output$o_ffh <- if (!is.null(input$ffh)) {
    renderText({
      str <- paste(isolate(input$ffh), collapse = ", ")
      str <- gsub("(II)", "Annex II species", gsub("(IV)", "Annex IV species", str))
      HTML(paste("FFH directive:", str))
    })
  }
  
  output$o_redlist <- renderText({
    if (is.null(input$redlist)) {
      "Red list Bavaria last updated: Rhopalocera 2016, Heterocera 2003"
    } else {
      str <- paste(isolate(input$redlist), collapse = ", ")
      str <- gsub("0", "0 (extinct)",
                  gsub("1", "1 (threatened by extinction)",
                       gsub("2", "2 (critically endangered)",
                            gsub("3", "3 (endangered)",
                                 gsub("G", "G (danger of unknown extent)",
                                      gsub("V", "V (prewarn list)",
                                           gsub("＊", "＊ (not endangered)",
                                                gsub("D", "D (data deficient)",
                                                     gsub("R", "R (extremely rare)",
                                                          gsub("♦", "♦ (not evaluated)",
                                                               gsub("-", "- (not included)", str)))))))))))
      HTML(paste("Red list Bavaria (last updated: Rhopalocera 2016, Heterocera 2003):", str))
    }
  })
  
  output$o_digitizer <- renderText({
    if (input$digitizer) "Notes of the digitizer on." else "Notes of the digitizer off."
  })
  
  enable("run_button")
})
