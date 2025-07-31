## ----------------------------------------------------------------------------
## server/table_output.R
## Generates the interactive butterfly table with or without
## images and digitizer notes, depending on input$pictures and input$digitizer
## ----------------------------------------------------------------------------

observe({
  req(db_table())  # ensure filtered data exists before running
  
  local_table <- db_table()
  
  # Rename columns for display
  rename_map <- c(
    "german_name" = "german name",
    "D_M_Y" = "date",
    "total_number_of_this_species" = "total number of this species",
    "FFH_directive" = "FFH directive",
    "red_list_Bavaria" = "red list Bavaria",
    "notes_of_the_digitizer" = "notes of the digitizer"
  )
  colnames(local_table) <- ifelse(
    colnames(local_table) %in% names(rename_map),
    rename_map[colnames(local_table)],
    colnames(local_table)
  )
  
  # Define desired column order
  core_cols <- c(
    "PID_link", "pictures", "family", "genus", "species", "suborder",
    "german name", "location", "land", "elevation", "date",
    "total number of this species", "FFH directive", "red list Bavaria",
    "breeding", "collector"
  )
  
  # Helper to resolve image path or fallback
  get_image_tag <- function(name) {
    base <- file.path("www", "images", name)
    exts <- c(".png", ".jpg", ".JPG")
    file_found <- NULL
    
    for (ext in exts) {
      candidate <- paste0(base, ext)
      if (file.exists(candidate)) {
        file_found <- paste0("images/", name, ext)
        break
      }
    }
    if (is.null(file_found)) {
      file_found <- "images/placeholder.png"
    }
    paste0("<img class='small-img' src='", file_found, "' height='30'></img>")
  }
  
  # -----------------------------
  # Table rendering logic
  # -----------------------------
  
  if (input$pictures && !input$digitizer && nrow(local_table) > 0) {
    df <- local_table %>%
      select(any_of(core_cols)) %>%
      rowwise() %>%
      mutate(pictures = get_image_tag(pictures)) %>%
      ungroup()
    
    output$o_table <- renderDataTable({
      datatable(df, escape = FALSE,
                options = list(scrollX = TRUE, fixedHeader = TRUE),
                extensions = "Scroller",
                rownames = FALSE)
    })
  }
  
  if (input$pictures && input$digitizer && nrow(local_table) > 0) {
    df <- local_table %>%
      select(any_of(c(core_cols, "notes of the digitizer"))) %>%
      rowwise() %>%
      mutate(pictures = get_image_tag(pictures)) %>%
      ungroup()
    
    output$o_table <- renderDataTable({
      datatable(df, escape = FALSE,
                options = list(scrollX = TRUE, fixedHeader = TRUE),
                extensions = "Scroller",
                rownames = FALSE)
    })
  }
  
  if (!input$pictures && !input$digitizer) {
    df <- local_table %>% select(any_of(core_cols))
    output$o_table <- renderDataTable({
      datatable(df, escape = FALSE,
                options = list(scrollX = TRUE, fixedHeader = TRUE),
                extensions = "Scroller",
                rownames = FALSE)
    })
  }
  
  if (!input$pictures && input$digitizer) {
    df <- local_table %>% select(any_of(c(core_cols, "notes of the digitizer")))
    output$o_table <- renderDataTable({
      datatable(df, escape = FALSE,
                options = list(scrollX = TRUE, fixedHeader = TRUE),
                extensions = "Scroller",
                rownames = FALSE)
    })
  }
  
  # -----------------------------
  # Download Handlers
  # -----------------------------
  
  output$download_data <- downloadHandler(
    filename = function() paste0("butterfly_data_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- df_selected_filter() %>% select(any_of(core_cols))
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  output$download_csv <- downloadHandler(
    filename = function() paste0("butterfly_full_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- db_table() %>% select(any_of(core_cols))
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  output$download_abcd <- downloadHandler(
    filename = function() paste0("butterfly_data_", Sys.Date(), ".xml"),
    content = function(file) {
      data <- df_selected_filter()
      
      root <- xml2::xml_new_root("DataSet", xmlns = "http://www.tdwg.org/schemas/abcd/2.06")
      unit_list <- xml2::xml_add_child(root, "Units")
      
      for (i in seq_len(nrow(data))) {
        row <- data[i, ]
        unit <- xml2::xml_add_child(unit_list, "Unit")
        
        xml2::xml_add_child(unit, "UnitID", row$PID)
        taxon <- xml2::xml_add_child(unit, "Identification")
        xml2::xml_add_child(taxon, "ScientificName", paste(row$genus, row$species))
        
        gathering <- xml2::xml_add_child(unit, "Gathering")
        xml2::xml_add_child(gathering, "Country", row$land)
        xml2::xml_add_child(gathering, "Locality", row$location)
        xml2::xml_add_child(gathering, "Date", as.character(row$date))
        
        if (!is.na(row$collector)) {
          xml2::xml_add_child(unit, "Collector", row$collector)
        }
      }
      
      xml2::write_xml(root, file, options = "format")
    }
  )
})
