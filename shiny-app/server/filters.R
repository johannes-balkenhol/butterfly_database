## ----------------------------------------------------------------------------
## server/filters.R
## Reactive filtering logic for the butterfly Shiny app
## ----------------------------------------------------------------------------

df_selected_filter <- reactiveVal()
db_table <- reactiveVal()

autoInvalidate <- reactiveTimer(10000)
observe({ autoInvalidate(); cat(".") })

output$o_tree <- renderPlot(NULL)
output$o_table <- renderDataTable(NULL)
output$o_map <- renderLeaflet({ leaflet() %>% addTiles() })
output$o_color <- renderPlotly(NULL)

observeEvent(input$run_button, {
  disable("run_button")
  
  res <- db
  cat("DEBUG: Initial rows:", nrow(res), "\n")
  
  ## ------------------------
  ## Suborder / Family filter
  ## ------------------------
  if (input$day && input$night) {
    if (!is.null(input$family) && length(input$family) > 0) {
      res <- res %>% filter(family %in% trimws(input$family))
      cat("DEBUG: After family filter (day+night):", nrow(res), "\n")
    }
    res <- res %>% filter(suborder %in% c("Rhopalocera", "Heterocera"))
  }
  if (input$day && !input$night) {
    if (!is.null(input$family_day) && length(input$family_day) > 0) {
      res <- res %>% filter(family %in% trimws(input$family_day))
      cat("DEBUG: After family_day filter:", nrow(res), "\n")
    }
    res <- res %>% filter(suborder == "Rhopalocera")
  }
  if (!input$day && input$night) {
    if (!is.null(input$family_night) && length(input$family_night) > 0) {
      res <- res %>% filter(family %in% trimws(input$family_night))
      cat("DEBUG: After family_night filter:", nrow(res), "\n")
    }
    res <- res %>% filter(suborder == "Heterocera")
  }
  if (!input$day && !input$night) {
    res <- res %>% filter(FALSE)
  }
  cat("DEBUG: After suborder/family filters:", nrow(res), "\n")
  
  ## ------------------------
  ## Genus / Species / Names
  ## ------------------------
  if (!is.null(input$genus) && length(input$genus) > 0) {
    res <- res %>% filter(genus %in% trimws(input$genus))
    cat("DEBUG: After genus filter:", nrow(res), "\n")
  }
  if (!is.null(input$species) && length(input$species) > 0) {
    res <- res %>% filter(species %in% trimws(input$species))
    cat("DEBUG: After species filter:", nrow(res), "\n")
  }
  if (!is.null(input$german_name) && length(input$german_name) > 0) {
    res <- res %>% filter(german_name %in% trimws(input$german_name))
    cat("DEBUG: After german_name filter:", nrow(res), "\n")
  }
  if (!is.null(input$land) && length(input$land) > 0) {
    res <- res %>% filter(land %in% trimws(input$land))
    cat("DEBUG: After land filter:", nrow(res), "\n")
  }
  
  ## ------------------------
  ## Elevation
  ## ------------------------
  if ("elevation" %in% names(res)) {
    res$elevation <- as.numeric(res$elevation)
    cat("DEBUG: Elevation range DB:", range(res$elevation, na.rm=TRUE), "\n")
    cat("DEBUG: Elevation input:", input$elevation, "\n")
    
    if (input$elevation[1] == 0) {
      res <- res %>% filter(between(elevation, input$elevation[1], input$elevation[2]) | is.na(elevation))
    } else {
      res <- res %>% filter(between(elevation, input$elevation[1], input$elevation[2]))
    }
    cat("DEBUG: After elevation filter:", nrow(res), "\n")
  }
  
  ## ------------------------
  ## Date Range
  ## ------------------------
  if ("D_M_Y" %in% names(res) && !is.null(input$date) && length(input$date) == 2 &&
      !any(is.na(input$date))) {
    cat("DEBUG: Date class:", class(res$D_M_Y), "\n")
    cat("DEBUG: Date range DB:", range(res$D_M_Y, na.rm=TRUE), "\n")
    cat("DEBUG: Date input:", input$date, "\n")
    
    res <- res %>% filter(!is.na(D_M_Y) & between(D_M_Y, input$date[1], input$date[2]))
    cat("DEBUG: After date filter:", nrow(res), "\n")
  } else {
    cat("DEBUG: Date filter skipped (input empty)\n")
  }
  
  ## ------------------------
  ## Total number
  ## ------------------------
  if ("total_number_of_this_species" %in% names(res)) {
    res$total_number_of_this_species <- as.numeric(res$total_number_of_this_species)
    cat("DEBUG: total_number_of_this_species class:", class(res$total_number_of_this_species), "\n")
    cat("DEBUG: range in DB:", range(res$total_number_of_this_species, na.rm=TRUE), "\n")
    cat("DEBUG: input range:", input$total_number, "\n")
    
    res <- res %>% filter(between(total_number_of_this_species, input$total_number[1], input$total_number[2]))
    cat("DEBUG: After total number filter:", nrow(res), "\n")
  }
  
  ## ------------------------
  ## FFH / Red List
  ## ------------------------
  if (!is.null(input$ffh) && length(input$ffh) > 0) {
    res <- res %>% filter(FFH_directive %in% input$ffh | grepl(paste(input$ffh, collapse = "|"), FFH_directive))
    cat("DEBUG: After FFH filter:", nrow(res), "\n")
  }
  if (!is.null(input$redlist) && length(input$redlist) > 0) {
    res <- res %>% filter(red_list_Bavaria %in% input$redlist)
    cat("DEBUG: After red list filter:", nrow(res), "\n")
  }
  
  
  
  ## ------------------------
  ## Save and handle output
  ## ------------------------
  df_selected_filter(res)
  db_table(setDT(res))
  
  if ("D_M_Y" %in% names(res)) db_table()[, D_M_Y := as.Date(D_M_Y)]
  
  cat("DEBUG: Rows after all filters:", nrow(res), "\n")
  
  if (nrow(res) < 1) {
    shinyalert("No matching data found. Please enter different parameters.", type = "warning", time = 90000)
    enable("run_button")
    return()
  }
  
  enable("run_button")
})


## ----------------------------------------------------------------------------
## Load Example Filters Button
## ----------------------------------------------------------------------------
observeEvent(input$load_example, {
  
  # Suborder
  updateAwesomeCheckbox(session, "day",   value = TRUE)
  updateAwesomeCheckbox(session, "night", value = FALSE)
  
  # Family (day mode)
  updateSelectInput(session, "family_day",
                    selected = "Nymphalidae")
  
  # Genus, species
  updateSelectInput(session, "genus",
                    selected = "Vanessa")
  
  updateSelectInput(session, "species",
                    selected = "Vanessa atalanta")
  
  # Country
  updateSelectInput(session, "land",
                    selected = "Germany")
  
  # Elevation
  updateSliderInput(session, "elevation",
                    value = c(0, 1500))
  
  # Date
  updateDateRangeInput(session, "date",
                       start = "1950-01-01",
                       end   = "2020-12-31")
  
  # Total number slider
  updateSliderInput(session, "total_number",
                    value = c(1, 20))
  
  # Options
  updateAwesomeCheckbox(session, "pictures", value = TRUE)
  updateAwesomeCheckbox(session, "digitizer", value = FALSE)
  
  # Optional: automatically trigger search
  session$sendCustomMessage("click_run_button", list())
})
  