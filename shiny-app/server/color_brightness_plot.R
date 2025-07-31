observeEvent(input$color_button, {
  
  if (length(input$color_butterfly_name) > 0) {
    
    df_selected_color <- db_color %>%
      filter(genus_and_species %in% input$color_butterfly_name) %>%
      filter(
        between(as.numeric(elevation), input$color_height[1], input$color_height[2]),
        between(as.numeric(color_brightness), input$color[1], input$color[2])
      )
    
    db_color_filtered <- setDT(df_selected_color)
    db_color_filtered$color_brightness <- as.numeric(db_color_filtered$color_brightness)
    db_color_filtered$elevation <- as.numeric(db_color_filtered$elevation)
    db_color_filtered <- setorder(db_color_filtered, cols = "elevation")
    
    if (nrow(db_color_filtered) == 0) {
      shinyalert("No data for this selection. Please select different parameters.", type = "warning")
      return()
    }
    
    # Species-wise regression lines with same x-values as points
    reg_lines <- list()
    for (sp in unique(db_color_filtered$genus_and_species)) {
      df_sp <- db_color_filtered[genus_and_species == sp]
      if (nrow(df_sp) > 1) {
        model <- lm(color_brightness ~ elevation, data = df_sp)
        reg_df <- data.frame(
          elevation = df_sp$elevation,
          color_brightness = predict(model, newdata = df_sp),
          genus_and_species = sp
        )
        reg_lines[[sp]] <- reg_df
      }
    }
    reg_lines_df <- if (length(reg_lines) > 0) do.call(rbind, reg_lines) else NULL
    
    # Plot scatter points
    output$o_color <- renderPlotly({
      p <- plot_ly(
        db_color_filtered,
        x = ~elevation,
        y = ~color_brightness,
        color = ~genus_and_species,
        type = "scatter",
        mode = "markers",
        text = ~paste("Location:", location)
      ) %>%
        layout(
          xaxis = list(title = "Elevation [m]"),
          yaxis = list(title = "Relative Color Brightness"),
          legend = list(title = list(text = "Species"))
        )
      
      # Add regression lines per species
      if (!is.null(reg_lines_df)) {
        p <- p %>%
          add_trace(
            data = reg_lines_df,
            x = ~elevation,
            y = ~color_brightness,
            color = ~genus_and_species,
            type = "scatter",
            mode = "lines",
            showlegend = FALSE,
            inherit = FALSE
          )
      }
      p
    })
    
    # Correlation
    if (length(unique(db_color_filtered$elevation)) > 1 &&
        length(unique(db_color_filtered$color_brightness)) > 1) {
      
      correlation <- round(
        cor(db_color_filtered$elevation, db_color_filtered$color_brightness, method = "pearson"),
        digits = 3
      )
      
      result <- cor.test(
        db_color_filtered$elevation,
        db_color_filtered$color_brightness,
        method = "pearson"
      )
      pvalue <- round(result$p.value, digits = 3)
      
      output$o_correlation <- if (pvalue < 0.05) {
        renderText({
          paste("Pearson correlation coefficient:", correlation,
                "; p-value:", pvalue,
                "; The correlation is statistically significant.")
        })
      } else {
        renderText({
          paste("Pearson correlation coefficient:", correlation,
                "; p-value:", pvalue,
                "; The correlation is not statistically significant.")
        })
      }
      
    } else {
      output$o_correlation <- renderText({
        "Not enough data to calculate the correlation."
      })
    }
    
  } else {
    shinyalert("No butterfly species selected. Please select at least one.", type = "warning", time = 90000)
  }
  
}, ignoreInit = FALSE)
