sidebar <- dashboardSidebar(
  tags$head(includeScript("www/submit_via_enter.js")),
  width = 250,
  sidebarMenu(
    fluidRow(
      useShinyjs(),
      chooseSliderSkin("Flat", color = "orange"),
      
      ## Suborder filters
      column(11, 
             h5(HTML("&emsp;"), "suborder (select at least one)"),
             style = "margin-bottom:+25px;"
      ),
      column(11, 
             awesomeCheckbox("day", "Rhopalocera on/off",
                             value = TRUE, status = "warning"),
             style = "margin-bottom:-25px;"
      ),
      column(11,
             awesomeCheckbox("night", "Heterocera on/off", 
                             value = TRUE, status = "warning"),
             style = "margin-bottom:-20px;"
      ),
      
      ## Family filters
      conditionalPanel(condition="input.day == 1 && input.night == 1",
                       column(11, 
                              selectInput("family", h5("family"), 
                                          choices = sort(unique(db$family)), 
                                          multiple = TRUE, selected = ""),
                              style = "margin-bottom:-25px;"
                       )
      ),
      conditionalPanel(condition="input.day == 1 && input.night == 0",
                       column(11,
                              selectInput("family_day", h5("family"), 
                                          choices = sort(unique(db$family[db$suborder == "Rhopalocera"])), 
                                          multiple = TRUE, selected = ""),
                              style = "margin-bottom:-25px;"
                       )
      ),
      conditionalPanel(condition="input.day == 0 && input.night == 1",
                       column(11,
                              selectInput("family_night", h5("family"), 
                                          choices = sort(unique(db$family[db$suborder == "Heterocera"])), 
                                          multiple = TRUE, selected = ""),
                              style = "margin-bottom:-25px;"
                       )
      ),
      conditionalPanel(condition="input.day == 0 && input.night == 0",
                       column(11,
                              selectInput("empty", h5("family"), 
                                          choices = list("no suborder selected"), 
                                          multiple = TRUE, selected = ""),
                              style = "margin-bottom:-25px;"
                       )
      ),
      
      ## Genus
      column(11,
             selectInput("genus", h5("genus"), 
                         choices = db_genus, 
                         multiple = TRUE, selected = ""),
             style = "margin-bottom:-20px;"
      ),
      
      ## Species
      column(11,
             selectInput("species", h5("species"), 
                         choices = db_species, 
                         multiple = TRUE, selected = ""),
             style = "margin-bottom:-20px;"
      ), 
      
      ## German name
      column(11,
             selectInput("german_name", h5("german name"), 
                         choices = db_german_name, 
                         multiple = TRUE, selected = ""),
             style = "margin-bottom:-20px;"
      ),
      
      ## Land (Country)
      column(11,
             selectInput("land", h5("land"), 
                         choices = db_land, 
                         multiple = TRUE, selected = ""),
             style = "margin-bottom:-20px;"
      ),
      
      ## Elevation
      column(11,
             sliderInput("elevation", h5 ("elevation of the location", br(), ("(0 ≙ no data available)")), 
                         min = 0 , max = 3000, value = c(0,3000)),
             style = "margin-bottom:-20px;"
      ),
      
      ## Date
      column(11,
             dateRangeInput("date", h5("date"),
                            start = min(db$D_M_Y, na.rm = TRUE),
                            end = max(db$D_M_Y, na.rm = TRUE),
                            min = min(db$D_M_Y, na.rm = TRUE),
                            max = max(db$D_M_Y, na.rm = TRUE),
                            format = "yyyy-mm-dd", startview = "decade", language = "en", separator = "to"),
             style = "margin-bottom:-20px;"
      ),
      
      ## Total number slider
      column(11,
             sliderInput("total_number", h5 ("total number of a species"),
                         min = 0, max = max(db$total_number_of_this_species, na.rm = TRUE),
                         value = c(1, max(db$total_number_of_this_species, na.rm = TRUE))),
             style = "margin-bottom:-20px;"
      ),
      
      ## Pictures on/off
      column(11,
             awesomeCheckbox("pictures", "pictures on/off", 
                             value = TRUE, status = "warning"),
             style = "margin-bottom:-22px;"
      ),
      
      ## FFH directive
      column(11,
             br(),
             h5(HTML("&emsp;"), "only butterflies with protection status"),
             style = "margin-bottom:-25px;"
      ),
      column(11,
             selectInput("ffh", h5("FFH directive"),
                         choices = list("Annex II species" = "II","Annex IV species" = "IV"), 
                         multiple = TRUE, selected = ""),
             style = "margin-bottom:-25px;"
      ),
      
      ## Red list Bavaria
      column(11,
             selectInput("redlist", h5("Red List Bavaria"), 
                         choices = list( "0: extinct" = 0, "1: threatened by extinction" = 1, "2: critically endangered" = 2,
                                         "3: endangered" = 3, "G: danger of unknown extent" = "G", "R: extremely rare" = "R",
                                         "V: prewarn list" = "V", "＊: not endangered" = "＊", "D: data deficient" = "D",
                                         "♦: not evaluated" = "♦", "-: not included" = "-"), 
                         multiple = TRUE, selected = ""),
             style = "margin-bottom:-20px;"
      ),
      
      ## Digitizer notes
      column(11,
             awesomeCheckbox("digitizer","notes of the digitizer on/off",
                             value = FALSE, status = "warning"),
             style = "margin-bottom:-20px;"
      ),
      
      ## Run button
      column(11,
             br(),
             div(style="position: fixed; bottom: 20px; width: 220px; z-index: 1000;",
                 actionBttn(
                   inputId = "run_button",
                   label = h5("search with these inputs"),
                   color = "warning",
                   style = "bordered"
                 )
             )
      ),
      
      ## Footer
      column(12,
             h6(HTML("&nbsp;"), "collection of Arthur Bott"),
             h6(HTML("&nbsp;"), "© Weber Felix, Johannes Balkenhol"),
             style = "margin-top: 20px; margin-bottom: 50px;"
      )
    )
  )
)

sidebar  # Needed to return the object from source()
