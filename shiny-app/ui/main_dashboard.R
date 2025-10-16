main_dashboard <- fluidPage(
  tags$head(
    tags$style(HTML("
      img.small-img { max-width: 75px; }
      .shiny-output-error, .shiny-output-error:before { visibility: hidden; }
      .wrapper { height: 100%; position: relative; overflow-x: hidden; overflow-y: hidden; }
    "))
  ),
  
  headerPanel(""),
  
  ## family tree
  fluidRow(
    box(
      title = "Family Tree", status = "warning", collapsible = TRUE, collapsed = TRUE, width = 12,
      style = "max-height: 800px; overflow-y: scroll;",
      column(12, withSpinner(
        plotOutput("o_tree", width = "100%", height = "100%"),
        type = 6, color = "#cf9206", size = 2, proxy.height = "300px"
      ))
    )
  ),
  
  ## data table
  fluidRow(
    box(
      title = "Specimen Table", status = "warning", collapsible = TRUE, width = 12,
      downloadButton("download_csv", "Download Table (CSV)", class = "btn-warning"),
      downloadButton("download_abcd", "Download ABCD/DwC (XML)", class = "btn-warning"),
      withSpinner(
        dataTableOutput("o_table", width = "100%", height = "auto"),
        type = 6, color = "#cf9206", size = 2
      ),
      h6(strong("Legend:"), br(),
         HTML("&emsp;"), "ssp. = subspecies", HTML("&emsp;"), "f. = form", HTML("&emsp;"), "abb. = abberation", 
         HTML("&emsp;"),"( ) = synonym", HTML("&emsp;"), "cf. = needs confirmation", HTML("&emsp;"), "Umg. = Umgebung (area)", br(),
         HTML("&emsp;"), "e.o. = ex ovum", HTML("&emsp;"), "e.l. = ex larva", HTML("&emsp;"), "e.p. = ex puppa",
         HTML("&emsp;"), "Coll. = collector", HTML("&emsp;"), "Leg. = legit collector", br(),
         HTML("&emsp;"), "0 = extinct", HTML("&emsp;"), "1 = threatened by extinction", HTML("&emsp;"), "2 = critically endangered", 
         HTML("&emsp;"), "3 = endangered", HTML("&emsp;"), "G = danger of unknown extent", br(), 
         HTML("&emsp;"), "R = extremely rare", HTML("&emsp;"), "V = prewarn list", HTML("&emsp;"), "＊ = not endangered",
         HTML("&emsp;"), "D = data deficient", HTML("&emsp;"), "♦ = not evaluated", HTML("&emsp;"), "- = not included")
    )
  ),
  
  ## geographic map
  fluidRow(
    box(
      title = "Geographic Map", status = "warning", collapsible = TRUE, collapsed = TRUE, width = 12,
      h5("For further information click on the cluster marker and the butterfly icons. (Requires active internet connection.)"),
      tags$head(tags$style(HTML("
        .marker-custom-small { background-color: rgba(181, 226, 140, 1); }
        .marker-custom-small div { background-color: rgba(110, 204, 57, 1); }
        .marker-custom-medium { background-color: rgba(241, 211, 87, 1); }
        .marker-custom-medium div { background-color: rgba(240, 194, 12, 1); }
        .marker-custom-large { background-color: rgba(253, 156, 115, 1); }
        .marker-custom-large div { background-color: rgba(241, 128, 23, 1); }
      "))),
      withSpinner(
        leafletOutput("o_map", width = "100%", height = "800px"),
        type = 6, color = "#cf9206", size = 2, proxy.height = "300px"
      ),
      h6("Icons made by Freepik and AmethystDesign from www.flaticon.com")
    )
  ),
  
  ## color brightness
  fluidRow(
    box(
      title = "Color Brightness vs Elevation", status = "warning", collapsible = TRUE, collapsed = TRUE, width = 12,
      column(6,
             sliderInput("color_height", h5("Elevation of the location (m)"),
                         min = 0, max = 3000, value = c(0, 3000))
      ),
      column(6,
             sliderInput("color", h5("Color Brightness"),
                         min = 50, max = 85, value = c(50, 85))
      ),
      column(11,
             selectInput("color_butterfly_name", h5("Butterfly Species"), 
                         choices = unique(db_color$genus_and_species),
                         multiple = TRUE, selected = ""),
             style = "margin-bottom:-20px;"
      ),
      column(12,
             withSpinner(
               plotlyOutput("o_color", width = "100%", height = "500px"),
               type = 6, color = "#cf9206", size = 2, proxy.height = "300px"
             )
      ),
      column(12, h5(strong("Correlation:"), br())),
      column(12,
             textOutput("o_correlation"),
             style = "margin-top:-10px; margin-bottom:10px;"
      ),
      column(12,
             actionBttn(
               inputId = "color_button",
               label = h5("Plot Color Brightness"),
               color = "warning",
               style = "bordered"
             )
      )
    )
  ),
  
  ## selected filters
  fluidRow(
    box(
      title = "Selected Filters", status = "warning", collapsible = TRUE, width = 12,
      textOutput("o_suborder"),
      textOutput("o_family"),
      textOutput("o_genus"),
      textOutput("o_species"),
      textOutput("o_german_name"),
      textOutput("o_land"),
      textOutput("o_elevation"),
      textOutput("o_date"),
      textOutput("o_total_number"),
      textOutput("o_pictures"),
      textOutput("o_ffh"),
      textOutput("o_redlist"),
      textOutput("o_digitizer"),
      h5("If you encounter any Bugs/Errors or want to give feedback, please contact me at felix1997weber2@gmail.com.")
    )
  )
)
