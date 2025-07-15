################################################################################
## Butterflies Database

## Digitizing of a collection in form of an excel sheet and filtering and displaying the data with R and a R Shiny interface. 
## With 13 different filter options in the sidebar.
## A family tree, data table and geographical map generated after filter selection. 
## A filterable graph plot of the color brightness of the butterflies in relation to the altitude of their location.
## Programming languages used: R (Shiny), HTML, Java Script, CSS.

## Collector: Arthur Bott
## Digitization: Dr. Mirko Wölfling and Felix Weber
## Program: Felix Weber

## Copyright © 2023, Felix Weber
## Released under the MIT License
## # out commented programming line
## ## comment
################################################################################


################################################################################
## load library 
################################################################################

library(shiny)            ## Shiny interface 
library(shinydashboard)   ## dashboard for Shiny (sidebar, main dashboard)
library(shinyWidgets)     ## widgets for Shiny
library(shinyjs)          ## JavaScript in Shiny
library(shinycssloaders)  ## loading animation
library(shinyalert)       ## alert popup
library(readxl)           ## read the excel
library(DT)               ## render tables
library(data.table)       ## convert dataframe to datatable
library(dplyr)            ## work with tables
library(tidyr)            ## work with tables
library(leaflet)          ## geographic map
library(igraph)           ## plot family tree
library(ggraph)           ## render family tree
library(plotly)           ## color brightness diagram + loading animation


## read in the excel tables ####################################################

## read in database excel
db <- read_excel("../data/butterfly_database.xlsx", col_types = c("text","text","text","text","text","text","text","text", "text", "text", "date", "text", "numeric", "text","text","text","text","text"))

## format Date 
db$D_M_Y <- as.Date(db$D_M_Y, format=  "%Y/%m/%d")

## create merged genus and species column for pictures
db <- mutate(db, pictures = paste(genus, species))

## create db_genus/species/german_name/land for selectInput
db_genus <- unique(db$genus[order(db$genus)])
db_species <- unique(db$species[order(db$species)])
db_german_name <- unique(db$german_name[order(db$german_name)])
db_land <- unique(db$land[order(db$land)])

## read in the color brightness excel table
db_color <- read_excel("../data/butterfly_color_brightness.xlsx")
db_color <- db_color [,c(1,3,4,5),]



################################################################################
## user interface
################################################################################

ui <- dashboardPage(skin = "yellow", 
                    dashboardHeader(
                      title = "Butterfly Collection",
                      titleWidth = 208),
                    

## sidebar #####################################################################

                    dashboardSidebar(
                      tags$head(includeScript("www/submit_via_enter.js")),
                      width = 250, 
                      sidebarMenu(
                        
                        fluidRow(useShinyjs(),
                                 chooseSliderSkin("Flat", color = "orange"),    
                          column(11, 
                                  h5(HTML("&emsp;"), "suborder (select at least one)"),
                                  style = "margin-bottom:-20px;",
                          ),
                          column(11, 
                                  awesomeCheckbox("day", "Rhopalocera on/off",
                                                  value = TRUE, status = "warning"),
                                  style = "margin-bottom:-25px;",
                          ),
                          column(11,
                                  awesomeCheckbox("night", "Heterocera on/off", 
                                                  value = TRUE, status = "warning"),
                                  style = "margin-bottom:-20px;",
                          ),
                          conditionalPanel(condition="input.day == 1 && input.night == 1",
                                          column(11, 
                                                  tags$style("
                                                            #family ~ .selectize-control .selectize-input {
                                                              max-height: 80px; overflow-y: auto; 
                                                              }
                                                            "),
                                                  selectInput("family", h5("family"), 
                                                              choices = list("Hesperiidae", "Lycaenidae", "Nymphalidae", "Papilionidae", "Pieridae", "Riodinidae",
                                                                             "Brahmaeidae", "Drepanidae", "Endromidae", "Erebidae", "Euteliidae", "Geometridae",
                                                                             "Lasiocampidae","Noctuidae", "Nolidae", "Notodontidae", "Saturniidae", "Sphingidae",
                                                                             "Cossidae", "Hepialidae", "Limacodidae"), 
                                                              multiple = TRUE, selected = ""),
                                                  style = "margin-bottom:-25px;",
                                          ),
                          ),
                          conditionalPanel(condition="input.day == 1 && input.night == 0",
                                          column(11,
                                                  tags$style("
                                                            #family_day ~ .selectize-control .selectize-input {
                                                              max-height: 80px; overflow-y: auto; 
                                                              }
                                                            "),
                                                  selectInput("family_day", h5("family"), 
                                                              choices = list("Hesperiidae", "Lycaenidae", "Nymphalidae", "Papilionidae", "Pieridae", "Riodinidae"), 
                                                              multiple = TRUE, selected = ""),
                                                  style = "margin-bottom:-25px;",
                                          )
                          ),
                          conditionalPanel(condition="input.day == 0 && input.night == 1",
                                          column(11,
                                                  tags$style("
                                                            #family_night ~ .selectize-control .selectize-input {
                                                              max-height: 80px; overflow-y: auto; 
                                                              }
                                                            "),     
                                                  selectInput("family_night", h5("family"), 
                                                              choices = list("Brahmaeidae", "Drepanidae", "Endromidae", "Erebidae","Euteliidae", "Geometridae",
                                                                             "Lasiocampidae","Noctuidae", "Nolidae", "Notodontidae", "Saturniidae", "Sphingidae",
                                                                             "Cossidae", "Hepialidae", "Limacodidae"), 
                                                              multiple = TRUE, selected = ""),
                                                  style = "margin-bottom:-25px;",
                                          )
                          ),   
                          conditionalPanel(condition="input.day == 0 && input.night == 0",
                                           column(11,
                                                  selectInput("empty", h5("family"), 
                                                              choices = list("no suborder selected"), 
                                                              multiple = TRUE, selected = ""),
                                                  style = "margin-bottom:-25px;",
                                           )
                          ),
                          column(11,
                                  tags$style("
                                            #genus ~ .selectize-control .selectize-input {
                                              max-height: 80px; overflow-y: auto; 
                                              }
                                            "),
                                  selectInput("genus", h5("genus"), 
                                              db_genus, 
                                              multiple = TRUE, selected = ""),
                                  style = "margin-bottom:-20px;",
                          ),
                          column(11,
                                  tags$style("
                                            #species ~ .selectize-control .selectize-input {
                                              max-height: 80px; overflow-y: auto; 
                                              }
                                            "),
                                  selectInput("species", h5("species"), 
                                              db_species, 
                                              multiple = TRUE, selected = ""),
                                  style = "margin-bottom:-20px;",
                          ), 
                          column(11,
                                  tags$style("
                                            #german_name ~ .selectize-control .selectize-input {
                                              max-height: 80px; overflow-y: auto; 
                                              }
                                            "),     
                                  selectInput("german_name", h5("german name"), 
                                              db_german_name, 
                                              multiple = TRUE, selected = ""),
                                  style = "margin-bottom:-20px;",
                          ),
                          column(11,
                                  tags$style("
                                        #land ~ .selectize-control .selectize-input {
                                          max-height: 80px; overflow-y: auto; 
                                          }
                                        "),
                                  selectInput("land", h5("land"), 
                                              db_land, 
                                              multiple = TRUE, selected = ""),
                                  style = "margin-bottom:-20px;",
                          ),
                          column(11,
                                  sliderInput("elevation", h5 ("elevation of the location", br(), ("(0 ≙ no data available)")), 
                                              min = 0 , max = 3000, value = c(0,3000)),
                                  style = "margin-bottom:-20px;",
                          ),
                          column(11,
                                  dateRangeInput("date", h5("date") ,start = "1900-01-01", end = "2010-01-01", min = "1900-01-01", max = "2010-01-01", format = "yyyy-mm-dd", startview = "decade", language = "en", separator = "to"),
                                  style = "margin-bottom:-20px;",  
                          ),
                          column(11,
                                  sliderInput("total_number", h5 ("total number of a species"),
                                              min = 0, max = 107, value = c(1,107)),
                                  style = "margin-bottom:-20px;",
                          ),
                          column(11,
                                  awesomeCheckbox("pictures", "pictures on/off", 
                                                  value = TRUE, status = "warning"),
                                  style = "margin-bottom:-22px;",
                          ),
                          column(11,
                                  br(),
                                  h5(HTML("&emsp;"), "only butterflies with protection status"),
                                  style = "margin-bottom:-25px;",
                          ),
                          column(11,
                                  selectInput("ffh", h5("FFH directive"),
                                              choices = list("Annex II species" = "II","Annex IV species" = "IV"), 
                                              multiple = TRUE, selected = ""),
                                  style = "margin-bottom:-25px;",
                          ),
                          column(11,
                                  tags$style("
                                        #redlist ~ .selectize-control .selectize-input {
                                          max-height: 80px; overflow-y: auto; 
                                          }
                                        "),
                                  selectInput("redlist", h5("Red List Bavaria"), 
                                              choices = list( "0: extinct" = 0, "1: threatened by extinction" = 1, "2: critically endangered" = 2,
                                                              "3: endangered" = 3, "G: danger of unknown extent" = "G", "R: extremely rare" = "R",
                                                              "V: prewarn list" = "V", "＊: not endangered" = "＊", "D:	data deficient" = "D",      ##＊= FULLWIDTH ASTERISK!
                                                              "♦: not evaluated" = "♦", "-: not included" = "-"), 
                                              multiple = TRUE, selected = ""),
                                  style = "margin-bottom:-20px;",
                          ),
                          column(11,
                                  awesomeCheckbox("digitizer","notes of the digitizer on/off",
                                                  value = FALSE, status = "warning"),
                                  style = "margin-bottom:-20px;", 
                          ),
                          column(11,
                                  br(),
                                  actionBttn(
                                    inputId = "run_button",
                                    label = h5("search with these inputs"),
                                    color = "warning",
                                    style = "bordered"
                                  ),
                                  style = "margin-bottom:+20px;",
                          ),
                          column(11,
                                  h6(HTML("&nbsp;"),"collection of Arthur Bott"),
                                  h6(HTML("&nbsp;"),"© Weber Felix"),
                                  style = "margin-bottom:+300px;",
                          )
                        )
                      )
                    ),


## main dashboard ##############################################################

                    dashboardBody(
                      tags$style( HTML(
                                      "img.small-img {
                                              max-width: 75px;
                                      }"
                                  ),
                                type="text/css",
                                ".shiny-output-error { visibility: hidden; }",
                                ".shiny-output-error:before { visibility: hidden; }",
                                ".wrapper{height:100%;position:relative;overflow-x:hidden;overflow-y:hidden}"
                      ),
                      headerPanel(""),
                      tags$head(
                        tags$style(HTML("/* body */
                                  .content-wrapper, .right-side {
                                    background-color: #ffffff;
                                  }"
                        ))
                      ), 
                     
## family tree                     
                      fluidRow(
                        box(
                          title = "family tree" , status="warning",collapsible = TRUE, collapsed = TRUE, width = 12,
                          style= "max-height: 800px; overflow-y: scroll;",
                          
                          column(12,  
                                 withSpinner(plotOutput("o_tree", width = "100%", height = "100%"),
                                             type = 6, color = "#cf9206", size = 2, proxy.height = "300px")
                          ),
                        )
                      ),
                      
## table                      
                      fluidRow(
                        box(
                          title = "table", status="warning", collapsible = TRUE, width = 12,

                          withSpinner(dataTableOutput("o_table", width = "100%", height = "auto"),
                                      type = 6, color = "#cf9206", size = 2),
                          
                          h6(strong("legend:"), br(),
                             HTML("&emsp;"), "ssp. = subspecies", HTML("&emsp;"), "f. = form", HTML("&emsp;"), "abb. = abberation", 
                             HTML("&emsp;"),"( ) = synonym", HTML("&emsp;"), "cf. = needs confirmation", HTML("&emsp;"), "Umg. = Umgebung (area)", br(),
                             
                             HTML("&emsp;"), "e.o. = ex ovum", HTML("&emsp;"), "e.l. = ex larva", HTML("&emsp;"), "e.p. = ex puppa",
                             HTML("&emsp;"), "Coll. = collector", HTML("&emsp;"), "Leg. = legit collector", br(),
                             
                             HTML("&emsp;"), "0 = extinct", HTML("&emsp;"), "1 = threatened by extinction", HTML("&emsp;"), "2 = critically endangered", 
                             HTML("&emsp;"), "3 = endangered", HTML("&emsp;"), "G = danger of unknown extent", br(), 
                             HTML("&emsp;"), "R = extremely rare", HTML("&emsp;"), "V = prewarn list", HTML("&emsp;"), "＊= not endangered",
                             HTML("&emsp;"), "D =	data deficient", HTML("&emsp;"), "♦ = not evaluated", HTML("&emsp;"), "- = not included"
                            ), 
                        )
                      ),
                      
## geographic map                     
                      fluidRow(
                        box(
                          title = "geographic map", status="warning", collapsible = TRUE, collapsed = TRUE, width = 12,
                          
                          h5 ("For further information click on the cluster marker and the butterfly icons.(To show the map you need an active internet connection.)"),
                          
                          tags$head(tags$style(HTML("
                            .marker-custom-small { 
                             background-color: rgba(181, 226, 140, 1);
                            }
                            .marker-custom-small div { 
                              background-color: rgba(110, 204, 57, 1);
                            }
                            .marker-custom-medium {
                              background-color: rgba(241, 211, 87, 1);
                            }
                            .marker-custom-medium div {
                              background-color: rgba(240, 194, 12, 1);
                            }
                            .marker-custom-large {
                              background-color: rgba(253, 156, 115, 1);
                            }
                            .marker-custom-large div {
                              background-color: rgba(241, 128, 23, 1);
                            }
                          "))),
                          
                          withSpinner(leafletOutput("o_map", width = "100%", height = "800px"),
                                      type = 6, color = "#cf9206", size = 2, proxy.height = "300px"),
                          
                          h6 ("Icons made by Freepik and AmethystDesign from www.flaticon.com")
                        )
                      ),
   
## color brightness
                      fluidRow(
                        box(
                          title = "color brightness of butterflies in corellation to their elevation", status="warning", collapsible = TRUE, collapsed = TRUE, width = 12, height = "100%",
                          
                          column(6,
                                 sliderInput("color_height", h5 ("elevation of the location in meter"),
                                             min = 0, max = 3000, value = c(0,3000)),
                          ),
                          column(6,
                                 sliderInput("color", h5 ("color brightness"),
                                             min = 50, max = 85, value = c(50,85))
                          ),
                          column(11,
                                 selectInput("color_butterfly_name", h5("butterfly species"), 
                                             db_color$genus_and_species, 
                                             multiple = TRUE, selected = ""),
                                 style = "margin-bottom:-20px;",
                          ),
                          column(12,  
                                 withSpinner(plotlyOutput("o_color", width = "100%", height = "500px"),
                                             type = 6, color = "#cf9206", size = 2, proxy.height = "300px"),
                          ),
                          column(12, 
                                 h5(strong("Correlation:"), br()),
                          ),
                          column(12, 
                                 textOutput("o_correlation"),
                                 style = "margin-top:-10px; margin-bottom:10px",
                          ),
                          column(12,
                                 actionBttn(
                                   inputId = "color_button",
                                   label = h5("plot color brightness"),
                                   color = "warning",
                                   style = "bordered"
                                 ), 
                          )
                        )
                      ),

## selected filters
                      fluidRow(
                        box(
                          title = "selected filters", status="warning", collapsible = TRUE,width = 12,
                          
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
                          
                          
                          h5("If you encounter any Bugs/Errors, or want to give Feedback, please contact me at felix1997weber2@gmail.com."),
                        )
                      ),   

                    ),

)



################################################################################
## server logic
################################################################################

server <- function(input, output) {
  
## print . to console every 10s to prevent grey out on server  
  autoInvalidate <- reactiveTimer(10000)
  observe({
    autoInvalidate()
    cat(".")
  })
  
## stop loading animation before pressing run button
  output$o_tree <- renderDT(NULL)
  output$o_table <- renderDT(NULL)
  output$o_map <- renderLeaflet({
    leaflet() %>% 
      addTiles()
  })
  output$o_color <- renderDT(NULL)
  
  
## run button
  observeEvent(input$run_button,{
    disable("run_button")

    
## filter database #############################################################
    
    ## switch off filter for testing stuff    
    #if(1 == 0){ 
    
    originalData = reactiveVal(db)
    
    df_selected_filter <-  reactive({

      res <-  originalData()
      
      if (input$day == TRUE & input$night == TRUE){
          if (is.null(input$family)){
            res <-  originalData()
          }
          else {
            res <-  originalData() %>% filter(family %in% trimws(input$family))
          }
          res <- res %>% filter(suborder == "Rhopalocera" | suborder == "Heterocera")
      } 
      if (input$day == TRUE & input$night == FALSE){
          if (is.null(input$family_day)){
            res <-  originalData()
          }
          else {
            res <-  originalData() %>% filter(family %in% trimws(input$family_day))
          }
          res <- res %>% filter(suborder == "Rhopalocera")
      } 
      if (input$day == FALSE & input$night == TRUE){
          if (is.null(input$family_night)){
            res <-  originalData()
          }
          else {
            res <-  originalData() %>% filter(family %in% trimws(input$family_night))
          }
          res <- res %>% filter(suborder == "Heterocera")
      } 
      if (input$day == FALSE & input$night == FALSE){
          res <- res %>% filter(suborder == "soll nichts finden")
      } 
      if (!is.null(input$genus)){
          res <- res %>% filter(genus %in% trimws(input$genus))
      }
      if (!is.null(input$species)){
          res <- res %>% filter(species %in% trimws(input$species))
      }
      if (!is.null(input$german_name)){
          res <- res %>% filter(german_name %in% trimws(input$german_name))
      }
      if (!is.null(input$land)){
          res <- res %>% filter(land %in% input$land) 
      }
      if (input$elevation[1] == 0){
          res <- res %>% filter(between(as.numeric(elevation),as.numeric(input$elevation[1]),as.numeric(input$elevation[2])) | is.na(elevation) )
      }
      if (input$elevation[1] > 0){
          res <- res %>% filter(between(as.numeric(elevation),as.numeric(input$elevation[1]),as.numeric(input$elevation[2])))
      }
      if(length(input$date[1]) > 0){
          res <- res %>% filter(between(D_M_Y, as.Date(input$date[1]), as.Date(input$date[2])))
      }
      if (input$total_number[1] >= 0){
          res <- res %>% filter(between(as.numeric(total_number_of_this_species),as.numeric(input$total_number[1]),as.numeric(input$total_number[2])))  
      }
      if (!is.null(input$ffh)){
          res <- res %>% filter(FFH_directive %in% input$ffh | FFH_directive %like% input$ffh)
      }
      if (!is.null(input$redlist)){
          res <- res %>% filter(red_list_Bavaria %in% input$redlist)
      }
      res
      
    })

    if (nrow(df_selected_filter()) < 1){
      shinyalert( "No matching data found. Please enter different parameters.", type = "warning",
                  time = 90000)
    }
    
    ## switch off filter for testing stuff 
    #}  
    
## convert dataframe to datatable
    db_table <- setDT(df_selected_filter())  
    db_table$D_M_Y <- as.Date(db_table$D_M_Y, format=  "%Y/%m/%d")
    #db_table$D_M_Y <- format(db_table$D_M_Y, "%d-%m-%Y") ## working but arrows filter inside db_table isn't working with this
    

## family tree #################################################################
          
## edgelist
    edgelist <- db_table[,c(1)]
    
      edgelist <- edgelist %>% rename(to = suborder)
      edgelist <- edgelist %>% mutate(from = "Lepidoptera", .before = to)
   
      edgelist_1 <- db_table[,c(1,2)]
      edgelist_1 <- edgelist_1 %>% rename(from = suborder, to = family)
      
      edgelist_2 <- db_table[,c(2,3)]
      edgelist_2 <- edgelist_2 %>% rename(from = family, to = genus)
      
      edgelist_3 <- db_table[,c(3,5)]
      edgelist_3 <- edgelist_3 %>% rename(from = genus, to = genus_and_species)
      
      edgelist <- edgelist %>% bind_rows(edgelist_1, edgelist_2, edgelist_3)
      
      edgelist <- edgelist %>% group_by(from,to) %>% slice(1)
    
## nodelist
    nodelist <- data.frame (list(labelnames = "Lepidoptera"))
    
      nodelist_1 <- db_table [,c(1)] 
      nodelist_1 <- nodelist_1 %>% rename(labelnames = suborder)
      nodelist_1 <- unique(nodelist_1)
      
      nodelist_2 <- db_table [,c(2)]
      nodelist_2 <- nodelist_2 %>% rename(labelnames = family) 
      nodelist_2 <- unique(nodelist_2)
      
      nodelist_3 <- db_table [,c(3)]
      nodelist_3 <- nodelist_3 %>% rename(labelnames = genus)
      nodelist_3 <- unique(nodelist_3)
      
      nodelist_4 <- db_table [,c(5)]
      nodelist_4 <- nodelist_4 %>% rename(labelnames = genus_and_species)
      nodelist_4 <- unique(nodelist_4)
      
      nodelist <- nodelist %>% bind_rows(nodelist_1, nodelist_2, nodelist_3, nodelist_4)
      
  ## create list for labels because of duplicate species names
      labellist <- data.frame (list(labelnames = "Lepidoptera"))
      species_label <- nodelist_4 %>% separate(labelnames, c("genus", "labelnames"), " ")
      species_label <- as.data.frame(species_label[,c(2)])
      colnames(species_label) <- c("labelnames")

      labellist <- labellist %>% bind_rows(nodelist_1, nodelist_2, nodelist_3, species_label)

## treeplot
  tree <- graph_from_data_frame(d=edgelist,vertices=nodelist)

  ## variable plotheight 
  plot_height = reactive(
                    if (nrow(nodelist) < 30){
                      return(150+10*(nrow(nodelist)))
                    }
                    else {
                      return(10*(nrow(nodelist)))
                    }
                )
  
  ## render tree  
  output$o_tree <- renderPlot(height = plot_height,

    ggraph(tree, "dendrogram") +    #, circular = TRUE ## circle-diagram, but too much data
      coord_flip() + 
      scale_y_reverse() +
      scale_x_reverse() +
      
      geom_edge_diagonal(show.legend = FALSE, colour = "black", edge_width = 1)+ 
      ## alternatives: geom_edge_: diagonal = smooth lines, elbow = 90° corners , bend = 90° bends , fan = straight lines,  
      
      geom_node_label(aes(label = labellist$labelnames), show.legend = FALSE, repel = FALSE, 
                      colour = "black", linewidth = 2, fill = "orange",label.padding = unit(0.15, "lines")),
  )
    
  
## pictures  ###################################################################

## change colnames 
  db_table_colnames <- db_table
  colnames(db_table_colnames)[colnames(db_table_colnames) %in% 
          c("german_name", "D_M_Y", "date_accuracy", "total_number_of_this_species", "FFH_directive", "red_list_Bavaria", "notes_of_the_digitizer")] <- 
          c("german name", "date",  "date accuracy", "total number of this species", "FFH directive", "red list Bavaria", "notes of the digitizer")
  
## table with pictures and without notes
  if (input$pictures == TRUE & input$digitizer == FALSE){
    if (nrow(df_selected_filter()) > 0){
      
      my_image_df <- as.data.frame(db_table_colnames)

      #image <- tibble()
      
      image <- my_image_df %>%
        select( "pictures", "suborder", "family", "genus", "species", "german name", "location",
                "land", "elevation", "date" , "date accuracy" , "total number of this species",
                "FFH directive", "red list Bavaria", "breeding", "collector")

      df_table_pictures <- image %>%
        mutate(
          pictures = paste0(
            "<img class = small-img src=\"",
            "images/",
            pictures,
            ".png",
            "\" height=\"30\" 
            data-toggle=\"tooltip\" data-placement=\"right\" title=\"",   ## add tooltip
            "\"></img>"
          )
        ) 
    }
  }
  
## table with pictures and notes
  if (input$pictures == TRUE & input$digitizer == TRUE){
    if (nrow(df_selected_filter()) > 0){
      
      my_image_df <- as.data.frame(db_table_colnames)
    
      #image <- tibble()

        image <- my_image_df %>%
          select( "pictures", "suborder", "family", "genus", "species", "german name", "location",
                  "land", "elevation", "date" , "date accuracy" , "total number of this species",
                  "FFH directive", "red list Bavaria", "breeding", "collector", "notes of the digitizer",)
      
      df_table_pictures <- image %>%
        mutate(
          pictures = paste0(
            "<img class = small-img src=\"",
            "images/",
            pictures,
            ".png",
            "\" height=\"30\" 
            data-toggle=\"tooltip\" data-placement=\"right\" title=\"",  
            "\"></img>"
          )
        ) 
    }
  }  
  
  
## table #######################################################################

## table with pictures
    output$o_table <-    
      renderDataTable(server=FALSE,{
        df_table_pictures
        },
        extensions = list("Scroller"),
        options = list(
          scrollX = TRUE,
          FixedHeader = TRUE),
        escape = FALSE,
        callback=JS(
          'table.on("mouseover","tr", function() {
              $(".small-img").hover(function(){
                    $(this).css("transform", "scale(13, 13)" 
                    );
                    $(this).css("transform-origin", "top left"
                    );
                    $(this).css("position", "relative"
                    );
                    $(this).css("top", "0px"
                    );
                    $(this).css("left", "0px"
                    );
                    $(this).css("z-index", "1"
                    );
      
              }, function(){
                    $(this).css("transform", "none"
                    );
                     $(this).css("position", "relative"
                    );
                    $(this).css("top", "0px"
                    );
                    $(this).css("left", "0px"
                    );
                    $(this).css("z-index", "0"
                    );
              });
          })'    
        )
      )
  
## tables without pictures  
  ## table without notes
  if (input$pictures == FALSE & input$digitizer==FALSE){
    
    output$o_table <-
      renderDataTable(server=FALSE,{
        datatable(db_table[,c(1:4,6:8,10:17)], colnames = c("suborder", "family", "genus", "species", "german name", "location",
                                                            "land", "elevation", "date ", "date accuracy" , "total number of this species",
                                                            "FFH directive", "Red List Bavaria", "breeding", "collector"),
                      options = list(scrollX = TRUE), 
                      extensions = list(FixedHeader = TRUE))
      }
      )
  }
  
  ## table with notes
  if (input$pictures == FALSE & input$digitizer==TRUE){
    
    output$o_table <-
      renderDataTable(server=FALSE,{
          datatable(db_table[,c(1:4,6:8,10:17,18)], colnames = c("suborder", "family", "genus", "species", "german name", "location",
                                                                 "land", "elevation", "date ", "date accuracy" , "total number of this species",
                                                                 "FFH directive", "Red List Bavaria", "breeding", "collector", "notes of the digitizers"),
                        options = list(scrollX = TRUE),
                        extensions = list(FixedHeader = TRUE))
      }
    )  
  }

  
## geographic map ##############################################################
  
## mutate GPS data
  db_map <- db_table %>% mutate(GPS_data = gsub("-", "x", GPS_data))
  db_map <- db_map %>% mutate(GPS_data = gsub("[.]", "y", GPS_data, ))
  
  db_map <- db_map %>% separate(GPS_data, c("latitude", "longitude"), ", ")
  
  db_map <- db_map %>% mutate(longitude = gsub("y", ".", longitude))
  db_map <- db_map %>% mutate(longitude = gsub("x", "-", longitude))
  db_map <- db_map %>% mutate(latitude = gsub("y", ".", latitude))
  db_map <- db_map %>% mutate(latitude = gsub("x", "-", latitude))
  
  db_map$longitude <- as.numeric(as.character(db_map$longitude))
  db_map$latitude <- as.numeric(as.character(db_map$latitude))

## geographic map
  ## icons
  butterflyicons <- iconList(
                          ## Rhopalocera
                            Hesperiidae = makeIcon("www/mapicons/hesperiidae_icon.png", iconWidth = 35, iconHeight = 35), 
                            Lycaenidae = makeIcon("www/mapicons/lycaenidae_icon.png", iconWidth = 35, iconHeight = 35),
                            Nymphalidae = makeIcon("www/mapicons/nymphalidae_icon.png", iconWidth = 35, iconHeight = 35),
                            Papilionidae = makeIcon("www/mapicons/papilionidae_icon.png", iconWidth = 35, iconHeight = 35),
                            Pieridae = makeIcon("www/mapicons/pieridae_icon.png", iconWidth = 35, iconHeight = 35),
                            Riodinidae = makeIcon("www/mapicons/riodinidae_icon.png", iconWidth = 35, iconHeight = 35),
                            
                          ## Heterocera
                            Brahmaeidae = makeIcon("www/mapicons/brahmaeidae_icon.png", iconWidth = 35, iconHeight = 35),
                            Cossidae = makeIcon("www/mapicons/cossidae_icon.png", iconWidth = 35, iconHeight = 35),
                            Drepanidae = makeIcon("www/mapicons/drepanidae_icon.png", iconWidth = 35, iconHeight = 35),
                            Endromidae = makeIcon("www/mapicons/endromidae_icon.png", iconWidth = 35, iconHeight = 35),
                            Erebidae = makeIcon("www/mapicons/erebidae_icon.png", iconWidth = 35, iconHeight = 35),
                            Euteliidae = makeIcon("www/mapicons/euteliidae_icon.png", iconWidth = 35, iconHeight = 35),
                            Geometridae = makeIcon("www/mapicons/geometridae_icon.png", iconWidth = 35, iconHeight = 35),
                            Hepialidae = makeIcon("www/mapicons/hepialidae_icon.png", iconWidth = 35, iconHeight = 35),
                            Lasiocampidae = makeIcon("www/mapicons/lasiocampidae_icon.png", iconWidth = 35, iconHeight = 35),
                            Limacodidae = makeIcon("www/mapicons/limacodidae_icon.png", iconWidth = 35, iconHeight = 35),
                            Noctuidae = makeIcon("www/mapicons/noctuidae_icon.png", iconWidth = 35, iconHeight = 35),
                            Nolidae = makeIcon("www/mapicons/nolidae_icon.png", iconWidth = 35, iconHeight = 35),
                            Notodontidae = makeIcon("www/mapicons/notodontidae_icon.png", iconWidth = 35, iconHeight = 35),
                            Saturniidae = makeIcon("www/mapicons/saturniidae_icon.png", iconWidth = 35, iconHeight = 35),
                            Sphingidae = makeIcon("www/mapicons/sphingidae_icon.png", iconWidth = 35, iconHeight = 35)
  )
  
  ## html legend
  ## Rhopalocera and Heterocera
  if (input$day == TRUE & input$night == TRUE){
    html_legend <-  "<b>Rhopalocera</b><br/>
                    <img src='/mapicons/hesperiidae_icon.png'
                    style='width:25px;height:25px;'>Hesperiidae<br/> 
                    <img src='/mapicons/lycaenidae_icon.png'
                    style='width:25px;height:25px;'>Lycaenidae<br/>
                    <img src='/mapicons/nymphalidae_icon.png' 
                    style='width:25px;height:25px;'>Nymphalidae<br/>
                    <img src='/mapicons/papilionidae_icon.png'
                    style='width:25px;height:25px;'>Papilionidae<br/>
                    <img src='/mapicons/pieridae_icon.png'
                    style='width:25px;height:25px;'>Pieridae<br/>
                    <img src='/mapicons/riodinidae_icon.png'   
                    style='width:25px;height:25px;'>Riodinidae<br/>
                    
                    <b>Heterocera</b><br/>
                    <img src='/mapicons/brahmaeidae_icon.png' 
                    style='width:25px;height:25px;'>Brahmaeidae<br/>
                    <img src='/mapicons/cossidae_icon.png' 
                    style='width:25px;height:25px;'>Cossidae<br/>
                    <img src='/mapicons/drepanidae_icon.png'
                    style='width:25px;height:25px;'>Drepanidae<br/>
                    <img src='/mapicons/endromidae_icon.png' 
                    style='width:25px;height:25px;'>Endromidae<br/>
                    <img src='/mapicons/erebidae_icon.png' 
                    style='width:25px;height:25px;'>Erebidae<br/>
                    <img src='/mapicons/geometridae_icon.png'
                    style='width:25px;height:25px;'>Geometridae<br/>
                    <img src='/mapicons/hepialidae_icon.png'  
                    style='width:25px;height:25px;'>Hepialidae<br/>
                    <img src='/mapicons/euteliidae_icon.png'
                    style='width:25px;height:25px;'>Eutellidae<br/>
                    <img src='/mapicons/lasiocampidae_icon.png'  
                    style='width:25px;height:25px;'>Lasiocampidae<br/>
                    <img src='/mapicons/limacodidae_icon.png'  
                    style='width:25px;height:25px;'>Limacodidae<br/>
                    <img src='/mapicons/noctuidae_icon.png' 
                    style='width:25px;height:25px;'>Noctuidae<br/>
                    <img src='/mapicons/nolidae_icon.png'  
                    style='width:25px;height:25px;'>Nolidae<br/>
                    <img src='/mapicons/notodontidae_icon.png' 
                    style='width:25px;height:25px;'>Notodontidae<br/>
                    <img src='/mapicons/saturniidae_icon.png' 
                    style='width:25px;height:25px;'>Saturniidae<br/>
                    <img src='/mapicons/sphingidae_icon.png'  
                    style='width:25px;height:25px;'>Sphingidae<br/>"
  }
  ## Rhopalocera 
  if (input$day == TRUE & input$night == FALSE){
    html_legend <- "<b>Rhopalocera</b><br/>
                    <img src='/mapicons/hesperiidae_icon.png'
                    style='width:25px;height:25px;'>Hesperiidae<br/> 
                    <img src='/mapicons/lycaenidae_icon.png'
                    style='width:25px;height:25px;'>Lycaenidae<br/>
                    <img src='/mapicons/nymphalidae_icon.png' 
                    style='width:25px;height:25px;'>Nymphalidae<br/>
                    <img src='/mapicons/papilionidae_icon.png'
                    style='width:25px;height:25px;'>Papilionidae<br/>
                    <img src='/mapicons/pieridae_icon.png'
                    style='width:25px;height:25px;'>Pieridae<br/>
                    <img src='/mapicons/riodinidae_icon.png'   
                    style='width:25px;height:25px;'>Riodinidae<br/>"
  }
  ## Heterocera
  if (input$day == FALSE & input$night == TRUE){
    html_legend <- "<b>Heterocera</b><br/>
                    <img src='/mapicons/brahmaeidae_icon.png' 
                    style='width:25px;height:25px;'>Brahmaeidae<br/>
                    <img src='/mapicons/cossidae_icon.png' 
                    style='width:25px;height:25px;'>Cossidae<br/>
                    <img src='/mapicons/drepanidae_icon.png'
                    style='width:25px;height:25px;'>Drepanidae<br/>
                    <img src='/mapicons/endromidae_icon.png' 
                    style='width:25px;height:25px;'>Endromidae<br/>
                    <img src='/mapicons/erebidae_icon.png' 
                    style='width:25px;height:25px;'>Erebidae<br/>
                    <img src='/mapicons/geometridae_icon.png'
                    style='width:25px;height:25px;'>Geometridae<br/>
                    <img src='/mapicons/hepialidae_icon.png'  
                    style='width:25px;height:25px;'>Hepialidae<br/>
                    <img src='/mapicons/euteliidae_icon.png'
                    style='width:25px;height:25px;'>Eutellidae<br/>
                    <img src='/mapicons/lasiocampidae_icon.png'  
                    style='width:25px;height:25px;'>Lasiocampidae<br/>
                    <img src='/mapicons/limacodidae_icon.png'  
                    style='width:25px;height:25px;'>Limacodidae<br/>
                    <img src='/mapicons/noctuidae_icon.png' 
                    style='width:25px;height:25px;'>Noctuidae<br/>
                    <img src='/mapicons/nolidae_icon.png'  
                    style='width:25px;height:25px;'>Nolidae<br/>
                    <img src='/mapicons/notodontidae_icon.png' 
                    style='width:25px;height:25px;'>Notodontidae<br/>
                    <img src='/mapicons/saturniidae_icon.png' 
                    style='width:25px;height:25px;'>Saturniidae<br/>
                    <img src='/mapicons/sphingidae_icon.png'  
                    style='width:25px;height:25px;'>Sphingidae<br/>"
  }
  
  ## render map
  output$o_map <- renderLeaflet({
    
    leaflet(data = db_map) %>%
      addTiles() %>% 
      addMarkers(~longitude, ~latitude, icon = ~butterflyicons[family], 
                 label = ~as.character(pictures), popup = ~as.character(paste("<strong>",family, genus, species,"</strong>",      ## pictures = genus+species
                                                                              "<br>", "location:", land, ",", location, 
                                                                              "<br>", "date :", D_M_Y,
                                                                              "<br>", "total number of this species:", total_number_of_this_species,
                                                                              "<br>", "german name:", german_name
                                                                                       )
                                                                                 ), 
                 clusterOptions = markerClusterOptions( iconCreateFunction=JS("function (cluster) {    
                                var childCount = cluster.getChildCount(); 
                                var c = ' marker-custom-';  
                                if (childCount > 100) {  
                                  c += 'large'; } 
                                else if (childCount > 50) {  
                                  c += 'medium'; } 
                                else { 
                                  c += 'small'; }    
                                return new L.DivIcon({ html: '<div><span>' + childCount + '</span></div>',
                                className: 'marker-cluster' + c,
                                iconSize: new L.Point(40, 40) });
                }")) 
      ) %>% 
      addControl(html = html_legend, position = "bottomleft")
  })
  
  ## render empty map
  if (nrow(df_selected_filter()) < 1){
    output$o_map <- renderLeaflet({
      leaflet() %>% 
        addTiles()
    })
  }
  
 
## selected filters ############################################################
 
    output$o_suborder <- if (input$day == TRUE & input$night == TRUE){
        renderText({paste("suborder: Rhopalocera and Heterocera (butterflies and moths)")
      })
    }
    else if (input$day == TRUE & input$night == FALSE){
      renderText({paste("suborder: Rhopalocera (butterflies)")
      })
    }
    else if (input$day == FALSE & input$night == TRUE){
      renderText({paste("suborder: Heterocera (moths)")
      })
    }
    output$o_family <- if (!is.null(input$family)){
      renderText({
        str_family1 <- paste("family: ")
        str_family2 <- paste(isolate(input$family), collapse = ", ")
        HTML(paste(str_family1, str_family2))
      })
    }
    output$o_genus <- if (!is.null(input$genus)){
      renderText({
        str_genus1 <- paste("genus: ")
        str_genus2 <- paste(isolate(input$genus), collapse = ", ")
        HTML(paste(str_genus1, str_genus2))
      })
    }
    output$o_species <- if (!is.null(input$species)){
      renderText({ 
        str_species1 <- paste("species: ")
        str_species2 <- paste(isolate(input$species), collapse = ", ")
        HTML(paste(str_species1, str_species2))
      })
    }
    output$o_german_name <- if (!is.null(input$german_name)){
      renderText({ 
        str_german_name1 <- paste("german name: ")
        str_german_name2 <- paste(isolate(input$german_name), collapse = ", ")
        HTML(paste(str_german_name1, str_german_name2))
      })
    }
    output$o_land <- if (!is.null(input$land)){
      renderText({
        str_land1 <- paste("land: ")
        str_land2 <- paste(isolate(input$land), collapse = ", ")
        HTML(paste(str_land1, str_land2))
      })
    }
    output$o_elevation <- if (input$elevation[1] == 0){
      renderText({ paste("Elevation from", isolate(input$elevation[1]), "to", isolate(input$elevation[2]), "meters and butterflies with no elevation data.")
      })
    }
    else {
      renderText({ paste("Elevation from", isolate(input$elevation[1]), "to", isolate(input$elevation[2]), "meters.") 
      })
    }
    output$o_date  <- renderText({ paste("Date  from", isolate(input$date[1]), "to", isolate(input$date[2]), ".") 
    })  
    
    output$o_total_number <- renderText({ paste("Total number of a species from", isolate(input$total_number[1]), "to", isolate(input$total_number[2]), "butterflies.") 
    })  

    output$o_pictures <- if (input$pictures==TRUE){
      renderText({paste("Pictures on.")
      })
    }
    else {
      renderText({paste("Pictures off.")
      })
    }
    output$o_ffh <- if (!is.null(input$ffh)){
      renderText({
        str_ffh1 <- paste("FFH directive: ")
        str_ffh2 <- paste(isolate(input$ffh), collapse = ", ")
        str_ffh2 <- gsub("(II)", "Annex II species",
                    gsub("(IV)", "Annex IV species", str_ffh2))
        HTML(paste(str_ffh1, str_ffh2))
      })
    }
    output$o_redlist <- if (is.null(input$redlist)){
      renderText({paste("Red list Bavaria last updated: Rhopalocera 2016, Heterocera 2003")
      })
    }
    else {
      renderText({
        str_redlist1 <- paste("Red list Bavaria (last updated: Rhopalocera 2016, Heterocera 2003): ")
        str_redlist2 <- paste(isolate(input$redlist), collapse = ", ")
        str_redlist2 <- gsub("0", "0 (extinct)",
                        gsub("1", "1 (threatened by extinction)",
                        gsub("2", "2 (critically endangered)",
                        gsub("3", "3 (endangered)",
                        gsub("G", "G (danger of unknown extent)",
                        gsub("V", "V (prewarn list)",
                        gsub("＊", "＊ (not endangered)",   ##＊= FULLWIDTH ASTERISK!
                        gsub("D", "D (data deficient)",
                        gsub("R", "R (extremely rare)",
                        gsub("♦", "♦ (not evaluated)",
                        gsub("-", "- (not included)",str_redlist2))))))))))) ## 11*)
        HTML(paste(str_redlist1, str_redlist2))
      })
    }
    output$o_digitizer <- if (input$digitizer==TRUE){
      renderText({paste("Notes of the digitizer on.")
      })
    }
    else {
      renderText({paste("Notes of the digitizer off.")
      })
    }

## run button
    enable("run_button")
  }, ignoreInit = FALSE) 
  
  
## color brightness  ###########################################################

## color button  
  observeEvent(input$color_button,{

## filter db_color  
    if (length(input$color_butterfly_name )>0){ 
      
      originalData = reactiveVal(db_color)
  
      df_selected_color <- reactive({ 
        if (length(input$color_butterfly_name) >0){
        res <- originalData() %>% filter(genus_and_species %in% input$color_butterfly_name) 
        }
        if (input$color_height[1] >=0){
          res <- res %>% filter(between(as.numeric(elevation),as.numeric((input$color_height[1])),as.numeric((input$color_height[2]))))
        }
        if (length(input$color[1]) >0){
          res <- res %>% filter(between(as.numeric(color_brightness),as.numeric((input$color[1])),as.numeric((input$color[2])))) 
        }
        
         res
      })

## convert dataframe to datatable and sort by elevation
    db_color <- setDT(df_selected_color())
    db_color$color_brightness <- as.numeric(db_color$color_brightness)
    db_color$elevation <- as.numeric(db_color$elevation)
    db_color <- setorder(db_color, cols = "elevation")

## create regression line
    if(nrow(db_color)>0){
        if(length(unique(db_color$genus_and_species))==1){  
            y_regression <- db_color %>% filter(!is.na(elevation)) %>% lm(color_brightness ~ elevation,.) %>% fitted.values()
        }
        else{
            y_regression <- db_color %>% filter(!is.na(elevation)) %>% lm(color_brightness ~ elevation*genus_and_species,.) %>% fitted.values()
        }
    }
    else{
        shinyalert("No data for this selection. Please select different parameters.", type = "warning")
    }
    
## render plot
      output$o_color <- renderPlotly(
        plot_ly(db_color, x = ~elevation, y = ~color_brightness, color = ~genus_and_species,
                type = "scatter", mode = "markers",
                text = ~paste("location:", location)
        )%>%
        layout(xaxis = list(title = "elevation [m]"), 
                yaxis = list(title = "relative color brightness"),
                legend = list(title=list(text="Species"))
        )%>%
        add_trace(x = ~elevation, y = ~y_regression, mode = "lines")
      )
    
    
## calculate correlation
      print(length(unique(db_color$elevation)))
      if (length(unique(db_color$elevation)) > 1 & length(unique(db_color$color_brightness)) > 1){
        correlation <- round(cor(as.numeric(db_color$elevation), as.numeric(db_color$color_brightness), method = "pearson"), digits = 3)
        result <- cor.test(as.numeric(db_color$elevation), as.numeric(db_color$color_brightness), method = "pearson") 
        pvalue <- round(result$p.value, digits = 3)
        
        output$o_correlation <- if(pvalue < 0.05){ 
          renderText({paste("Pearson correlation coefficient:", correlation, ";", "p-value:", pvalue, ";", "The correlation is statistically significant.")
          })
        }
        else{
          renderText({paste("Pearson correlation coefficient:", correlation, ";", "p-value:", pvalue, ";", "The correlation is not statistically significant.")
          })      
        }
      }
      else{
        output$o_correlation <- renderText({paste("Not enough Data to calculate the correlation.")
        })
      }
      
      
## Each Correlation for the individual selections (not finished)
    ## create Table to print
      #db_correlation <- unique(db_color$genus_and_species[order(db_color$genus_and_species)])
      #print(db_correlation)
      
    ## Calculate the Pearson correlation coefficient
      #correlation <- for (i in db_correlation) {
        
      #    db_cor <- db_color %>% filter(genus_and_species == i)
      #    cor(as.numeric(db_cor$elevation), as.numeric(db_cor$color_brightness), method = "pearson") 
      #}
      #print(db_cor) passt so sind alle einzeln drin
      #print(correlation) Hier liegt der Fehler (wahrscheinlich im for loop) correlation immer NULL aber auch für jede i ein output
      
      
      #db_correlation <- cbind(db_correlation, pvalue=  1 ## =Platzhalter
      #    )
      
      #db_correlation <- cbind(db_correlation, pvalue=
      #    ifelse(db_correlation[,c(2)] < 0.05, "The correlation is statistically significant.", "The correlation is not statistically significant."))
      #print (db_correlation)

      
## Alert for no selected input  
    }
    else {
      shinyalert("No butterfly species selected. Please select at least one.", type = "warning",
                 time = 90000)
    }
    
## color button   
  }, ignoreInit = FALSE) 
  
  
}


## run the app #################################################################

shinyApp(ui, server)

