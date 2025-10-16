tutorial_page <- fluidPage(
  tags$head(
    tags$style(HTML("
      .tutorial-section {
        background-color: #fff9e6;
        border-left: 4px solid #cf9206;
        padding: 15px;
        margin-bottom: 20px;
        border-radius: 4px;
      }
      .tutorial-step {
        background-color: #ffffff;
        border: 1px solid #f0c20c;
        padding: 12px;
        margin-bottom: 15px;
        border-radius: 4px;
      }
      .step-number {
        background-color: #cf9206;
        color: white;
        border-radius: 50%;
        width: 30px;
        height: 30px;
        display: inline-flex;
        align-items: center;
        justify-content: center;
        font-weight: bold;
        margin-right: 10px;
      }
      .tutorial-icon {
        color: #cf9206;
        margin-right: 8px;
      }

    "))
  ),
  
  headerPanel(""),
  
  ## Welcome Section
  fluidRow(
    box(
      title = "Welcome to the Butterfly Collection Database", 
      status = "warning", 
      width = 12,
      solidHeader = TRUE,
      h4("A FAIR-Compliant Butterfly Database: a Digital Archive Enabling Global Accessibility, Scientific Research, and Scalability"),
      p("In pursuit of enhancing research data management (RDM) and adhering to the FAIR principles 
        (Findable, Accessible, Interoperable, Reusable), we have meticulously constructed a comprehensive Butterfly Database. This database serves 
        as a digital repository for a rich collection of butterfly specimens, thoughtfully digitized through high-quality photographs, and enriched 
        with crucial metadata. Our project leveraged the power of R and R-Shiny to provide global access to this invaluable resource, enabling researchers 
        and enthusiasts worldwide to seamlessly interact with the data. This database digitalizes the collection of Arthur Bott."),
      p("The collection contains a total of 5,974 butterflies (Lepidoptera) across 47 insect boxes, with specimens dating from 1900 to 2002, covering over 
        a century. It comprises 351 genera and 680 species, totalling 712 taxa, representing a significant portion of European butterfly species."),
    )
  ),
  
  ## Quick Start Guide
  fluidRow(
    box(
      title = "Quick Start Guide", 
      status = "warning", 
      width = 12,
      collapsible = TRUE,
      
      div(class = "tutorial-step",
          span(class = "step-number", "1"),
          strong("Select Suborder:"),
          p("Select Rhopalocera (butterflies) or/and Heterocera (moths) to search in specific database or combined. At least one must be selected.")
      ),
      
      div(class = "tutorial-step",
          span(class = "step-number", "2"),
          strong("Apply Filters and Search"),
          p(" Multiple filters are present for the user to narrow their search. These include family, genus, species, location, elevation, date range, or protection status. Press search after choosing the wanted filters.")
      ),
      
      div(class = "tutorial-step",
          span(class = "step-number", "3"),
          strong("Explore Results:"),
          tags$ul(
            tags$li("The search results start with the phylogenetic tree of species found with the search with their genus, family and suborder information."),
            tags$li("Next is the speciment table which can be used to check details for the foudn butterflies along with an species indentifier (PIDs) which can be checked for more information."),
            tags$li("A geogrpahical map is also present to identify collection sites and below that is the interactive analysis panel linking elevation with color brightness traits derived from specimen images."),
          )
      ),
      
      div(class = "tutorial-step",
          span(class = "step-number", "4"),
          strong("Download Data:"),
          p("The results can be exported by using the download buttons above the Specimen Table in Excel, CSV, or ABCD/DwC XML format.")
      )
    )
  ),
  
  ## Main Filters Section
  fluidRow(
    box(
      title = "Main Filters & Options", 
      status = "warning", 
      width = 12,
      collapsible = TRUE,
      collapsed = TRUE,
      
      h4(icon("filter", class = "tutorial-icon"), "Taxonomic Filters"),
      div(class = "tutorial-section",
          p(strong("Suborder:"), "Choose between Rhopalocera (butterflies) and Heterocera (moths). Both can be selected simultaneously."),
          p(strong("Family:"), "Filter by taxonomic family from the available list. Available families change based on your suborder selection."),
          p(strong("Genus:"), "Select one or more genera to view only those specimens."),
          p(strong("Species:"), "Filter by specific species names."),
          p(strong("German Name:"), "Search using common names for butterflies in German.")
      ),
      
      h4(icon("map-marker", class = "tutorial-icon"), "Geographic Filters"),
      div(class = "tutorial-section",
          p(strong("Land:"), "Filter specimens by country where they were collected."),
          p(strong("Elevation:"), "Set minimum and maximum elevation range. Note: 0 indicates no elevation data is available for that specimen.")
      ),
      
      h4(icon("calendar", class = "tutorial-icon"), "Temporal & Quantity Filters"),
      div(class = "tutorial-section",
          p(strong("Date Range:"), "Select the collection date range using the date picker."),
          p(strong("Total Number:"), "Filter by the total number of specimens of a species in the collection.")
      ),
      
      h4(icon("shield", class = "tutorial-icon"), "Conservation Status"),
      div(class = "tutorial-section",
          p(strong("FFH Directive:"), "Filter species protected under EU's Fauna-Flora-Habitat directive (Annex II or IV)."),
          p(strong("Red List Bavaria:"), "Filter by conservation status according to Bavaria's Red List:"),
          tags$ul(
            tags$li("0: extinct"),
            tags$li("1: threatened by extinction"),
            tags$li("2: critically endangered"),
            tags$li("3: endangered"),
            tags$li("G: danger of unknown extent"),
            tags$li("R: extremely rare"),
            tags$li("V: prewarn list"),
            tags$li("＊: not endangered"),
            tags$li("D: data deficient"),
            tags$li("♦: not evaluated"),
            tags$li("-: not included")
          )
      ),
      
      h4(icon("eye", class = "tutorial-icon"), "Display Options"),
      div(class = "tutorial-section",
          p(strong("Pictures on/off:"), "Toggle whether specimen images are displayed in the table."),
          p(strong("Digitizer Notes:"), "Show or hide notes added by the digitizer during data entry.")
      )
    )
  ),
  
  ## Results Interpretation
  fluidRow(
    box(
      title = "Understanding Your Results", 
      status = "warning", 
      width = 12,
      collapsible = TRUE,
      collapsed = TRUE,
      
      p("The main display area is organized into collapsible sections that facilitate interactive exploration of the dataset. It includes:"),
      
      h4(icon("table", class = "tutorial-icon"), "Results Table"),
      div(class = "tutorial-section",
          p("A searchable results table with stable specimen identifiers (PIDs) and key metadata.
            Clicking on a specimen’s persistent identifier (PID) in the results table opens a detailed specimen information panel.", br(),
            "This interactive knowledge panel is divided into three sections:"),
          tags$ul(
            tags$li("Specimen-level data: Collection date, total number of individuals, conservation assessments (e.g., FFH Directive status, Bavarian Red List category), and collector metadata"),
            tags$li("Species-level data: Full taxonomic hierarchy (suborder, family, genus, species), vernacular names, and accepted binomial nomenclature."),
            tags$li("Environmental context: Geographic location (including country, coordinates), as well as ecological parameters such as annual temperature and precipitation, where available"),
          )
      ),
      
      h4(icon("sitemap", class = "tutorial-icon"), "Phylogenetic Tree"),
      div(class = "tutorial-section",
          p("Phylogenetic tree view of your filtered specimens, showing relationships between families, genera, and species.")
      ),
      
      h4(icon("globe", class = "tutorial-icon"), "Geographic Map"),
      div(class = "tutorial-section",
          p("Interactive map showing collection locations. Can be zoomed in and out. Butterfly icons in the bottom left corner can be used to identify families.")
      ),
      
      h4(icon("paint-brush", class = "tutorial-icon"), "Color Brightness vs Elevation"),
      div(class = "tutorial-section",
          p("Interactive analysis panel linking elevation with color brightness traits derived from specimen images.")
      )
    )
  ),
  
  ## Tips & Troubleshooting
  fluidRow(
    box(
      title = "Tips & Support", 
      status = "warning", 
      width = 12,
      collapsible = TRUE,
      collapsed = TRUE,
      
      h4(icon("lightbulb", class = "tutorial-icon"), "Helpful Tips"),
      div(class = "tutorial-section",
          tags$ul(
            tags$li("Start with broad filters and narrow down gradually for better results."),
            tags$li("Use the 'Selected Filters' box at the bottom to review your active filters."),
            tags$li("Download buttons work on filtered data - apply filters before downloading."),
            tags$li("Empty filter selections mean 'all' - don't select anything to see everything in that category."),
            tags$li("Some specimens may have missing data (elevation = 0, no photos, etc.) - this is normal for historical collections.")
          )
      ),
      
      h4(icon("question-circle", class = "tutorial-icon"), "Need Help?"),
      div(class = "tutorial-section",
          p("If you encounter any bugs, errors, or have feedback about the database, please contact:"),
          p(strong("felix1997weber2@gmail.com")),
          br(),
          p(icon("copyright"), "Weber Felix, Johannes Balkenhol"),
          p("Collection of Arthur Bott")
      )
    )
  )
)

# # Standalone app to test the tutorial page
# library(shiny)
# library(shinydashboard)

# ui <- dashboardPage(
  # dashboardHeader(title = "Butterfly Collection Tutorial"),
  # dashboardSidebar(
    # width = 250,
    # sidebarMenu(
      # menuItem("Tutorial", tabName = "tutorial", icon = icon("book"))
    # )
  # ),
  # tutorial_page
# )

# server <- function(input, output, session) {
  # # No server logic needed for static tutorial page
# }

# shinyApp(ui, server)