source("global.R")

ui <- dashboardPage(
  dashboardHeader(title = "Butterfly Collection", titleWidth = 250),
  
  # Sidebar with filters (kept as-is)
  dashboardSidebar(
    width = 250,
    source("ui/sidebarPanel.R", local = TRUE)$value
  ),
  
  # Main body with top tab buttons
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper {
          transition: margin-left 0.3s ease;
          background-color: #ffffff;
          padding: 15px;
        }
        .main-header .logo { width: 250px !important; }
        .main-header .navbar { margin-left: 250px !important; }
        .main-sidebar {
          width: 250px !important;
          overflow-y: auto !important;
        }

        /* Adapt when sidebar collapsed */
        body.sidebar-collapse .content-wrapper {
          margin-left: 0 !important;
        }
        body.sidebar-collapse .main-header .navbar {
          margin-left: 0 !important;
        }

        /* Top tab buttons */
        .nav-tabs {
          background-color: #fff9e6;
          border-bottom: 2px solid #cf9206;
          position: sticky;
          top: 0;
          z-index: 1000;
        }
        .nav-tabs > li > a {
          color: #333;
          font-weight: bold;
        }
        .nav-tabs > li.active > a {
          background-color: #cf9206 !important;
          color: white !important;
        }
      "))
    ),
    
    # top tab buttons (Database / Tutorial)
    tabsetPanel(
      id = "tabs",
      tabPanel("Database", source("ui/main_dashboard.R", local = TRUE)$value),
      tabPanel("Tutorial", source("ui/tutorial_page.R", local = TRUE)$value)
    )
  )
)

server <- function(input, output, session) {
  source("server/filters.R", local = TRUE)
  source("server/family_tree.R", local = TRUE)
  source("server/map_plot.R", local = TRUE)
  source("server/color_brightness_plot.R", local = TRUE)
  source("server/table_output.R", local = TRUE)
  source("server/output_text.R", local = TRUE)
  source("server/pid_handler.R", local = TRUE)
}

shinyApp(ui, server)
