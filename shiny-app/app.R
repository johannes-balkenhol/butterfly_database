source("global.R")

ui <- dashboardPage(
  dashboardHeader(title = "Butterfly Collection", titleWidth = 208),
  dashboardSidebar(source("ui/sidebarPanel.R")$value),
  dashboardBody(source("ui/main_dashboard.R")$value),
  #dashboardBody(source("ui/color_plot_ui.R")$value)
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
