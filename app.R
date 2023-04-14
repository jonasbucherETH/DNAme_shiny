library("shiny")
library("shinymanager")
library("shinyjs")
library("tidyverse")
library("ggpubr")
library("plotly")
library("RColorBrewer")
# library("ComplexHeatmap")
# library("tximport")
# library("vsn")
# library("clusterProfiler")
# library("DT")
# library("colourpicker")
# library("writexl")
# library("circlize")
# library("GO.db")
library("shinydashboard")
# library("shinyBS")
# library("pixiedust")
library("ezRun")
# library("kableExtra")
# library("ggrepel")
# library("gplots")
# library("sortable")
# library("waiter")
# library("shinycssloaders")
# library("ggprism")
# library("ggbeeswarm")
# library("rstatix")
# library("gridExtra")
# library("shinytitle")
# library("shinylogs")

# JB libraries
library("dmrseq")
library("rGREAT")
library("circlize")
library("GetoptLong")

# console.error = function () {
#   require("system").stderr.write(Array.prototype.join.call(arguments, ' ') + '\n');
# };

reactiveConsole(TRUE)

# For secure login:
# library(digest)
# digest("password1", algo = "md5")
# credentials <- data.frame(
#   user = c("user1", "user2"),
#   password = c(
#     "password1", "password2"),
#   admin = c(FALSE, TRUE),
#   comment = "Login Page.",
#   stringsAsFactors = FALSE
# )

########
# spinner <- tagList(
#   spin_chasing_dots(),
#   span("Loading stuff...", style="color:white;")
# )

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(
    title = "DNAme",
    titleWidth = 200
    # tags$li(
    #   a(
    #     href = 'mailto:sequencing@fgcz.ethz.ch?subject=exploreDEG-shiny-app-feedback',
    #     "Request Features/Report Bugs"),
    #   class = "dropdown"
    # ),
    # tags$li(
    #   a(href = 'http://www.fgcz.ch',
    #     target = "_blank",
    #     img(src = 'fgcz_logo.png', title = "FGCZ", height = "30px"),
    #     style = "padding-top:10px; padding-bottom:5px;"),
    #   class = "dropdown"),
    # tags$li(
    #   a(href = 'http://www.ethz.ch/en.html',
    #     target = "_blank",
    #     img(src = 'eth_logo.png', title = "FGCZ", height = "22px"),
    #     style = "padding-top:13px; padding-bottom:10px;"),
    #   class = "dropdown"),
    # tags$li(
    #   a(href = 'http://www.uzh.ch/en.html',
    #     target = "_blank",
    #     img(src = 'University_of_Zurich_Logo.png', title = "FGCZ", height = "30px"),
    #     style = "padding-top:10px; padding-bottom:5px;"),
    #   class = "dropdown")
  ),
  dashboardSidebar(
    width = 200,
    shinyjs::useShinyjs(),
    sidebarMenu(
      id = "tabs",
      menuItem(
        text = "DMR",
        tabName = "tab-DMRseq",
        icon = icon("ranking-star")
      ),
      menuItem(
        text = "rGREAT",
        tabName = "tab-great",
        icon = icon("ranking-star")
      )
    )
  ),
  dashboardBody(
    # use_tracking(),
    # tags$head(tags$link(rel = "shortcut icon", href = "sushi.png")),
    # tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "main.css")),
    # use_waiter(),
    
    tabItems(
      source("ui-DMRseq.R", local = TRUE)$value,
      source("ui-great.R", local = TRUE)$value
    )
  )
)

server <- function(input, output, session) {
  source("server-inputData.R", local = TRUE)
  source("server-DMRseq.R", local = TRUE)
  source("server-great.R", local = TRUE)
}

shinyApp(ui = ui, server = server)
