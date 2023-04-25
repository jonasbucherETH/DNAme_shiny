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
library("DT")
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
library("KEGGREST")
library("biomaRt")
library("BioMartGOGeneSets")
library("parallel")
library("AnnotationHub")
library("GenomicFeatures")
library("gridExtra")
library("cowplot")
# library("shinydashboardPlus")
library(dplyr)
library(plyr)
library(readxl)

breakStrings <- function(x, minSizeForBreak = 20, lb = "\n", nb = 2) {
  sapply(x, minSizeForBreak = minSizeForBreak, lb = lb, FUN = function(x, minSizeForBreak, lb) {
    if (nchar(x) <= minSizeForBreak) {
      return(x)
    }
    
    g <- gregexpr(" ", x)[[1]]
    if (length(g) == 0) {
      return(x)
    }
    if (length(g) == 1 & all(g == -1)) {
      return(x)
    }
    
    if (nb == 2) {
      mid <- nchar(x) / 2
      mid <- g[order(abs(g - mid))[1]]
      substr(x, mid, mid) <- lb
    } else if (nb == 3) {
      mid1 <- round(nchar(x) / 3)
      mid2 <- mid1 * 2
      mid1 <- g[order(abs(g - mid1))[1]]
      mid2 <- g[order(abs(g - mid2))[1]]
      substr(x, mid1, mid1) <- lb
      substr(x, mid2, mid2) <- lb
    }
    
    return(x)
  })
}


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
        # startExpanded = TRUE,
        # menuSubItem(
        #   text = "BP",
        #   # tabName = "subtab-great-BP",
        #   tabName = "tab-great",
        #   icon = NULL,
        # )
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
      source("ui-great.R", local = TRUE)$value,
      source("ui-methylKit.R", local = TRUE)$value
    )
  )
  # controlbar = dashboardControlbar(
  #   id = "controlbar",
  #   controlbarMenu(
  #     id = "controlbarMenu",
  #     controlbarItem(
  #       "BP",
  #       "BP selected"
  #     ),
  #     controlbarItem(
  #       "MF",
  #       "MF selected"
  #     )
  #   )
  # )
)

server <- function(input, output, session) {
  source("server-inputData.R", local = TRUE)
  source("server-DMRseq.R", local = TRUE)
  source("server-great.R", local = TRUE)
  source("server-methylKit.R", local = TRUE)
}

shinyApp(ui = ui, server = server)
