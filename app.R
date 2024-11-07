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
# library("shinydashboard")
# library("shinyBS")
# library("pixiedust")
# library("ezRun")
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
library("bs4Dash")
library("dplyr")
library("plyr")
library("readxl")
library("methylKit")
library("shinyWidgets")
library("colourpicker")
library("htmltools")
# library(data.table)
library("scales") # heatmap
library("svglite") # for Homer
library("ComplexHeatmap") # conflift with monaLisa
library("monaLisa") # for homerToPFMatrixList
library("InteractiveComplexHeatmap")
library("readr") # for read_tsv
library("sortable")
library("kableExtra")

### new
library("gplots") # col2hex
library("rtracklayer")

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

# TODO: un-comment this again
# source("output_module.R")
source("dataInputModule.R")
source("server-Overview-module.R")
source("ui-Overview-module.R")
source("server-dmrseq-module.R")
source("ui-dmrseq-module.R")
source("server-methylKit-module.R")
source("ui-methylKit-module.R")
source("server-great-module.R")
source("ui-great-module.R")
source("server-monaLisa.R")
source("ui-monaLisa.R")

ui <- dashboardPage(
  # skin = "black",
  fullscreen = T,
  # help = T,
  header = dashboardHeader(
    title = "Methylator",
    # titleWidth = 200
    # shinydashboardPlus::socialButton(
    #   url = "https://github.com/jonasbucherETH",
    #   type = "github"
    # ),
    tags$li(
      a(
        href = 'mailto:sequencing@fgcz.ethz.ch?subject=exploreDEG-shiny-app-feedback',
        "Request Features/Report Bugs"),
      class = "dropdown"
    ),
    tags$li(
      a(href = 'http://www.fgcz.ch',
        target = "_blank",
        img(src = 'fgcz_logo.png', title = "FGCZ", height = "30px"),
        style = "padding-top:10px; padding-bottom:5px;"),
      class = "dropdown"),
    tags$li(
      a(href = 'http://www.ethz.ch/en.html',
        target = "_blank",
        img(src = 'eth_logo.png', title = "FGCZ", height = "22px"),
        style = "padding-top:13px; padding-bottom:10px;"),
      class = "dropdown"),
    tags$li(
      a(href = 'http://www.uzh.ch/en.html',
        target = "_blank",
        img(src = 'University_of_Zurich_Logo.png', title = "FGCZ", height = "30px"),
        style = "padding-top:10px; padding-bottom:5px;"),
      class = "dropdown")
  ),
  sidebar = dashboardSidebar(
    width = 200,
    shinyjs::useShinyjs(),
    # Then include your navigation menu
    sidebarMenu(
      id = "tabs",
      menuItem(
        text = "Upload Data",
        tabName = "tab-inputData",
        icon = icon("import", lib = "glyphicon")
      ),
      menuItem(
        text = "Settings",
        tabName = "tab-Overview",
        icon = icon("cogs")
      ),
      menuItem(
        text = "DMRs",
        tabName = "tab-dmrseq",
        icon = icon("chart-area")
      ),
      menuItem(
        text = "methylKit",
        tabName = "tab-methylKit",
        icon = icon("chart-simple")
      ),
      menuItem(
        text = "Functional analysis",
        tabName = "tab-great",
        # startExpanded = TRUE,
        icon = icon("chart-pie"),
        startExpanded = FALSE,
        # bs4SidebarMenuSubItem(
        #   text,
        #   tabName = NULL,
        #   href = NULL,
        #   newTab = NULL,
        #   icon = shiny::icon("angles-right"),
        #   selected = NULL
        # )
        menuSubItem(
          text = "Biological Processes",
          tabName = "BP",
          icon = icon("disease")
        ),
        menuSubItem(
          text = "Cellular Components",
          tabName = "CC",
          icon = icon("microscope")
        ),
        menuSubItem(
          text = "Molecular Functions",
          tabName = "MF",
          icon = icon("dna", lib = "font-awesome")
        )
      ),
      menuItem(
        text = "Motif analysis",
        tabName = "tab-monaLisa",
        icon = icon("text-height")
      )
    )

      # ),
      # # menuItem(
      # #   text = "Biological Processes",
      # #   tabName = "BP",
      # #   icon = icon("dna")
      # # ),
      # # menuItem(
      # #   text = "Cellular Component",
      # #   tabName = "CC",
      # #   icon = icon("dna")
      # # ),
      # # menuItem(
      # #   text = "Molecular Function",
      # #   tabName = "MF",
      # #   icon = icon("dna")
      # # ),
      #   # menuItemOutput("menuItemReactome"),
      #   # menuItemOutput("menuItemKegg")
      # menuItem(
      #   text = "Motif analysis",
      #   tabName = "tab-HOMER",
      #   icon = icon("text-height")
      # )
  ),
  body = dashboardBody(
    # use_tracking(),
    # tags$head(tags$link(rel = "shortcut icon", href = "sushi.png")),
    # tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "main.css")),
    # use_waiter(),
    # tags$head(
    #   tags$style(
    #     "body {overflow-y: hidden;}"
    #   )
    # ),
    tabItems(
      # source("ui-Overview.R", local = TRUE)$value,
      # source("ui-DMRseq.R", local = TRUE)$value
      # source("ui-great.R", local = TRUE)$value,
      # output_module_UI("output_BP"),
      # output_module_UI("output_CC"),
      # output_module_UI("output_MF")
      tabItem(
        tabName = "tab-inputData",
        dataInputUI(id = "inputData")
      ),
      tabItem(
        tabName = "tab-Overview",
        overviewUI(id = "overview")
      ),
      tabItem(
        tabName = "tab-dmrseq",
        dmrseqUI(id = "dmrseq")
      ),
      tabItem(
        tabName = "tab-methylKit",
        methylKitUI(id = "methylKit")
      ),
      tabItem(
        tabName = "BP",
        greatUI("output_BP")
      ),
      tabItem(
        tabName = "CC",
        greatUI("output_CC")
      ),
      tabItem(
        tabName = "MF",
        greatUI("output_MF")
      ),
      
      tabItem(
        tabName = "Motif analysis 1",
        monaLisaUI("se1")
      )

      # source("ui-methylKit.R", local = TRUE)$value
      # source("ui-HOMER.R", local = TRUE)$value
    )
  ),
  controlbar = dashboardControlbar()
  
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
  # run_module: c(dmrseq, methylkit, rgreat, monalisa)
  # run_module <- c(TRUE, FALSE, FALSE, FALSE)
  # run_module <- c(FALSE, TRUE, FALSE, FALSE)
  run_module <- c(TRUE, TRUE, FALSE, FALSE)
  # run_module <- c(FALSE, FALSE, FALSE, TRUE)
  # source("server-inputData.R", local = TRUE)
  dataValues <- reactiveValues()
  sharedValues <- reactiveValues()
  dataInputServer(id = "inputData", dataValues = dataValues, sharedValues = sharedValues)
  overviewServer(id = "overview", dataValues = dataValues, sharedValues = sharedValues)
  # source("server-Overview.R", local = TRUE)
  
  if(run_module[1]){
    # source("server-DMRseq.R", local = TRUE)
    dmrseqServer("dmrseq", dataValues = dataValues, sharedValues = sharedValues)
    
  }
  
  if(run_module[2]){
    methylKitServer("methylKit", dataValues, sharedValues)
  }
  if(run_module[3]){
    # greatResult_BP <- rGREAT_rds$greatResult_BP
    # greatResult_CC <- rGREAT_rds$greatResult_CC
    # greatResult_MF <- rGREAT_rds$greatResult_MF
    # greatResult_BP <- dataValues$rGREAT_rds$greatResult_BP
    # greatResult_CC <- dataValues$rGREAT_rds$greatResult_CC
    # greatResult_MF <- dataValues$rGREAT_rds$greatResult_MF
    # enrichmentTable_BP <- getEnrichmentTable(greatResult_BP)
    # enrichmentTable_CC <- getEnrichmentTable(greatResult_CC)
    # enrichmentTable_MF <- getEnrichmentTable(greatResult_MF)
    # greatServer(id = "output_BP", greatResult = greatResult_BP, enrichmentTable = enrichmentTable_BP)
    # greatServer(id = "output_CC", greatResult = greatResult_CC, enrichmentTable = enrichmentTable_CC)
    # greatServer(id = "output_MF", greatResult = greatResult_MF, enrichmentTable = enrichmentTable_MF)
    greatServer(id = "output_BP", greatResult = dataValues$rGREAT_rds$greatResult_BP, sharedValues = sharedValues)
    greatServer(id = "output_CC", greatResult = dataValues$rGREAT_rds$greatResult_CC, sharedValues = sharedValues)
    greatServer(id = "output_MF", greatResult = dataValues$rGREAT_rds$greatResult_MF, sharedValues = sharedValues)
  }
  if(run_module[4]){
    # source("server-HOMER.R", local = TRUE)
    source("server-monaLisa.R")
    # se <- inputDataReactive()$se
    monaLisaServer(id = "se1", summarizedExp = se_monaLisa)
  }
}

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

shinyApp(ui = ui, server = server)
