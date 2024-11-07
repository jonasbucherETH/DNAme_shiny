dmrseqUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow( ### NOTE: 1 row has width = 12
      column(
        width = 12,
        bs4TabCard(
          width = NULL,
          id = ns("dmrseqTabCard"),
          maximizable = TRUE,
          status = "primary",
          headerBorder = TRUE,
          selected = NULL,
          side = "left",
          type = "tabs",
          solidHeader = TRUE, # solid color background
          background = NULL, # background color of the box,
          height = "800px",
          sidebar = bs4CardSidebar(
            id = ns("sidebardmrseq"),
            width = 25, # Sidebar opening width in percentage
            background = "#333a40",
            startOpen = TRUE,
            icon = shiny::icon("gears"),
            easyClose = TRUE,
            # Manually namespace IDs in conditionalPanel conditions
            conditionalPanel( # conditionalPanel Empirical Distribution
              condition = paste0("input['", ns("dmrseqTabCard"), "'] == 'Empirical distribution plot'"),
              numericInput(ns("width"), "Image Width", min = 100, max = 1000, value = 400),
              numericInput(ns("height"), "Image Height", min = 100, max = 1000, value = 300),
              selectInput(ns("format"), "Output Format", choices = c("png", "pdf", "svg"), selected = "png"),
              downloadButton(ns("download_empiricalDistribution"), "Download"),
              selectInput(
                inputId = ns("xFactorEmpiricalDistribution"),
                label = "Select distribution",
                choices = c("M", "Cov"),
                selected = "M"
              ),
              checkboxInput(
                inputId = ns("bySample"),
                label = "Plot a separate line for each sample",
                value = FALSE
              ),
              numericInput(
                inputId = ns("textSizeDMRseq"),
                label = "Font Size", min = 4, max = 30,
                value = 12, step = 0.5
              )
            ), # close conditionalPanel Empirical Distribution
            conditionalPanel( # conditionalPanel DMR plot
              condition = paste0("input['", ns("dmrseqTabCard"), "'] == 'DMR plot'"),
              # Uncomment and namespace these inputs if needed
              # selectInput(
              #   inputId = ns("dmrFactor1"), 
              #   label = "Select which factor to colour the samples by", 
              #   choices = "", 
              #   selected = ""
              # ),
              # checkboxGroupInput(
              #   inputId = ns("dmrGroups"),
              #   label = "Select groups to plot",
              #   choices = "",
              #   selected = ""
              # ),
              numericInput(
                inputId = ns("selectRegion"),
                label = "Select Region to plot",
                value = 1
              ),
              selectInput(
                inputId = ns("annotation_track"),
                label = "Select species for annotation",
                choices = c("None" = "", "dm3","dm6","galGal5","hg19","hg38","mm9","mm10","rn4","rn5","rn6"),
                selected = "None"
              )
            ) # close conditionalPanel DMR plot
          ), # close sidebar
          tabPanel(
            title = "Empirical distribution plot",
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            plotOutput(
              outputId = ns("empiricalDistributionPlot"),
              # inline = FALSE,
              width = "100%"
            )
          ), # close tabPanel Empirical distribution plot
          tabPanel(
            title = "DMR plot",
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            plotOutput(
              outputId = ns("dmrPlot"),
              inline = FALSE,
              width = "100%"
            )
          ) # close tabPanel DMR plot
        ) # close bs4TabCard
      ) # close column
    ) # close fluidRow
  )
}
