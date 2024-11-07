methylKitUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        bs4TabCard(
          width = 12,
          id = ns("methylKitTabCard"),
          maximizable = TRUE,
          status = "primary",
          headerBorder = TRUE,
          selected = NULL,
          side = "left",
          type = "tabs",
          solidHeader = TRUE,  # solid color background
          background = NULL,   # background color of the box
          sidebar = bs4CardSidebar(
            id = ns("sidebarMethylKit"),
            width = 25,  # Sidebar opening width in percentage
            background = "#333a40",
            startOpen = TRUE,
            icon = shiny::icon("gears"),
            easyClose = TRUE,
            # Manually namespace IDs in conditionalPanel conditions
            conditionalPanel(
              condition = paste0("input['", ns("methylKitTabCard"), "'] == 'Methylation Statistics'"),
              selectInput(
                inputId = ns("sampleHistograms"),
                label = "Select sample to display",
                choices = character(0),
                selected = character(0)
              ),
              colourInput(
                inputId = ns("color_methylation_histogram"),
                label = "Fill Color of Methylation Histogram",
                value = "#28a745",
                showColour = "both",
                palette = "square",
                allowTransparent = TRUE,
                returnName = FALSE,
                closeOnClick = FALSE,
                # width = NULL
                width = "100%"
              ),
              colourInput(
                inputId = ns("color_coverage_histogram"),
                label = "Fill Color of Coverage Histogram",
                value = "#3c8dbc",
                showColour = "both",
                palette = "square",
                allowTransparent = TRUE,
                returnName = FALSE,
                closeOnClick = FALSE,
                # width = NULL
                width = "100%"
              )
            ),  # close conditionalPanel Methylation Statistics
            conditionalPanel(
              condition = paste0("input['", ns("methylKitTabCard"), "'] == 'PCA of samples'"),
              # Uncomment and namespace if needed
              # pickerInput(
              #   inputId = ns("pickFactorsPCA"),
              #   label = "Select PCs to plot", 
              #   choices = character(0),
              #   multiple = TRUE,
              #   options =  list("max-options" = 2)
              # ),
              # verbatimTextOutput(outputId = ns("res_classic")),
              selectInput(
                inputId = ns("pickFactor1PCA"),
                label = "Select PC for x-axis",
                choices = character(0),
                selected = character(0)
              ),
              selectInput(
                inputId = ns("pickFactor2PCA"),
                label = "Select PC for y-axis",
                choices = character(0),
                selected = character(0)
              ),
              textInput(
                inputId = ns("pcaTitle"),
                label = "Title of plot",
                value = "PCA plot of samples"
              ),
              checkboxInput(
                inputId = ns("sampleLabelsPCA"),
                label = "Display sample labels",
                value = TRUE
              ),
              numericInput(
                inputId = ns("pointSizePCA"),
                label = "Point size", min = 0, max = 6,
                value = 4, step = 0.5
              ),
              numericInput(
                inputId = ns("textSizePCA"),
                label = "Font Size", min = 8, max = 30,
                value = 18, step = 1
              )
            )  # close conditionalPanel PCA of samples
          ),  # close sidebar
          tabPanel(
            title = "Results Table",
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            dataTableOutput(
              outputId = ns("methylKitTable"),
              width = "100%"
            )
          ),
          tabPanel(
            title = "Methylation Statistics",
            width = NULL,
            solidHeader = TRUE,
            fluidRow(
              column(
                width = 6,
                plotOutput(
                  outputId = ns("methylationHistogram"),
                  inline = FALSE,
                  width = "100%"
                )
              ),
              column(
                width = 6,
                plotOutput(
                  outputId = ns("coverageHistogram"),
                  inline = FALSE,
                  width = "100%"
                )
              )
            )
          ),  # close tabPanel Methylation Statistics
          tabPanel(
            title = "PCA of samples",
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            plotOutput(
              outputId = ns("pcaPlot"),
              inline = FALSE,
              width = "100%"
            ),
            fluidRow(
              column(
                width = 6,
                plotOutput(
                  outputId = ns("pcaScree"),
                  inline = FALSE,
                  width = "100%"
                )
              )
            )
            # Uncomment and namespace if needed
            # DT::dataTableOutput(ns("pcaLoadings"))
          )  # close tabPanel PCA
        )  # close bs4TabCard
      )  # close column
    )  # close fluidRow
  )  # close tagList
}
