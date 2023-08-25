tabItem(
  tabName = "tab-DMRseq",
  fluidRow( ### NOTE: 1 row has width = 12
    column(
      width = 12,
      bs4TabCard(
        width = NULL,
        id = "dmrseqTabCard",
        maximizable = TRUE,
        status = "primary",
        headerBorder = T,
        selected = NULL,
        side = "left",
        type = "tabs",
        solidHeader = T, # solid color background
        background = NULL, #  background color of the box,
        height = "800px",
        sidebar = bs4CardSidebar(
          id = "sidebardmrseq",
          width = 25, # Sidebar opening width in percentage
          background = "#333a40",
          startOpen = TRUE,
          icon = shiny::icon("gears"),
          easyClose = TRUE,
          conditionalPanel( # conditionalPanel Empirical Distribution
            condition = "input.dmrseqTabCard == 'Empirical distribution plot'",
            numericInput("width", "Image Width", min = 100, max = 1000, value = 400),
            numericInput("height", "Image Height", min = 100, max = 1000, value = 300),
            selectInput("format", "Output Format", choices = c("png", "pdf", "svg"), selected = "png"),
            downloadButton("downloadBtn", "Download"),
            selectInput(
              inputId = "xFactorEmpiricalDistribution",
              label = "Select distribution",
              choices = c("M", "Cov"),
              selected = "M"
            ),
            checkboxInput(
              inputId = "bySample",
              label = "Plot a separate line for each sample",
              value = FALSE
            ),
            numericInput(
              inputId = "textSizeDMRseq",
              label = "Font Size", min = 4, max = 30,
              value = 12, step = 0.5
              # width = "100px"
            )
          ), # close conditionalPanel Empirical Distribution
          conditionalPanel( # conditionalPanel DMR plot
            condition = "input.dmrseqTabCard == 'DMR plot'",
            selectInput(
              inputId = "dmrFactor1", 
              label = "Select which factor to colour the samples by", 
              choices = "", 
              selected = ""
            ),
            checkboxGroupInput(
              inputId = "dmrGroups",
              label = "Select groups to plot",
              choices = "",
              selected = ""
            ),
            numericInput(
              inputId = "selectRegion",
              label = "Select Region to plot",
              value = 1
              # min = NA,
              # max = NA,
              # step = NA,
              # width = NULL
            )
          ) # close conditionalPanel DMR plot
        ), # close sidebar
        tabPanel(
          title = "Empirical distribution plot",
          width = NULL,
          solidHeader = TRUE,
          status = "primary",
          plotOutput(
            outputId = "empiricalDistributionPlot",
            inline = F,
            width = "100%"
          )
        ), # close tabPanel Empirical distribution plot
        tabPanel(
          title = "DMR plot",
          width = NULL,
          solidHeader = TRUE,
          status = "primary",
          plotOutput(
            outputId = "dmrPlot",
            inline = F,
            width = "100%"
          )
        ) # close tabPanel DMR plot
      ) # close tabCard
    ) # close  column
  ) # close fluidRow
) # close tabItem

