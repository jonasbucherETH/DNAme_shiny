tabItem(
  tabName = "tab-DMRseq",
  fluidRow( ### NOTE: 1 row has width = 12
    column(
      width = 12,
      bs4TabCard(
        width = 12,
        id = "dmrseqTabCard",
        maximizable = TRUE,
        status = "primary",
        headerBorder = T,
        selected = NULL,
        side = "left",
        type = "tabs",
        solidHeader = T, # solid color background
        background = NULL, #  background color of the box
        sidebar = bs4CardSidebar(
          id = "sidebardmrseq",
          width = 25, # Sidebar opening width in percentage
          background = "#333a40",
          startOpen = TRUE,
          icon = shiny::icon("gears"),
          easyClose = TRUE,
          conditionalPanel( # conditionalPanel Empirical Distribution
            condition = "input.dmrseqTabCard == 'Empirical Distribution plot'",
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
            )
          ), # close conditionalPanel Empirical Distribution
          conditionalPanel( # conditionalPanel DMR plot
            condition = "input.dmrseqTabCard == 'DMR plot'",
            numericInput(
              inputId = "selectRegion",
              label = "Select Region to plot",
              value = 1,
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

