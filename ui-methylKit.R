tabItem(
  tabName = "tab-methylKit",
  fluidRow( ### NOTE: 1 row has width = 12
    column(
      width = 10,
      bs4TabCard(
        width = 12,
        id = NULL,
        # title = NULL,
        maximizable = TRUE,
        status = "primary",
        ###
        solidHeader = T, # solid color background
        background = NULL, #  background color of the box
        tabPanel(
          title = "Methylation Statistics",
          width = NULL,
          solidHeader = TRUE,
          status = "primary",
          fluidRow(
            column(
              width = 6,
              plotOutput(
                outputId = "methylationHistogram",
                inline = F,
                width = "100%"
                # height = "auto"
              )
            ),
            column(
              width = 6,
              plotOutput(
                outputId = "coverageHistogram",
                inline = F,
                width = "100%"
                # height = "auto"
              )
            )
          )
        ) # close tabPanel
      ), # close tabBox
    ), # close column
    column(
      width = 2,
      box(
        title = "parameters",
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        selectInput(
          inputId = "sample",
          label = "Select sample to display",
          choices = character(0),
          selected = character(0)
        )
      )
    ) # close column
  ) # close fluidRow
) # close tabItem

