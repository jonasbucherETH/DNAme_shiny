tabItem(
  tabName = "tab-methylKit",
  fluidRow( ### NOTE: 1 row has width = 12
    column(
      width = 12,
      tabBox(
        title = NULL,
        width = 10,
        tabPanel(
          title = "Methylation Statistics",
          width = NULL,
          solidHeader = TRUE,
          status = "primary",
          plotOutput(
            outputId = "methylationHistogram",
            inline = F,
            # width = "100%",
            height = "auto"
          )
          # collapsible = FALSE,
          # collapsed = FALSE,
        )
      ), # close tabPanel
      box(
        title = "parameters",
        width = 2,
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

