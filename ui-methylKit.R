tabItem(
  tabName = "tab-methylKit",
  fluidRow( ### NOTE: 1 row has width = 12
    column(
      width = 12,
      tabBox(
        title = NULL,
        width = 12,
        tabPanel(
          title = "Basic Statistics",
          width = NULL,
          solidHeader = TRUE,
          status = "primary",
          # collapsible = FALSE,
          # collapsed = FALSE,
          selectInput(
            inputId = "sample",
            label = "Select sample to display",
            choices = character(0),
            selected = character(0)
          ),
          
          # plotOutput(
          #   outputId = "methylationHistogram",
          #   inline = F,
          #   # width = "100%",
          #   height = "auto"
          # )  
        ), # close tabPanel Basic Statistics
      ) # close tabBox
    ) # close column
  ) # close fluidRow
) # close tabItem

