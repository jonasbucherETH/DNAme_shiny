tabItem(
  tabName = "tab-DMRseq",
  fluidRow( ### NOTE: 1 row has width = 12
    column(
      width = 2,
      box(
        title = "Parameters",
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,
        collapsed = FALSE,
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
      ) # close box 
    ), # close parameters column
    column(
      width = 6,
      box(
        title = "Empirical distribution plot",
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        collapsible = FALSE,
        collapsed = FALSE,
        
        plotOutput(
          outputId = "empiricalDistributionPlot",
          inline = F,
          width = "100%"
        )
      ) # close box 
    ) # close plot column
  ) # close fluidRow
) # close tabItem

