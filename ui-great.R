tabItem(
  tabName = "tab-great",
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
          inputId = "xFactorVolcano",
          label = "Select factor for x-axis",
          choices = c("fold_enrichment", "z-score"),
          selected = "fold_enrichment"
        ),
        selectInput(
          inputId = "yFactorVolcano",
          label = "Select factor for y-axis",
          choices = c("p_value", "p_adjust"),
          selected = "p_value"
        ),
        numericInput(
          inputId = "min_region_hits",
          label = "Minimum amount of region hits for inclusion",
          value = 5,
          min = 1,
          max = 100,
          step = 1,
          width = NULL
        ),
        textInput(
          inputId = "titleVolcanoPlot",
          label = "Plot title",
          value = "Volcano Plot"
        )
      ) # close box 
    ), # close parameters column
    column(
      width = 6,
      box(
        title = "volcanoPlot",
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        collapsible = FALSE,
        collapsed = FALSE,
        
        plotOutput(
          outputId = "volcanoPlot",
          inline = F,
          width = "100%"
        )
      ) # close box 
    ), # close plot column
    column(
      width = 4,
      box(
        title = "Enrichment table",
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        collapsible = FALSE,
        collapsed = FALSE,
        
        DT::dataTableOutput("mytable")
      ) # close box 
    ), # close Enrichment table column
    
  ) # close fluidRow
) # close tabItem

