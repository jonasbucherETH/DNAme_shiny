tabItem(
  # tabName = "tab-great",
  tabName = id,
  fluidRow( ### NOTE: 1 row has width = 12
    # column(
    #   width = 2,
    #   box(
    #     title = "Parameters",
    #     width = NULL,
    #     solidHeader = TRUE,
    #     status = "primary",
    #     collapsible = TRUE,
    #     collapsed = FALSE,
    #     selectInput(
    #       inputId = "xFactorVolcano",
    #       label = "Select factor for x-axis",
    #       choices = c("fold_enrichment", "z-score"),
    #       selected = "fold_enrichment"
    #     ),
    #     selectInput(
    #       inputId = "yFactorVolcano",
    #       label = "Select factor for y-axis",
    #       choices = c("p_value", "p_adjust"),
    #       selected = "p_value"
    #     ),
    #     # numericInput(
    #     #   inputId = "min_region_hits",
    #     #   label = "Minimum amount of region hits for inclusion",
    #     #   value = 5,
    #     #   min = 1,
    #     #   max = 100,
    #     #   step = 1,
    #     #   width = NULL
    #     # ),
    #     textInput(
    #       inputId = "titleVolcanoPlot",
    #       label = "Plot title",
    #       value = "Volcano Plot"
    #     )
    #   ) # close box
    # ), # close parameters column
    column(
      width = 12,
      tabBox(
        title = NULL,
        width = NULL,
        # sidebar = boxSidebar(
        #   id = "boxSidebar",
        #   background = "#808080",
        #   width = "50%",
        #   selectInput(
        #     inputId = "selectCollection",
        #     label = "Select collection to display",
        #     choices = c("BP", "CC", "MF"),
        #     selected = "BP"
        #   )
        # ),
        tabPanel(
          title = "Volcano Plot",
          width = NULL,
          solidHeader = TRUE,
          status = "primary",
          # collapsible = FALSE,
          # collapsed = FALSE,
          fluidRow(
              column(
                width = 10,
                plotlyOutput(
                  outputId = "volcanoPlot",
                  inline = F,
                  # width = "100%",
                  height = "auto"
                ),
              ),
              column(
                width = 2,
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
                textInput(
                  inputId = "titleVolcanoPlot",
                  label = "Plot title",
                  value = "Volcano Plot"
                )
              ) # close parameters column
          ) # close fluidRow within tabPanel
        ), # close tabPanel volcano
        tabPanel(
          title = "Region-Gene Associations Plot",
          width = NULL,
          solidHeader = TRUE,
          status = "primary",
          # collapsible = FALSE,
          # collapsed = FALSE,
          # selectInput(
          #   inputId = "selectTermID",
          #   label = "Select term",
          #   choices = "",
          #   selected = ""
          # ),
          
          # htmlOutput(outputId = "enrichment_table"),
          
          plotOutput(
            outputId = "associationsPlots"
            # inline = F
            # width = "100%"
          )
          
        ), # close tabPanel associations
        tabPanel(
          title = "Dot Plot",
          width = NULL,
          solidHeader = TRUE,
          status = "primary",
          # collapsible = FALSE,
          # collapsed = FALSE,
          
          plotOutput(
            outputId = "dotPlot",
            inline = F,
            # width = "100%",
            height = "auto"
          )  
        ), # close tabPanel volcano
        
      ) # close tabBox 
    ), # close plot column
  ), # close fluidRow 1
  fluidRow( # fluidRow 2
    column(
      width = 12,
      box(
        title = "Enrichment table",
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        collapsible = FALSE,
        collapsed = FALSE,
        br(), 
        checkboxGroupInput("columns_to_display1", 
            label = NULL, 
            choiceNames = character(0),
            choiceValues = character(0),
            # choices = NULL, 
            selected = character(0)
        ),
        br(),
        DT::dataTableOutput(
            outputId = "enrichmentTable")
      ) # close box 
    ) # close Enrichment table column
  ) # close fluidRow 2
) # close tabItem



