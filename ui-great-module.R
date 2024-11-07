greatUI <- function(id) {
  
  ns <- NS(id)
  # dashboardBody(
  #   # Output content for each output category
  # )
  tagList(
    fluidRow(
      column(
        width = 12,
        # tabsetPanel(
        bs4TabCard(
          id = ns("tabBoxGreat"),
          # id = NULL,
          # title = NULL,
          width = NULL,
          maximizable = TRUE,
          status = "primary",
          headerBorder = T,
          
          # selected = "Volcano Plot",
          selected = NULL,
          side = "left",
          type = "tabs",
          solidHeader = T, # solid color background
          background = NULL, #  background color of the box
          height = "800px",
          # dropdownMenu = boxDropdown(
          #   icon = shiny::icon("wrench"),
          #   cardDropdownItem(id = ns("downloadPlotGreat"), href = NULL, icon = shiny::icon("download-alt"))
          # ),
          sidebar = bs4CardSidebar(
            id = ns("sidebarGreatPlots"),
            width = 25, # Sidebar opening width in percentage
            background = "#333a40",
            startOpen = TRUE,
            icon = shiny::icon("gears"),
            easyClose = TRUE,
            conditionalPanel(
              condition = "input.tabBoxGreat == 'Enrichment Table'",
              ns = NS(id),
              prettyCheckboxGroup(
                ns("columns_to_display1"), 
                label = "Columns to display", 
                choices = character(0),
                selected = character(0),
                status = "primary"
              )
            ),
            conditionalPanel(
              condition = "input.tabBoxGreat == 'Volcano Plot'",
              ns = NS(id),
              selectInput(
                inputId = ns("xFactorVolcano"),
                label = "Select factor for x-axis",
                choices = c("fold_enrichment", "z-score"),
                selected = "fold_enrichment"
              ),
              selectInput(
                inputId = ns("yFactorVolcano"),
                label = "Select factor for y-axis",
                choices = c("p_value", "p_adjust"),
                selected = "p_value"
              ),
              textInput(
                inputId = ns("titleVolcanoPlot"),
                label = "Plot title",
                value = "Volcano Plot",
              )
            ), # close conditionalPanel volcanoPlot
            conditionalPanel(
              condition = "input.tabBoxGreat == 'Region-Gene Associations Plots'",
              ns = NS(id),
              selectizeInput(
                inputId = ns("selectizeTerm"), 
                label = "Select Term for associations", 
                choices = character(0), 
                selected = character(0), 
                multiple = FALSE,
                options = NULL
              )
            ), # close conditionalPanel Region-Gene Associations Plots
            conditionalPanel(
              condition = "input.tabBoxGreat == 'Dot Plot'",
              ns = NS(id),
              actionButton(
                inputId = ns("actionButtonDotPlot"), 
                label = "Generate dot plot with selected terms",
                icon = NULL, 
                width = NULL
              )
            ) # close conditionalPanel Dot Plot
          ), # close boxSidebar
          # .list = c(
          tabPanel(
            title = "Enrichment Table",
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            DT::dataTableOutput(
              outputId = ns("enrichmentTable")
            )
          ),
          tabPanel(
            title = "Volcano Plot",
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            # collapsible = FALSE,
            # collapsed = FALSE,
            # plotlyOutput(
            #   outputId = ns("volcanoPlot"),
            #   inline = F
            #   # width = "100%",
            #   # height = "auto"
            # )
            plotOutput(
              outputId = ns("volcanoPlot"),
              inline = F,
              width = "70%",
              height = "700px"
              # height = "auto"
            )
            
          ),
          tabPanel( # tabPanel associations
            title = "Region-Gene Associations Plots",
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            # collapsible = FALSE,
            # collapsed = FALSE,
            # htmlOutput(outputId = "enrichment_table"),
            plotOutput(
              outputId = ns("associationsPlots")
              # inline = F
              # width = "100%"
            )
          ), # close tabPanel associations
          
          tabPanel( # tabPanel dot plot
            title = "Dot Plot",
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            # collapsible = FALSE,
            # collapsed = FALSE,
            
            # plotOutput(
            # plotlyOutput(
            #   outputId = ns("dotPlot")
            #   # inline = F
            #   # width = "100%",
            #   # height = "auto"
            # )
            plotOutput(
              outputId = ns("dotPlot"),
              # hover = hoverOpts(id=ns("hoverDotPlot"))
              # inline = F
              width = "100%",
              height = "auto"
            )
            # verbatimTextOutput(
            #   outputId = "infoDotPlot", 
            #   placeholder = FALSE
            # )
          ) # close tabPanel dot plot
        ) # close tabBox 
      ) # close plot column
    ) # close fluidRow 1
    # fluidRow( # fluidRow 2
    #   column(
    #     width = 12,
    #     box(
    #       title = "Enrichment table",
    #       width = NULL,
    #       solidHeader = TRUE,
    #       status = "primary",
    #       collapsible = FALSE,
    #       collapsed = FALSE,
    #       br(), 
    # awesomeCheckboxGroup(ns("columns_to_display1"), 
    #                    label = NULL, 
    #                    # choiceNames = character(0),
    #                    # choiceValues = character(0),
    #                    choices = character(0),
    #                    selected = character(0),
    #                    status = "primary"
    # ),
    # checkboxGroupButtons(ns("columns_to_display1"), 
    #                      label = "Columns to display", 
    #                      # choiceNames = character(0),
    #                      # choiceValues = character(0),
    #                      choices = character(0),
    #                      selected = character(0),
    #                      # checkIcon = list(
    #                      #   yes = tags$i(class = "fa fa-check-square", 
    #                      #                style = "color: steelblue"),
    #                      #   no = tags$i(class = "fa fa-square-o", 
    #                      #               style = "color: steelblue")),
    #                      checkIcon = list(
    #                        yes = icon("ok",
    #                                   lib = "glyphicon")),
    #                      size = "sm"
    # ),
    # br(),
    # DT::dataTableOutput(
    #   outputId = ns("enrichmentTable")
    # )
    #     ) # close box 
    #   ) # close Enrichment table column
    # ) # close fluidRow 2
  ) # close dashboardBody
}