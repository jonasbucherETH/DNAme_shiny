tabItem(
  tabName = "tab-DMRseq",
  fluidRow( ### NOTE: 1 row has width = 12
    # column(
    #   width = 2,
      # box(
      #   title = "Parameters",
      #   width = NULL,
      #   solidHeader = TRUE,
      #   status = "primary",
      #   collapsible = TRUE,
      #   collapsed = FALSE,
      #   selectInput(
      #     inputId = "xFactorEmpiricalDistribution",
      #     label = "Select distribution",
      #     choices = c("M", "Cov"),
      #     selected = "M"
      #   ),
      #   # checkboxInput(
      #   #   inputId = "bySample",
      #   #   label = "Plot a separate line for each sample",
      #   #   value = FALSE
      #   # )
      #   tags$br("Plot a separate line for each sample"),
      #   switchInput(
      #     inputId = "bySample", 
      #     label = NULL,
      #     value = FALSE
      #   )
    #   ) # close box 
    # ), # close parameters column
    column(
      width = 8,
      bs4TabCard(
        # id = ns("tabBoxGreat"),
        id = NULL,
        # title = NULL,
        width = 12,
        maximizable = TRUE,
        status = "primary",
        solidHeader = T, # solid color background
        background = NULL, #  background color of the box
        headerBorder = T,
        selected = NULL,
        side = "left",
        type = "tabs",
        # dropdownMenu = boxDropdown(
        #   icon = shiny::icon("wrench"),
        #   cardDropdownItem(id = ns("downloadPlotGreat"), href = NULL, icon = shiny::icon("glyphicon-download-alt"))
        # ),
        tabPanel(
          title = "Empirical distribution",
          # width = NULL,
          # solidHeader = TRUE,
          # status = "primary",
          # collapsible = FALSE,
          # collapsed = FALSE,
          
          # use_waiter("waiterEmpiricalDistribution"),
          # waiterShowOnLoad(),
          # bs4Loading(),

          plotOutput(
            outputId = "empiricalDistributionPlot",
            inline = F,
            width = "100%"
          )
        ),
        # dropdownButton(
        #   tags$h3("Plot settings"),
        #   circle = TRUE,
        #   status = "danger",
        #   icon = icon("gear"), width = "250px",
        #   tooltip = tooltipOptions(title = "Click to see plot settings")
        sidebar = bs4CardSidebar(
          id = "sidebarDMRseq",
          width = 25, # Sidebar opening width in percentage
          background = "#333a40",
          startOpen = FALSE,
          icon = shiny::icon("gears"),
          easyClose = TRUE,
          selectInput(
            inputId = "xFactorEmpiricalDistribution",
            label = "Select distribution",
            choices = c("Methylation Proportion" = "M",
                        "Coverage" = "Cov"),
            selected = "M"
          ),
          tags$br("Plot a separate line for each sample"),
          switchInput(
            inputId = "bySample",
            label = NULL,
            value = FALSE
          )
        )
      ) # close box 
    ) # close plot column
  ) # close fluidRow
) # close tabItem

