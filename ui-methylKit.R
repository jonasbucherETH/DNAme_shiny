tabItem(
  tabName = "tab-methylKit",
  fluidRow( ### NOTE: 1 row has width = 12
    column(
      width = 12,
      bs4TabCard(
        width = 12,
        # id = NULL,
        id = "methylKitTabCard",
        # title = NULL,
        maximizable = TRUE,
        status = "primary",
        headerBorder = T,
        selected = NULL,
        side = "left",
        type = "tabs",
        ###
        solidHeader = T, # solid color background
        background = NULL, #  background color of the box
        sidebar = bs4CardSidebar(
          id = "sidebarMethylKit",
          width = 25, # Sidebar opening width in percentage
          background = "#333a40",
          startOpen = TRUE,
          icon = shiny::icon("gears"),
          easyClose = TRUE,
          conditionalPanel(
            condition = "input.methylKitTabCard == 'Methylation Statistics'",
            selectInput(
              inputId = "sampleHistograms",
              label = "Select sample to display",
              choices = character(0),
              selected = character(0)
            )
          ), # close conditionalPanel Methylation Statistics
          conditionalPanel(
            condition = "input.methylKitTabCard == 'PCA of samples'",
            # pickerInput(
            #   inputId = "pickFactorsPCA",
            #   label = "Select PCs to plot", 
            #   choices = character(0),
            #   multiple = TRUE,
            #   options =  list("max-options" = 2)
            # ),
            # verbatimTextOutput(outputId = "res_classic"),
            selectInput(
              inputId = "pickFactor1PCA",
              label = "Select PC for x-axis",
              choices = character(0),
              selected = character(0)
            ),
            selectInput(
              inputId = "pickFactor2PCA",
              label = "Select PC for y-axis",
              choices = character(0),
              selected = character(0)
            ),
            textInput(
              inputId = "pcaTitle",
              label = "Title of plot",
              value = "PCA plot of samples"
            ),
            checkboxInput(
              inputId = "sampleLabelsPCA",
              label = "Display sample labels",
              value = TRUE
            ),
            numericInput(
              inputId = "pointSizePCA",
              label = "Point size", min = 1, max = 6,
              value = 3, step = 0.5,
              # width = "100px"
            ),
            numericInput(
              inputId = "textSizePCA",
              label = "Font Size", min = 4, max = 30,
              value = 12, step = 0.5,
              # width = "100px"
            )
            # selectInput(
            #   inputId = "colorPalettePCA",
            #   label = "Select color palette for PCA",
            #   choices = character(0),
            #   selected = character(0)
            # ),
            # plotOutput(
            #   outputId = "colorPalettesPlot",
            #   inline = F,
            #   width = "100%"
            #   # height = "auto"
            # ),
            # materialSwitch(
            #   inputId = "manualColoursPCA", 
            #   label = "Select colours manually", 
            #   status = "primary",
            #   value = FALSE, 
            #   width = NULL
            # ),
            # conditionalPanel(
            #   condition = "input.manualColoursPCA == true",
            #   selectizeInput(
            #     inputId = "selectizeColoursPCA",
            #     label = "",
            #     choices = character(0),
            #     selected = character(0),
            #     multiple = TRUE
            #   ),
            #   uiOutput('colourPanelPCA'),
            #   bs4Dash::actionButton(
            #     inputId = "actionButtonColours",
            #     label = "Apply colours",
            #     icon = NULL,
            #     # style = "unite",
            #     # color = "default",
            #     size = "sm"
            #   )
            # )
          ) # close conditionalPanel PCA of samples
        ), # close sidebar
            
        tabPanel(
          title = "Methylation Statistics",
          width = NULL,
          solidHeader = TRUE,
          # status = "primary",
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
        ), # close tabPanel Methylation Statistics
        tabPanel(
          title = "PCA of samples",
          width = NULL,
          solidHeader = TRUE,
          status = "primary",
          dropdown(
            style = "unite", icon = icon("gear"),
            status = "danger", width = "300px",
            animate = animateOptions(
              enter = animations$fading_entrances$fadeInLeftBig,
              exit = animations$fading_exits$fadeOutRightBig
            ),
            tags$h3("Colour options"),
            
            selectInput(
              inputId = "colorPalettePCA",
              label = "Select color palette for PCA",
              choices = character(0),
              selected = character(0)
            ),
            plotOutput(
              outputId = "colorPalettesPlot",
              inline = F,
              width = "100%"
              # height = "auto"
            ),
            materialSwitch(
              inputId = "manualColoursPCA", 
              label = "Select colours manually", 
              status = "primary",
              value = FALSE, 
              width = NULL
            ),
            conditionalPanel(
              condition = "input.manualColoursPCA == true",
              selectizeInput(
                inputId = "selectizeColoursPCA",
                label = "",
                choices = character(0),
                selected = character(0),
                multiple = TRUE
              ),
              uiOutput('colourPanelPCA'),
              bs4Dash::actionButton(
                inputId = "actionButtonColours",
                label = "Apply colours",
                icon = NULL,
                # style = "unite",
                # color = "default",
                size = "sm"
              )
            )
          ), # close drowpdown
          plotOutput(
            outputId = "pcaPlot",
            inline = F,
            width = "100%"
            # height = "auto"
          ),
          plotOutput(
            outputId = "pcaScree",
            inline = F,
            width = "100%"
            # height = "auto"
          )
          # DT::dataTableOutput("pcaLoadings")
          
        ) # close tabPanel PCA
      ) # close tabBox
    ) # close column
  ) # close fluidRow
) # close tabItem

