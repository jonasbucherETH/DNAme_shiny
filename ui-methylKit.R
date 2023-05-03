tabItem(
  tabName = "tab-methylKit",
  fluidRow( ### NOTE: 1 row has width = 12
    column(
      width = 10,
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
        
        # bs4CardSidebar(
        #   id = "sidebarMethylKit",
        #   width = 50,
        #   background = "#333a40",
        #   startOpen = T,
        #   # icon = shiny::icon("gears"),
        #   easyClose = TRUE,
        #   uiOutput("moreControls")
        # ),
        
        tabPanel(
          title = "Methylation Statistics",
          width = NULL,
          solidHeader = TRUE,
          # status = "primary",
          fluidRow(
            column(
              width = 5,
              plotOutput(
                outputId = "methylationHistogram",
                inline = F,
                width = "100%"
                # height = "auto"
              )
            ),
            column(
              width = 5,
              plotOutput(
                outputId = "coverageHistogram",
                inline = F,
                width = "100%"
                # height = "auto"
              )
            ),
            column(
              width = 2,
              selectInput(
                inputId = "sample",
                label = "Select sample to display",
                choices = character(0),
                selected = character(0)
              )
            )
          )
        ), # close tabPanel Methylation Statistics
        tabPanel(
          title = "PCA of samples",
          width = 10,
          solidHeader = TRUE,
          status = "primary",
          plotOutput(
            outputId = "pcaPlot",
            inline = F,
            width = "100%"
            # height = "auto"
          ),
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
          ),
          # input$sampleLabelsPCA
          # input$textSizePCA
          # input$pointSizePCA
          # input$pcaTitle
          
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
          checkboxInput(
            inputId = "manualColoursPCA", 
            label = "Select colours manually", 
            value = FALSE, 
            width = NULL
          ),
          selectizeInput(
            inputId = "selectizeColoursPCA",
            label = "",
            choices = character(0),
            selected = character(0),
            multiple = TRUE
          ),
          uiOutput('colourPanelPCA'),
        ) # close tabPanel PCA
      ) # close tabBox
    ) # close column
  ) # close fluidRow
) # close tabItem

