tabItem(
  tabName = "tab-Overview",
  fluidRow( ### NOTE: 1 row has width = 12
    column(
      width = 4,
      bs4Card(
        title = "Info & summary",
        width = NULL,
        id = "summaryBox",
        status = "primary",
        headerBorder = T,
        solidHeader = T,
        tableOutput("settingsTable"),
        br(),
        h5("Result summary")
        # verbatimTextOutput("cytosineContext")
        # tableOutput("summaryTable")
      ) # close box
    ), # close column
    column(
      width = 3,
      bs4Card(
        title = "Dataset info",
        width = NULL,
        id = "datasetInfoBox",
        status = "primary",
        headerBorder = T,
        solidHeader = T,
        tableOutput("samplesTable")
      )
    ), # close column (Dataset info)
    column(
      width = 5,
      bs4Card(
        title = "Color settings",
        width = NULL,
        id = "displaySettingsBox",
        status = "primary",
        headerBorder = T,
        solidHeader = T,
        # h3("Select color palette for groups"),
        # br(),
        fluidRow(
          # h4("Select color palette for groups"),
          column(
            width = 3,
            # br(),
            # h3("Select color palette for continuous variables"),
            radioGroupButtons(
              inputId = "colorPaletteDiverging",
              label = "Continuous variables",
              choices = "RdYlBu",
              selected = "RdYlBu",
              direction = "vertical",
              status = "default",
              size = "normal"
              # individual = TRUE
            ),
            br(),br(),br(),
            # h3("Select color palette for groups"),
            radioGroupButtons(
              inputId = "colorPalette",
              label = "Groups",
              choices = "Dark2",
              selected = "Dark2",
              direction = "vertical",
              status = "default",
              size = "normal"
              # individual = TRUE
            )
          ),
          column(
            width = 8,
            plotOutput(
              outputId = "colorPalettesDivergingPlot",
              inline = F,
              width = "100%",
              height = "40vh"
              # height = "auto"
            ),
            plotOutput(
              outputId = "colorPalettesCategoricalPlot",
              inline = F,
              width = "100%",
              height = "30vh"
              # height = "auto"
            )#,
          )
        ),
        lapply(1:6, function(i) {
          colourpicker::colourInput(
            inputId = paste0("GroupColour", i),
            label = "",
            value =  brewer.pal(10, "Dark2")[i],
            palette = "square",
            closeOnClick = TRUE,
            returnName = TRUE
          )
        })
        
        # radioGroupButtons(
        #   inputId = "colorPalette",
        #   label = "Select color palette for groups",
        #   choices = "Dark2",
        #   selected = "Dark2",
        #   direction = "vertical",
        #   status = "default",
        #   individual = TRUE
        # ),
        # plotOutput(
        #   outputId = "colorPalettesPlot",
        #   inline = F,
        #   width = "100%"
        #   # height = "auto"
        # )#,
        # materialSwitch(
        #   inputId = "manualColors", 
        #   label = "Select group colors manually", 
        #   status = "primary",
        #   value = FALSE, 
        #   width = NULL
        # ) #,
        # conditionalPanel(
        #   condition = "input.manualColors == true",
        #   selectizeInput(
        #     inputId = "selectizeColors",
        #     label = "",
        #     choices = character(0),
        #     selected = character(0),
        #     multiple = TRUE
        #   ),
          # uiOutput('colorPanel'),
        #   bs4Dash::actionButton(
        #     inputId = "actionButtonColors",
        #     label = "Apply colors",
        #     icon = NULL,
        #     # style = "unite",
        #     # color = "default",
        #     size = "sm"
        #   )
        # ) # close conditionalPanel
      ) # close box
    ) # close column
  ) # close fluidRow
) # close tabItem

