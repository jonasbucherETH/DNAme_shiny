tabItem(
  tabName = "tab-Overview",
  fluidRow( ### NOTE: 1 row has width = 12
    column(
      width = 3,
      bs4Card(
        title = "General settings",
        width = NULL,
        id = "settingsBox",
        status = "primary",
        headerBorder = T,
        solidHeader = T,
        radioGroupButtons(
          inputId = "selectContext",
          label = "Cytosine context",
          choices = "CpG",
          status = "primary"
        ),
        radioGroupButtons(
          inputId = "selectMetyhlation",
          label = "Methylation",
          choices = c("Hypo", "Hyper", "All"),
          status = "primary"
        )
        # verbatimTextOutput("cytosineContext")
        # tableOutput("summaryTable")
      ), # close box/card
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
      ) # close box/card
    ), # close column
    # column(
    #   width = 3,
    #   bs4Card(
    #     title = "Dataset info",
    #     width = NULL,
    #     id = "datasetInfoBox",
    #     status = "primary",
    #     headerBorder = T,
    #     solidHeader = T,
    #     tableOutput("samplesTable")
    #   )
    # ), # close column (Dataset info)
    column(
      width = 9,
      bs4Card(
        title = "Plot Color settings",
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
            width = 7,
            # br(),
            # h3("Select color palette for continuous variables"),
            switchInput(
              inputId = "colorblind_only",
              label = "colorblind-friendly",
              labelWidth = "150px",
              value = TRUE
            )
          ),
          column(
            width = 5,
            actionBttn(
              inputId = "confirm_color_selection",
              label = "Confirm color selection",
              block = TRUE,
              style = "material-flat",
              color = "primary"
            )
          )
        ),
        fluidRow(
          # h4("Color palette group variables"),
          tags$style(HTML("
              .btn-custom-class {
                border: none;
                box-shadow: none;
                padding: 0;
                width: 40px;
                height: 40px;
                margin: 2px;
              }
              
              .btn-custom-class.active {
                box-shadow: 0px 0px 5px 2px #666;
                position: relative;
              }
              
              .btn-custom-class.active .checkmark {
                position: absolute;
                top: 2px;
                left: 2px;
                color: white;
                font-size: 1.2em;
              }
              
              .checkmark {
                display: none;
              }
              
              .btn-custom-class.active .checkmark {
                display: block;
              }
              
              /* Custom class for radioGroupButtons */
              .btn-radio-class {
                display: block;
                border: none;
                box-shadow: none;
                padding: 0;
                width: 70px;
                height: 40px;
                color: white;
                background-color: #0275d8;
                margin: 17px;
              }
              
              /* Active button styling */
              .btn-radio-class.active {
                background-color: #0056b3; /* Custom active background color */
                position: relative;
                box-shadow: 0px 0px 5px 2px #666;
              }
              
              h5.disabled {
                opacity: 0.5;  /* Set opacity to 70% */
              }
              
          ")), # end of style custom
          
          # br(),br(),
          # column(
          #   width = 3,
          #   radioGroupButtons(
          #     inputId = "colorPaletteDiverging",
          #     label = "Select Continuous Color Scale",
          #     choices = "RdPu",
          #     selected = "RdPu",
          #     direction = "vertical",
          #     status = "default",
          #     size = "normal"
          #     # individual = TRUE
          #   )
          # ),
          # column(
          #   width = 2,
          #   uiOutput("palette_radio_buttons_groups", inline = TRUE)
          # ),
          column(
            width = 12, 
            uiOutput("color_buttons_groups")
          )
          # column(
          #   width = 5,
          #   plotOutput(
          #     outputId = "colorPalettesDivergingPlot",
          #     inline = F,
          #     width = "100%",
          #     height = "40vh"
          #     # height = "auto"
          #   )
          # )
        ), # end fluidRow diverging
        # fluidRow(
        #   # h4("groups"),
        #   column(
        #     width = 3,
        #     # h3("Select color palette for groups"),
        #     radioGroupButtons(
        #       inputId = "colorPalette",
        #       label = "Groups",
        #       choices = "Dark2",
        #       selected = "Dark2",
        #       direction = "vertical",
        #       status = "default",
        #       size = "normal"
        #       # individual = TRUE
        #     )
        #   ),
        #   column(
        #     width = 5,
        #     plotOutput(
        #       outputId = "colorPalettesCategoricalPlot",
        #       inline = F,
        #       width = "100%",
        #       height = "30vh"
        #       # height = "auto"
        #     )#,
        #   )
        # ),
        fluidRow(
          column(
            width = 7,
            h4("Assign colours to groups and samples")
          ),
          column(
            width = 5,
            actionBttn(
              inputId = "confirm_color_assignment",
              label = "Confirm color assignment",
              block = TRUE,
              style = "material-flat",
              color = "primary"
            )
          )
        ),
        fluidRow(
          # First column: inputs 1 to 4
          column(
            width = 6,
            lapply(1:4, function(i) {
              colourpicker::colourInput(
                inputId = paste0("GroupColour", i),
                label = "",
                value = brewer.pal(8, "Dark2")[i],
                palette = "square",
                closeOnClick = TRUE,
                returnName = TRUE
              )
            })
          ),
          
          # Second column: inputs 5 to 8
          column(
            width = 6,
            lapply(5:8, function(i) {
              colourpicker::colourInput(
                inputId = paste0("GroupColour", i),
                label = "",
                value = brewer.pal(8, "Dark2")[i],
                palette = "square",
                closeOnClick = TRUE,
                returnName = TRUE
              )
            })
          )
        )
        
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

