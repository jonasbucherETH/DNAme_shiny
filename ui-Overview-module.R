overviewUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 3,
        bs4Card(
          title = "General settings",
          width = NULL,
          id = ns("settingsBox"),
          status = "primary",
          headerBorder = TRUE,
          solidHeader = TRUE,
          radioGroupButtons(
            inputId = ns("selectContext"),
            label = "Cytosine context",
            choices = "CpG",
            status = "primary"
          ),
          radioGroupButtons(
            inputId = ns("selectMetyhlation"),
            label = "Methylation",
            choices = c("Hypo", "Hyper", "All"),
            status = "primary"
          )
        ), # close box/card
        bs4Card(
          title = "Info & summary",
          width = NULL,
          id = ns("summaryBox"),
          status = "primary",
          headerBorder = TRUE,
          solidHeader = TRUE,
          tableOutput(ns("settingsTable")),
          br(),
          h5("Result summary")
        ) # close box/card
      ), # close column
      column(
        width = 9,
        bs4Card(
          title = "Plot Color settings",
          width = NULL,
          id = ns("displaySettingsBox"),
          status = "primary",
          headerBorder = TRUE,
          solidHeader = TRUE,
          fluidRow(
            column(
              width = 7,
              switchInput(
                inputId = ns("colorblind_only"),
                label = "Colorblind-friendly",
                labelWidth = "150px",
                value = TRUE
              )
            ),
            column(
              width = 5,
              actionBttn(
                inputId = ns("confirm_color_selection"),
                label = "Confirm color selection",
                block = TRUE,
                style = "material-flat",
                color = "primary"
              )
            )
          ),
          fluidRow(
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
            column(
              width = 12, 
              uiOutput(ns("color_buttons_groups"))
            )
          ), # end fluidRow
          fluidRow(
            column(
              width = 7,
              h4("Assign colours to groups and samples")
            ),
            column(
              width = 5,
              actionBttn(
                inputId = ns("confirm_color_assignment"),
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
                  inputId = ns(paste0("GroupColour", i)),
                  label = "",
                  value = RColorBrewer::brewer.pal(8, "Dark2")[i],
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
                  inputId = ns(paste0("GroupColour", i)),
                  label = "",
                  value = RColorBrewer::brewer.pal(8, "Dark2")[i],
                  palette = "square",
                  closeOnClick = TRUE,
                  returnName = TRUE
                )
              })
            )
          )
        ) # close box
      ) # close column
    ) # close fluidRow
  )
}