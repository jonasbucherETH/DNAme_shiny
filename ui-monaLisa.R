monaLisaUI <- function(id) {
  
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        # tabsetPanel(
        bs4TabCard(
          id = ns("tabBoxMonaLisa"),
          # id = NULL,
          # title = NULL,
          width = NULL,
          maximizable = TRUE,
          status = "primary",
          headerBorder = T,
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
          tabPanel(
            title = "Heatmap",
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            plotOutput(
              outputId = ns("heatmap_motifs"),
              inline = F,
              width = "100%",
              # height = "700px"
              height = "auto"
            )
          ),
          sidebar = bs4CardSidebar(
            id = ns("sidebarmonaLisa"),
            width = 25, # Sidebar opening width in percentage
            background = "#333a40",
            startOpen = TRUE,
            icon = shiny::icon("gears"),
            easyClose = TRUE
            # conditionalPanel(
            #   condition = "input.tabBoxMonaLisa == ''",
            #   ns = NS(id),
          )
        )
      )
    )
  ) # close dashboardBody
}