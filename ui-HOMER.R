tabItem(
  tabName = "tab-HOMER",
  fluidRow( ### NOTE: 1 row has width = 12
    column(
      width = 12,
      bs4TabCard(
        width = NULL,
        # id = NULL,
        id = "homerTabCard",
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
        tabPanel(
          title = "Known motifs",
          width = NULL,
          solidHeader = TRUE,
          status = "primary"
          # DT::dataTableOutput(
          #   outputId = "motifTable"
          # )
        ),
        tabPanel(
          title = "de novo motifs",
          width = NULL,
          solidHeader = TRUE,
          status = "primary",
          # plotOutput(
          #   outputId = "heatmapMotifs"
          # )
          
          # actionButton(inputId='linkDeNovoMotifs', label="open de novo motif results viewer", 
          #                     icon = icon("th"), 
          #                     onclick ="window.open('http://google.com', '_blank')"
          # ),
          uiOutput("linkDeNovoMotifs"),
          InteractiveComplexHeatmapOutput(
            heatmap_id = "heatmapMotifs",
            compact = TRUE, # = no sub-heatmap for selected part
            title1 = "",
            # title2 = "", # only when compact = F
            width1 = 500,
            height1 = 700,
            action = "hover",
            response = "hover"
          )
        )

      ) # close tabCard
    ) # close column
  ) # close fluidRow
) # close tabItem

