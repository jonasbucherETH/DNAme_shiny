tabItem(
  tabName = "tab-HOMER",
  fluidRow( ### NOTE: 1 row has width = 12
    column(
      width = 10,
      bs4TabCard(
        width = 12,
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
          title = "Motif enrichment",
          width = 10,
          solidHeader = TRUE,
          status = "primary"
          # DT::dataTableOutput(
          #   outputId = "motifTable"
          # )
        ),
        tabPanel(
          title = "Heatmap of Motifs",
          width = 10,
          solidHeader = TRUE,
          status = "primary",
          # plotOutput(
          #   outputId = "heatmapMotifs"
          # )
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

