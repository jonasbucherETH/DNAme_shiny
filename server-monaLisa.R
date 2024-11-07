monaLisaServer <- function(id, summarizedExp) {
  moduleServer(
    id,
    function(input, output, session) {
      # myPalette <- inputDataReactive()$myPalette
      # sampleNames <- inputDataReactive()$sampleNames
      
      # dmRegions <- dmrseq_rds$dmRegions
      # significantRegions <- dmrseq_rds$significantRegions
      
      # dataset <- inputDataReactive()$dataset
      # factorNames <- inputDataReactive()$factorNames
      # factorLevels <- inputDataReactive()$factorLevels
      # factors <- inputDataReactive()$factors
      # colourList <- inputDataReactive()$colourList
      # testCovariate <- inputDataReactive()$testCovariate
      
      print("###---### monaLisa - Reached checkpoint 1 ###---###")
      
      ### TODO: sidebar
      # updateCardSidebar("sidebarmonaLisa")
      # observeEvent(input$monaLisaTabCard, {
      #   updateCardSidebar("sidebarmonaLisa")
      # })
      
      # observe({
      #   
      #   bindEvent(input$list_selection, {
      #     selected_list <- returnList[[input$list_selection]]
      #   })
      # })
      se_sel <- apply(assay(summarizedExp, "negLog10Padj"), 1, 
                    function(x) max(abs(x), 0, na.rm = TRUE))
      
      order_p <- se_sel %>% order(decreasing = T)
      
      n_motifs <- 10
      # se_sel[order_p][1:n_motifs]
      
      # top10_p <- apply(assay(se, "negLog10Padj"), 1, 
      #                  function(x) max(abs(x), 0, na.rm = TRUE))
        
      heatmap_motifs <- plotMotifHeatmaps(
          x = summarizedExp[se_sel[order_p][1:n_motifs],],
          which.plots = c("log2enr", "negLog10Padj"), 
          width = 4,
          cluster = TRUE,
          highlight = NULL,
          show_dendrogram = FALSE,
          show_motif_GC = FALSE,
          width.seqlogo = 1.5,
          use_raster = FALSE,
          na_col = "white",
          doPlot = FALSE,
          col.enr = c("#053061", "#2166AC", "#4393C3", "#92C5DE", "#D1E5F0", "#F7F7F7",
                      "#FDDBC7", "#F4A582", "#D6604D", "#B2182B", "#67001F"),
          col.sig = c("#F0F0F0", "#D9D9D9", "#BDBDBD", "#969696", "#737373", "#525252",
                      "#252525", "#000000"),
          col.gc = c("#F7FCF5", "#E5F5E0", "#C7E9C0", "#A1D99B", "#74C476", "#41AB5D", "#238B45",
                     "#006D2C", "#00441B"),
          # maxEnr = 2, maxSig = 10,
          maxEnr = NULL, maxSig = NULL,
          show_seqlogo = TRUE)
      
      output$heatmap_motifs <- renderPlot({
        heatmap_motifs
      })

    }
  )
}
