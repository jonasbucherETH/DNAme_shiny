observe({
  all_motifs <- inputDataReactive()$all_motifs
  # motifPlots <- inputDataReactive()$motifPlots
  
  # # Define function to read SVG files and convert to grob object
  # svg_to_grob <- function(file_path) {
  #   svg_content <- read_svg(file_path)
  #   grob <- as_grob(svg_content)
  #   return(grob)
  # }
  # 
  # motifPlots <- c(
  #   "~/git/DNAme_shiny/data/homerResults/motif10.logo.svg",
  #   "~/git/DNAme_shiny/data/homerResults/motif11.logo.svg",
  #   "~/git/DNAme_shiny/data/homerResults/motif12.logo.svg"
  # )
  # 
  # plot_grobs <- lapply(motifPlots, svg_to_grob)
  # 
  # # Create a data.table with a column containing SVG images
  # dt <- data.table(
  #   x = c(1, 2, 3),
  #   y = c(4, 5, 6),
  #   plot = plot_grobs
  #   # plot = c(7,8,9)
  #   
  # )
  # 
  # # Render the data.table using DT::renderDataTable
  # output$motifTable <- DT::renderDataTable({
  #   # Set options for the table
  #   options <- list(
  #     pageLength = 5,
  #     # Set escape to FALSE to prevent HTML escaping of the cell contents
  #     escape = FALSE,
  #     # Use custom rendering function to display images
  #     columnDefs = list(
  #       list(
  #         targets = c(3), # Column containing SVG images
  #         render = JS(
  #           "function(data, type, row, meta) {",
  #           "  if (type === 'display') {",
  #           "    // Return img tag with source set to SVG file",
  #           "    return HTML('<img src=\"data:image/svg+xml;base64,'+btoa(data)+'\"/>');",
  #           "  } else {",
  #           "    return data;",
  #           "  }",
  #           "}"
  #         )
  #       )
  #     )
  #   )
  #   # Create the datatable object
  #   DT::datatable(dt, options = options)
  # })
  
  
  ######################## heatmap ########################
  # Define the heatmap
  motifs_pfm <- homerToPFMatrixList(filename = "~/data/homer/test1/homerMotifs.all.motifs", n = 100L)
  all_motifs_mat <- as.matrix(all_motifs[, c("log_p_value", "fold_enrichment")])
  all_motifs_mat[all_motifs_mat[,2]=="Inf", 2] <- 50
  rownames(all_motifs_mat) <- all_motifs$motif_name
  p_value_mat <- all_motifs_mat[,1]
  fold_enrichment_mat <- all_motifs_mat[,2]
  
  maxwidth <- max(vapply(TFBSTools::Matrix(motifs_pfm), ncol, 0L))
  grobL <- lapply(motifs_pfm, monaLisa::seqLogoGrob, xmax = maxwidth, xjust = "center")
  width.seqlogo = 1.5
  hmSeqlogo <- ComplexHeatmap::HeatmapAnnotation(logo = annoSeqlogo(grobL = grobL, 
                                                                    which = "row", space = unit(0.1, "mm"), width = unit(width.seqlogo, 
                                                                                                                         "inch")), show_legend = FALSE, show_annotation_name = FALSE, 
                                                 which = "row")
  myPalette <- colorRampPalette(colors = viridis::plasma(n = 7)[c(2, 4, 6)])
  
  ht1 <- ComplexHeatmap::Heatmap(
    p_value_mat, 
    name = "log(P value)", 
    left_annotation = hmSeqlogo,
    cluster_rows = FALSE,
    show_row_names = FALSE,
    col = myPalette(n=3),
    # column_title_side = "top",
    column_names_side = "top",
    column_names_rot = 0,
    column_names_centered = TRUE
  )
  
  ht2 <- ComplexHeatmap::Heatmap(
    fold_enrichment_mat, 
    name = "Fold Enrichment", 
    show_row_names = FALSE, 
    col = myPalette(n=3),
    # column_title_side = "top"
    column_names_side = "top",
    column_names_rot = 0,
    column_names_centered = TRUE
  )
  
  ht_list <- ht1 + ht2
  
  ht_full <- ComplexHeatmap::draw(ht_list,
                                   # column_title = "Motif Enrichment Heatmap", column_title_gp = gpar(fontsize = 16),
                                   column_sub_title_side = "top",
                                   ht_gap = unit(5, "pt"), main_heatmap = 1,
                                   row_gap = unit(5, "pt"))
  
  makeInteractiveComplexHeatmap(input, output, session, ht_full,
                                                        heatmap_id = "heatmapMotifs",
                                                        click_action = NULL, hover_action = NULL,
                                                        dblclick_action = NULL, brush_action = NULL, res = 72,
                                                        show_cell_fun = TRUE, show_layer_fun = TRUE)
  


  
  # observeEvent( # Event 
  #   {
  #     input$sample
  #   },
  #   ignoreInit = F, # If TRUE, then, when the eventified object is first created/initialized, don't trigger the action or (compute the value). The default is FALSE.
  #   ignoreNULL = T, # default = TRUE
  #   {
  #     
  #   }
  # ) # close event
  
  
}) # close observe
