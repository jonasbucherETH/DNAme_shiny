observe({
  # dmRegions <- inputDataReactive()$dmRegions
  # significantRegions <- inputDataReactive()$significantRegions
  greatResult <- inputDataReactive()$greatResult
  enrichmentTable <- inputDataReactive()$enrichmentTable
  
  table <- enrichmentTable
  
  # testCovariate <- "Treatment"
  
  observeEvent( # Event number 1
    {
      # input$min_region_hits
      input$xFactorVolcano
      input$yFactorVolcano
      input$titleVolcanoPlot
    },
    ignoreInit = F, # If TRUE, then, when the eventified object is first created/initialized, don't trigger the action or (compute the value). The default is FALSE.
    ignoreNULL = T, # default = TRUE
    {
      
      min_region_hits <- 5
      # min_region_hits <- input$min_region_hits
      # table <- getEnrichmentTable(greatResult, min_region_hits)
      genome_fraction <- table$genome_fraction
      observed_region_hits <- table$observed_region_hits
      fold_enrichment <- table$fold_enrichment
      p_value <- table$p_value
      p_adjust <- table$p_adjust
      n <- greatResult@n_total
      z_score = (observed_region_hits - n*genome_fraction)/sqrt(n*genome_fraction*(1-genome_fraction))
      
      col_fun = colorRamp2(seq(min_region_hits, min_region_hits + min(50, max(observed_region_hits)), length = 11), rev(brewer.pal(11, "Spectral")))
      p <- 0.9
      a <- 0.5
      # size_fun = function(x) (10^x)/2
      # size_fun_rev = function(x) log((2*x), base = 10)
      size_fun = function(x) x^p + a
      size_fun_rev = function(x) (x-a)^(1/p)
      # size_fun = function(x) x^0.3*2 + 0.1
      # size_fun_ggplot2 <- function(x) scales::sqrt_trans()(x)^0.3*2 + 0.1
      
      
      if(input$xFactorVolcano == "fold_enrichment") {
        x = log2(fold_enrichment)
        xlab = "log2 fold enrichment: log2(obs/exp)"
      } else {
        x = z_score
        xlab = "z-score: (obs-exp)/sd"
      }
      
      if(input$yFactorVolcano == "p_value") {
        y = -log10(p_value)
        ylab = "-log10(p-value)"
      } else {
        y = -log10(p_adjust)
        ylab = "-log10(p-adjust)"
      }
      
      y[is.infinite(y)] = max(y[is.finite(y)])
      
      l = observed_region_hits >= min_region_hits
      if(!any(l)) {
        plot(NULL, xlim = c(0, 1), ylim = c(0, 1), ann = FALSE, axes = FALSE)
        text(0.5, 0.5, qq("No term left with min_region_hits >= @{min_region_hits}"))
        return(invisible(NULL))
      }
      
      
      
      volcanoPlot <- ggplot(data.frame(x = x[l], y = y[l], observed_region_hits = observed_region_hits[l], genome_fraction = genome_fraction[l]), aes(x = x, y = y))
      
      volcanoPlot <- volcanoPlot +
        # geom_point(aes(color = observed_region_hits, size = size_fun_ggplot2(genome_fraction)), shape = 16) +
        geom_point(aes(color = observed_region_hits, size = size_fun(genome_fraction)), shape = 16) +
        # scale_color_gradientn(colors = rev(brewer.pal(11, "Spectral")), limits = c(min_region_hits, min_region_hits + min(50, max(observed_region_hits)))) +
        scale_color_gradientn(colors = rev(brewer.pal(11, "Spectral")), limits = c(min_region_hits, min_region_hits + min(50, max(observed_region_hits)))) +
        # scale_size_continuous(breaks = seq(min(genome_fraction), max(genome_fraction), length = 5)^0.5*1.5 + 0.5, labels = paste0(sprintf("%.2f", round(seq(min(genome_fraction), max(genome_fraction), length = 5)*100)), "%")) +
        # scale_size_continuous(breaks = (seq(min(genome_fraction), max(genome_fraction), length = 5)*10)^0.5, labels = paste0(sprintf("%.2f", round(seq(min(genome_fraction), max(genome_fraction), length = 5)*100)), "%")) +
        
        # geom_point(aes(color = observed_region_hits, size = size_fun(genome_fraction)), shape = 16) +
        # scale_color_gradientn(colors = rev(brewer.pal(11, "Spectral")), limits = c(min_region_hits, min_region_hits + min(50, max(observed_region_hits)))) +
        # scale_size_continuous(trans = "log", breaks = seq(min(genome_fraction), max(genome_fraction), length = 5)^0.5*2 + 0.1, labels = paste0(sprintf("%.2f", round(seq(min(genome_fraction), max(genome_fraction), length = 5)*100)), "%")) +
        # scale_size(range = c(1, max(size_fun(genome_fraction))+0.1)) +
        
        # scale_size(range = c(10^(min(genome_fraction)), 10^(max(genome_fraction))), 
        # scale_size(range = c(1,10),
        #            breaks = seq(size_fun(min(genome_fraction)), size_fun(max(genome_fraction)), length = 5),
      #            labels = paste0(sprintf("%.2f", min(genome_fraction), max(genome_fraction), length = 5)*100), "%")) +
        scale_size(range = c(1,10),
                 breaks = seq(size_fun(min(genome_fraction)), size_fun(max(genome_fraction)), length = 5),
                 # labels = paste0(sprintf("%.2f", min(genome_fraction), max(genome_fraction), length = 5)*100), "%")) +
                 labels =  paste0(sprintf("%.2f", size_fun_rev(seq(size_fun(min(genome_fraction)), size_fun(max(genome_fraction)), length = 5))*100), "%")) +
        # labels = paste0(sprintf("%.2f", round(seq(min(genome_fraction), max(genome_fraction), length = 5)*100)), "%")) 
    
                 
                   # scale_size(range = c(1,10)) +
                   # scale_size(range = c(1, 6)) +
         labs(x = xlab, y = ylab, color = "Observed region hits", size = "Genome fraction") +
         theme_bw() +
         theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
               legend.position = c(0.02, .98),
               legend.justification = c(0, 1),
               # legend.key.size = 3,
               # legend.key.height = unit(1, 'cm'),
               # legend.key.width = unit(1, 'cm'),
               legend.title.align = 0.5, legend.text.align = 0.5,
               plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 20)),
               plot.margin = unit(c(1, 0.5, 0.5, 0.5), "cm"), 
               axis.title.y = element_text(margin = margin(r = 10)),
               axis.title.x = element_text(margin = margin(t = 10))) +
         # ggtitle(ifelse(is.null(input$titleVolcanoPlot), "Volcano plot", input$titleVolcanoPlot)) +
         ggtitle(input$titleVolcanoPlot) +
         geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "grey") +
         annotate("text", x = range(x)[1], y = -log10(0.05), label = qq("Region hits >= @{input$min_region_hits}"), hjust = 0, vjust = 1.5, size = 5) +
         annotate("text", x = Inf, y = -log10(0.05), label = qq("@{input$yFactorVolcano} = 0.05"), hjust = 1.05, vjust = 1.5, size = 4)
       
       if(input$xFactorVolcano == "fold_enrichment") {
         volcanoPlot <- volcanoPlot + geom_vline(xintercept = 1, linetype = "dashed", color = "grey")
       }
       
       output$volcanoPlot <- renderPlot({
         volcanoPlot
       }, height = function() {
         session$clientData$output_volcanoPlot_width * 0.8
       }
       )
    
    }) # close Event number 1
       
   output$enrichmentTable = DT::renderDataTable({
     table[, c("description", "observed_region_hits", "fold_enrichment", "p_value")]
   })
   
   
   observeEvent(input$selectTermID, {
     term = input$selectTermID
     
     tb = getRegionGeneAssociations(object, term_id = term)
     tb = as.data.frame(tb)
     colnames(tb) = c("Chromosome", "Start", "End", "Width", "Strand", "Annotated Genes", "Distance to TSSs")
     tb = tb[, -5]
     
     showModal(modalDialog(
       title = qq("Region-gene associations for term: @{term}"),
       HTML(qq("<pre>plotRegionGeneAssociations(@{obj_name}, term_id = '@{term}')</pre>")),
       plotOutput(outputId = "selectTermID_plot", width = "1000px", height= "400px"),
       hr(),
       HTML(qq("<pre>getRegionGeneAssociations(@{obj_name}, term_id = '@{term}')</pre>")),
       renderDT(datatable(tb, escape = FALSE, rownames = FALSE, selection = 'none', 
                          options = list(searching = FALSE))),
       easyClose = TRUE,
       size = "l"
     ))
   })
   
   associationsPlot <- plotRegionGeneAssociations(greatResult, term_id = NULL, which_plot = 1:3)
   
   output$associationsPlot <- renderPlot({
     associationsPlot
   })
                   
      
}) # close observe
  