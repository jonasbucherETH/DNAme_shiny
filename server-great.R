observe({
  # dmRegions <- inputDataReactive()$dmRegions
  # significantRegions <- inputDataReactive()$significantRegions
  greatResult <- inputDataReactive()$greatResult
  enrichmentTable <- inputDataReactive()$enrichmentTable
  regionGeneAssociations <- inputDataReactive()$regionGeneAssociations
  
  # greatResult_BP <- inputDataReactive()$greatResult_BP
  # enrichmentTable_BP <- inputDataReactive()$enrichmentTable_BP
  # greatResult_CC <- inputDataReactive()$greatResult_CC
  # enrichmentTable_CC <- inputDataReactive()$enrichmentTable_CC
  # greatResult_MF <- inputDataReactive()$greatResult_MF
  # enrichmentTable_MF <- inputDataReactive()$enrichmentTable_MF
  # 
  # output$output_BP <- shinydashboard::renderMenu({
  #   shinydashboard::menuSubItem("BP Output", tabName = "BP")
  # })
  # 
  # output$output_CC <- shinydashboard::renderMenu({
  #   shinydashboard::menuSubItem("CC Output", tabName = "CC")
  # })
  # 
  # output$output_MF <- shinydashboard::renderMenu({
  #   shinydashboard::menuSubItem("MF Output", tabName = "MF")
  # })
  

  # if(req(input$tabs)=="tab-great-BP") {
  #   message("You selected BP")
  #   greatResult <- inputDataReactive()$greatResult_BP
  #   enrichmentTable <- inputDataReactive()$enrichmentTable_BP
  # } 
  # if(req(input$tabs)=="tab-great-CC") {
  #   message("You selected CC")
  #   greatResult <- inputDataReactive()$greatResult_CC
  #   enrichmentTable <- inputDataReactive()$enrichmentTable_CC
  # }
  # if(req(input$tabs)=="tab-great-MF") {
  #   message("You selected MF")
  #   greatResult <- inputDataReactive()$greatResult_MF
  #   enrichmentTable <- inputDataReactive()$enrichmentTable_MF
  # }

  enrichmentTable$description <- breakStrings(enrichmentTable$description)
  
  # colour palette from deepak
  myPalette <- colorRampPalette(colors = viridis::plasma(n = 7)[c(2, 4, 6)])
  
  min_region_hits <- 5
  
  gr_all <- getRegionGeneAssociations(greatResult)
  # gr_all <- regionGeneAssociations
  gr_full_len <- greatResult@n_total
  
  createLink <- function(val) {
    # sprintf('<a href="http://amigo.geneontology.org/amigo/term/%s" target="_blank" class="btn btn-primary"</a>', val)
    sprintf('<a href="http://amigo.geneontology.org/amigo/term/%s" target="_blank" class="btn btn-primary">%s</a>', val, val)
  }
  
  # enrichmentTable$Link <- paste0("http://amigo.geneontology.org/amigo/term/", enrichmentTable$id)

  # table <- enrichmentTable

  # nHalf <- ceiling(ncol(enrichmentTable)/2)
  # cat(colnames(enrichmentTable)[1:nHalf])
  
  updateCheckboxGroupInput(
    session = session,
    inputId = "columns_to_display1",
    choiceNames = colnames(enrichmentTable)[-1],
    choiceValues = colnames(enrichmentTable)[-1],
    # choiceNames = colnames(enrichmentTable)[2:length(enrichmentTable)],
    # choiceValues = colnames(enrichmentTable)[2:length(enrichmentTable)],
    # choices = colnames(enrichmentTable)[1:nHalf],
    selected = colnames(enrichmentTable)[c(2,5,6,7)],
    inline = TRUE
  )
  
  # observeEvent(input$tabs, {
  #   # currentMenuItem <- strsplit(input$tabs, "_")[[1]][2]
  #   currentMenuItem <- strsplit(input$tabs, "-")[[1]][2]
  #   print(currentMenuItem)
  #   # # if (currentMenuItem == "tab-great") {
  #   # if (currentMenuItem == "great") {
  #   #   updateControlbar("controlbar")
  #   # }
  #   # updateControlbarMenu("controlbarMenu", selected = currentMenuItem)
  # })
  # # Clicking on the second controlbar item makes the box sidebar open
  # observeEvent(input$controlbarMenu, {
  #   #### NOTE: here I could put the whole recalculation
  #   if (input$controlbarMenu == "BP") {
  #     print("You chose BP")
  #     # updateBoxSidebar("boxSidebar")
  #   }
  #   else {
  #     print("You chose MF")
  #   }
  # })
  
  # if("KG" %in% enrichmentTable$collection) {
    # output$menuItemReactome <- renderMenu({
    #   menuSubItem(
    #     text = "Reactome", 
    #     tabName = "tab-Reactome"
    #   )
    # })
    # output$menuItemKegg <- renderMenu({
    #   menuSubItem(
    #     text = "Kegg", 
    #     tabName = "tab-Kegg"
    #   )
    # })
  # }
  
  output$menuItemReactome <- renderMenu({
    menuSubItem(
      text = "Reactome", 
      tabName = "tab-Reactome"
      )
  })
  output$menuItemKegg <- renderMenu({
    menuSubItem(
      text = "Kegg", 
      tabName = "tab-Kegg"
      )
  })
  
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
      # min_region_hits <- input$min_region_hits
      # table <- getEnrichmentTable(greatResult, min_region_hits)
      genome_fraction <- enrichmentTable$genome_fraction
      observed_region_hits <- enrichmentTable$observed_region_hits
      fold_enrichment <- enrichmentTable$fold_enrichment
      p_value <- enrichmentTable$p_value
      p_adjust <- enrichmentTable$p_adjust
      n <- greatResult@n_total
      z_score = (observed_region_hits - n*genome_fraction)/sqrt(n*genome_fraction*(1-genome_fraction))
      
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
      
      
      volcanoPlot <- ggplot(data.frame(x = x[l], y = y[l], observed_region_hits = observed_region_hits[l], genome_fraction = genome_fraction[l], id = enrichmentTable$id[l]), aes(x = x, y = y, text = id))
      
      volcanoPlot <- volcanoPlot +
        # geom_point(aes(color = log(observed_region_hits), size = log(genome_fraction))) +
        geom_point(aes(color = observed_region_hits, size = genome_fraction)) +
        # scale_color_gradientn(
        #   colors = myPalette(n = nrow(observed_region_hits)),
        #   breaks = c(ceiling(min(log(observed_region_hits))), floor(max(log(observed_region_hits))))) +
        scale_color_gradientn(
          trans = "log",
          colors = myPalette(n = nrow(observed_region_hits)),
          # breaks = c(ceiling(min(log(observed_region_hits))), floor(max(log(observed_region_hits))))) +
          # breaks = c(min(observed_region_hits), max(observed_region_hits))
          breaks = waiver()
        ) +
        # scale_size(range = c(1,10),
        #          breaks = seq(size_fun(min(genome_fraction)), size_fun(max(genome_fraction)), length = 5),
        #          # labels = paste0(sprintf("%.2f", min(genome_fraction), max(genome_fraction), length = 5)*100), "%")) +
        #          labels =  paste0(sprintf("%.2f", size_fun_rev(seq(size_fun(min(genome_fraction)), size_fun(max(genome_fraction)), length = 5))*100), "%")
        # ) +
        scale_size(
          trans = "log1p",
          range = c(1,6),
          # trans = "asn",
          breaks = waiver()
          # n.breaks = 5
          # labels = paste0(sprintf("%.2f", min(genome_fraction), max(genome_fraction), length = 5)*100), "%")) +
          # labels =  paste0(sprintf("%.2f", size_fun_rev(seq(size_fun(min(genome_fraction)), size_fun(max(genome_fraction)), length = 5))*100), "%")
        ) +
        
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
        annotate("text", x = range(x)[1], y = -log10(0.05), label = qq("Region hits >= @{min_region_hits}"), hjust = 0, vjust = 1.5, size = 5) +
        annotate("text", x = Inf, y = -log10(0.05), label = qq("@{input$yFactorVolcano} = 0.05"), hjust = 1.05, vjust = 1.5, size = 4)
      
      if(input$xFactorVolcano == "fold_enrichment") {
        volcanoPlot <- volcanoPlot + geom_vline(xintercept = 1, linetype = "dashed", color = "grey")
      }
      
      volcanoPlotly <- ggplotly(volcanoPlot,
                                tooltip = c("id"),
                                width = session$clientData$output_volcanoPlot_width,
                                height = session$clientData$output_volcanoPlot_height,
                                interactive = F) %>%
                                layout(legend = list(
                                  orientation = "h",
                                  x = -1,
                                  y = 1
                                  )
                                )
       
      output$volcanoPlot <- renderPlotly({
         volcanoPlotly
      }
      # , height = function() {
      #       session$clientData$output_volcanoPlot_width * 0.8
      #    }
      )
      
      # cdata <- session$clientData
      # output$pid <- renderPlotly({
      #   p <- ggplot(iris) + 
      #     geom_point(aes(Sepal.Length, Sepal.Width)) +
      #     facet_wrap(~Species)
      #   ggplotly(p, width = cdata$output_pid_width, height = cdata$output_pid_height)
    
  }) # close Event number 1
  
  selected_cols <- reactive({
    cols <- c("id", input$columns_to_display1)
    enrichmentTable[, cols, drop = FALSE]
  })
  
  # Render the datatable
  output$enrichmentTable <- DT::renderDataTable({
    
    # datatable(selected_cols(), rownames = FALSE, selection = "single",
    #           # callback = JS("function(data) { return '<a href=\"' + data + '\" target=\"_blank\">Link</a>'}"),
    #           options = list(pageLength = 10, dom = 't', scrollX = TRUE),
    #           escape = FALSE) %>% formatStyle(
    #             columns = 'Link', textDecoration = 'underline', fontWeight = 'bold',
    #             color = 'blue', fontStyle = 'italic', target = "cell")
    dt <- selected_cols()
    dt$id <- createLink(dt$id)
    # dt <- datatable(dt)
    return(dt)
    
    }, escape = F, filter = "bottom", selection = "single"
  )
  
   
   # # React to the user's row selection
   # selected_row <- reactive({
   #   # Get the index of the selected row
   #   input$enrichmentTable_rows_selected
   # })

  #### React to the user's row selection using observeEvent
  observeEvent( # Event number 2
    {
      input$enrichmentTable_rows_selected
    },
    ignoreInit = F,
    ignoreNULL = F,
    {
      # Get the index of the selected row
      selected_row <- input$enrichmentTable_rows_selected
      # Use the selected row as an input for some other action (e.g., printing the selected row index)
      # print(selected_row)
      
      gr_full_len <- greatResult@n_total
      gr_all <- getRegionGeneAssociations(greatResult)
      # if(selected_row) {
      if(!is.null(input$enrichmentTable_rows_selected)) {
        gr_term <- getRegionGeneAssociations(greatResult, enrichmentTable$id[selected_row])
        term_id <- enrichmentTable$description[selected_row]
        associationsPlot <- ggplot_great(gr_all, gr_term = gr_term, gr_full_len, term_id = term_id)
      } else {
        associationsPlot <- ggplot_great(gr_all, gr_term = NULL, gr_full_len, term_id = NULL)
      }
      
      output$associationsPlots <- renderPlot({
        # grid.arrange(associationsPlot[[1]], associationsPlot[[2]], associationsPlot[[3]], nrow = 1)
        plot_grid(associationsPlot[[1]], associationsPlot[[2]], associationsPlot[[3]], nrow = 1, aligh = "v", axis = "b")
        
      })
    }) # close Event number 2
   
   
   # observeEvent(input$selectTermID, {
   #   term = input$selectTermID
   #   
   #   tb = getRegionGeneAssociations(object, term_id = term)
   #   tb = as.data.frame(tb)
   #   colnames(tb) = c("Chromosome", "Start", "End", "Width", "Strand", "Annotated Genes", "Distance to TSSs")
   #   tb = tb[, -5]
   #   
   #   showModal(modalDialog(
   #     title = qq("Region-gene associations for term: @{term}"),
   #     HTML(qq("<pre>plotRegionGeneAssociations(@{obj_name}, term_id = '@{term}')</pre>")),
   #     plotOutput(outputId = "selectTermID_plot", width = "1000px", height= "400px"),
   #     hr(),
   #     HTML(qq("<pre>getRegionGeneAssociations(@{obj_name}, term_id = '@{term}')</pre>")),
   #     renderDT(datatable(tb, escape = FALSE, rownames = FALSE, selection = 'none', 
   #                        options = list(searching = FALSE))),
   #     easyClose = TRUE,
   #     size = "l"
   #   ))
   # })
  
  top20 <- enrichmentTable[1:20, ]

  dotPlot <- ggplot(data = top20, mapping = aes(
    x = description,
    y = fold_enrichment,
    color = -log10(p_adjust),
    size = observed_gene_hits
  )) +
    geom_point(show.legend = T, alpha = 0.7, stroke = 1.5) +
    coord_flip(expand = T) +
    xlab("") +
    ylab(expression(Log[2] ~ "Fold-change")) +
    # facet_grid(collection ~ , labeller = label_wrap_gen(width = 12, multi_line = T), scales = "free", space = "free_y") +
    # facet_grid(collection ~ Comparison, labeller = label_wrap_gen(width = 12, multi_line = T), scales = "free", space = "free_y") +
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 70)) +
    theme_bw() +
    theme(axis.text = element_text(color = "black"),
          legend.title = element_text(color = "black"),
          axis.title = element_text(color = "black", face = "bold")
    ) + 
    labs(
      color = expression(-Log[10] ~ "adjusted" ~ italic(P) ~ " "),
      size = "Annotated genes"
    ) +
    scale_colour_gradientn(
      colours = myPalette(n = nrow(top20)),
      breaks = c(ceiling(min(-log10(top20$p_adjust))), floor(max(-log10(top20$p_adjust))))
    ) +
    scale_color_gradientn(
      trans = "log",
      colors = myPalette(n = nrow(top20)),
      # breaks = c(ceiling(min(log(observed_region_hits))), floor(max(log(observed_region_hits))))) +
      # breaks = c(min(observed_region_hits), max(observed_region_hits))
      breaks = waiver()
    ) +
    guides(size = guide_legend(reverse=TRUE))
  
  output$dotPlot <- renderPlot({
    dotPlot
  }, height = function() {
        session$clientData$output_dotPlot_width * 0.8
     }
  )
   
}) # close observe
  