observe({
  # dmRegions <- inputDataReactive()$dmRegions
  # significantRegions <- inputDataReactive()$significantRegions
  greatResult <- inputDataReactive()$greatResult
  enrichmentTable <- inputDataReactive()$enrichmentTable
  regionGeneAssociations <- inputDataReactive()$regionGeneAssociations
  
  enrichmentTable$description <- breakStrings(enrichmentTable$description)
  
  # colour palette from deepak
  myPalette <- colorRampPalette(colors = viridis::plasma(n = 7)[c(2, 4, 6)])
  
  min_region_hits <- 5
  
  # gr_all <- getRegionGeneAssociations(greatResult)
  gr_all <- regionGeneAssociations
  gr_full_len <- greatResult@n_total
  
  ### add link to amigo to enrichmentTable
  # enrichmentTable$Link <- htmltools::a(
  #   href = paste0("http://amigo.geneontology.org/amigo/term/", enrichmentTable$id),
  #   label = "Link",
  #   style = "color: blue",
  #   target = "_blank"
  # )
  
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
      ggplot_great = function(gr_all, gr_term = NULL, gr_full_len, term_id = NULL) {
        
        # op = par(no.readonly = TRUE)
        # on.exit(suppressWarnings(par(op)))
        # par(mfrow = c(1, 3), mar = c(6, 4, 4, 1), xpd = NA)
        
        using_term = !is.null(gr_term)
        
        df_all = data.frame(distTSS = unlist(gr_all$dist_to_TSS))
        if(using_term) {
          df_term = data.frame(distTSS = unlist(gr_term$dist_to_TSS))
        }
        
        # make plots
        if(using_term) {
          tb = table(table(unlist(gr_term$annotated_genes)))
          vt = numeric(10)
          vt[as.numeric(names(tb))] = tb
          vt[is.na(vt)] = 0
          v = c(vt[1:9], sum(vt[10:length(vt)]))
          names(v) = c(as.character(1:9), ">= 10")
          v[is.na(v)] = 0
          p = v/sum(v)
          # pos = barplot(p, col = "black", xlab = "Number of associated regions per gene", ylab = "This term's genes", ylim = c(0, max(p)*1.5), main = qq("Number of associated regions per gene\nTerm: @{term_id}"))
          # text(pos[, 1], p + 0.01, v, adj = c(0.5, 0), cex = 0.8)
          df1 <- data.frame(x = names(p), y = p, v = v)
          df1$x <- factor(df1$x, levels = df1$x)
          p1 <- ggplot(df1, aes(x, y)) +
            geom_col(fill = "black", color = "black") +
            labs(x = "Number of associated regions per gene",
                 y = "This term's genes",
                 title = paste("Number of associated regions per gene\nTerm:", term_id)) +
            ylim(0, max(p) * 1.25) +
            geom_text(aes(label = v), vjust = -0.5, size = 3)
        } else {
          tb = table(table(unlist(gr_all$annotated_genes)))
          v = c(gr_full_len - length(gr_all), tb["1"], tb["2"], sum(tb[as.numeric(names(tb)) > 2]))
          names(v) = c("0", "1", "2", "> 3")
          v[is.na(v)] = 0
          p = v/sum(v)
          df1 <- data.frame(x = names(p), y = p, v = v)
          df1$x <- factor(df1$x, levels = df1$x)
          # p1 <- ggplot(data.frame(x = names(p), y = p, v = v), aes(x, y)) +
          p1 <- ggplot(df1, aes(x, y)) +
            geom_col(fill = c("red", "grey", "grey", "grey"), color = "black") +
            labs(x = "Number of associated genes per region",
                 y = "Genomic regions",
                 title = "Number of associated genes per region") +
            ylim(0, max(p) * 1.25) +
            geom_text(aes(label = v, color = x), vjust = -0.5, size = 3) +
            scale_color_manual(values = c("red", "black", "black", "black")) +
            guides(color = "none", fill = guide_legend(override.aes = list(color = c("grey", "red")), title = NULL))
        }
        p1 <- p1 + theme_classic()
        p1 <- p1 + theme(
          axis.title.x = element_text(vjust = -1),
          axis.text.x = element_text(size = 10, vjust = -1)
          )
        ################ 2
        v = cbind(
          c("<-500"       = sum(df_all$distTSS < -500000),
            "[-500, -50)" = sum(df_all$distTSS >= -500000 & df_all$distTSS < -50000),
            "[-50, -5)"   = sum(df_all$distTSS >= -50000  & df_all$distTSS < -5000),
            "[-5, 0]"     = sum(df_all$distTSS >= -5000   & df_all$distTSS < 0),
            "0"           = sum(df_all$distTSS == 0),
            "(0, 5]"      = sum(df_all$distTSS > 0       & df_all$distTSS <= 5000),
            "(5, 50]"     = sum(df_all$distTSS > 5000    & df_all$distTSS <= 50000),
            "(50, 500]"   = sum(df_all$distTSS > 50000   & df_all$distTSS <= 500000),
            "> 500"       = sum(df_all$distTSS > 500000)))
        
        if(using_term) {
          v = cbind( 
            c("<-500"       = sum(df_term$distTSS < -500000),
              "[-500, -50)" = sum(df_term$distTSS >= -500000 & df_term$distTSS < -50000),
              "[-50, -5)"   = sum(df_term$distTSS >= -50000  & df_term$distTSS < -5000),
              "[-5, 0]"     = sum(df_term$distTSS >= -5000   & df_term$distTSS < 0),
              "0"           = sum(df_term$distTSS == 0),
              "(0, 5]"      = sum(df_term$distTSS > 0       & df_term$distTSS <= 5000),
              "(5, 50]"     = sum(df_term$distTSS > 5000    & df_term$distTSS <= 50000),
              "(50, 500]"   = sum(df_term$distTSS > 50000   & df_term$distTSS <= 500000),
              "> 500"       = sum(df_term$distTSS > 500000)), v)
        }
        
        bins <- data.frame(
          bin = c("<-500", "[-500, -50)", "[-50, -5)", "[-5, 0]", "0", "(0, 5]", "(5, 50]", "(50, 500]", "> 500"),
          lower_bound = c(-Inf, -500000, -50000, -5000, 0, 0, 5000, 50000, 500000),
          upper_bound = c(-500000, -50000, -5000, 0, 0, 5000, 50000, 500000, Inf)
        )
        
        # Create a function to compute the bin counts
        compute_counts <- function(df, bins) {
          counts <- sapply(seq_len(nrow(bins)), function(i) {
            sum(df$distTSS >= bins[i, "lower_bound"] & df$distTSS < bins[i, "upper_bound"])
          })
          counts <- as.data.frame(counts)
          rownames(counts) <- bins$bin
          return(counts)
        }
        
        # Compute the bin counts
        counts_all <- compute_counts(df_all, bins)
        if (using_term) {
          counts_term <- compute_counts(df_term, bins)
        }
        
        # Compute the fractions
        p_all <- counts_all / colSums(counts_all)
        p_all[is.na(p_all)] <- 0
        if (using_term) {
          p_term <- counts_term / colSums(counts_term)
          p_term[is.na(p_term)] <- 0
        }
        
        # Combine the fractions into a data frame
        if(using_term) {
          # df_plot <- data.frame(
          #   bin = rep(bins$bin, 2),
          #   fraction = c(p_all, p_term),
          #   set = rep(c("Set-wide", "This term"), each = nrow(bins)))
          # colnames(df_plot) <- c("bin", "fraction", "set")
          df_plot <- data.frame(
            rep(bins$bin, 2),
            rbind(p_all, p_term),
            rep(c("Set-wide", "This term"), each = nrow(bins)),
            rbind(counts_all, counts_term))
          colnames(df_plot) <- c("bin", "fraction", "set", "count")  
        } else {
          df_plot <- data.frame(
            rep(bins$bin, 1),
            p_all,
            counts_all)
          colnames(df_plot) <- c("bin", "fraction", "count")
        }
        
        df_plot$bin <- factor(df_plot$bin, levels = unique(df_plot$bin), ordered=TRUE)
        
        if(using_term) {
          p2 <- ggplot(df_plot, aes(x = bin, y = fraction, fill = set)) +
            geom_bar(position = "dodge", stat = "identity", color = "black") +
            labs(x = "Distance to TSS (kb)",
                 y = "Region-gene associations (fraction)",
                 title = paste("Binned by orientation and distance to TSS\nTerm:", term_id)) +
            ylim(0, max(df_plot$fraction) * 1.5) +
            geom_text(aes(label = count), position = position_dodge(0.90), vjust = -0.5, size = 3) +
            scale_fill_manual(values = c("blue", "green"))
        } else {
          p2 <- ggplot(df_plot, aes(x = bin, y = fraction)) +
            geom_bar(position = "dodge", stat = "identity", fill = "blue", color = "black") +
            labs(x = "Distance to TSS (kb)",
                 y = "Region-gene associations (fraction)",
                 title = "Binned by orientation and distance to TSS") +
            ylim(0, max(df_plot$fraction) * 1.5) +
            geom_text(aes(label = count), position = position_dodge(0.90), vjust = -0.5, size = 3)
          # + scale_fill_manual(values = c("blue", "green"))
        }
        
        ### arrow
        p2 <- p2 + geom_segment(aes(x = 5, y = 0, xend = 5, yend = max(df_plot$fraction) * 1.25))
        p2 <- p2 + geom_segment(aes(x = 5, y = max(df_plot$fraction) * 1.25, xend = 6, yend = max(df_plot$fraction) * 1.25),
                                arrow = arrow(angle = 20, length = unit(0.5, "cm")))
        p2 <- p2 + annotate("text", x = 5.5, y = max(df_plot$fraction) * 1.3, label = "TSS")

        p2 <- p2 + theme_classic()
        p2 <- p2 + theme(
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
          legend.position = "topright") +
          guides(fill="none")
        
        ######################################   3    #################################
        # Compute the bin counts
        
        df_counts <- df_plot
        df_counts$fraction[6] <- df_plot$fraction[4] + df_plot$fraction[6]
        df_counts$fraction[7] <- df_plot$fraction[3] + df_plot$fraction[7]
        df_counts$fraction[8] <- df_plot$fraction[2] + df_plot$fraction[8]
        df_counts$fraction[9] <- df_plot$fraction[1] + df_plot$fraction[9]
        if(using_term) {
          df_counts$fraction[15] <- df_plot$fraction[13] + df_plot$fraction[15]
          df_counts$fraction[16] <- df_plot$fraction[12] + df_plot$fraction[16]
          df_counts$fraction[17] <- df_plot$fraction[11] + df_plot$fraction[17]
          df_counts$fraction[18] <- df_plot$fraction[10] + df_plot$fraction[18]
          df_counts <- df_counts[c(5:9, 14:18), ]
        } else {
          df_counts <- df_counts[c(5:9), ]
        }
        # df_counts <- df_counts[c(5:9, 14:18), ]
        df_counts$bin <- factor(df_counts$bin, levels = unique(df_counts$bin), ordered=TRUE)
        
        if(using_term) {
          p3 <- ggplot(df_counts, aes(x = bin, y = fraction, fill = set)) +
            geom_bar(position = "dodge", stat = "identity", color = "black") +
            labs(x = "Absolute distance to TSS (kb)",
                 y = "Region-gene associations (fraction)",
                 title = paste("Binned by absolute distance to TSS\nTerm:", term_id)) +
            ylim(0, max(df_counts$fraction) * 1.5) +
            scale_fill_manual(values = c("blue", "green"))
        } else {
          p3 <- ggplot(df_counts, aes(x = bin, y = fraction)) +
            geom_bar(position = "dodge", stat = "identity", fill = "blue", color = "black") +
            labs(x = "Absolute distance to TSS (kb)",
                 y = "Region-gene associations (fraction)",
                 title = "Binned by absolute distance to TSS") +
            ylim(0, max(df_counts$fraction) * 1.5)

          # + scale_fill_manual(values = c("blue", "green"))
        }
        
        ### arrows
        # p3 <- p3 + geom_segment(aes(x = 0, y = 0, xend = 0, yend = max(df_counts$fraction) * 1.25),
        #                         arrow = arrow(angle = 90, length = unit(0.5, "cm")))
        
        p3 <- p3 + theme_classic()
        p3 <- p3 + theme(
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
          legend.position = "topright")
        
        ################ arrange plots
        # p_all <- grid.arrange(p1, p2, p3, ncol = 3)
        p_all <- list("p1" = p1, "p2" = p2, "p3" = p3)
        
        return(p_all)
        # grid.arrange(p1, p2, ncol = 2)
      }
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
  