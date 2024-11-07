observe({
  methylKit_rds <- inputDataReactive()$methylKit_rds
  myPalette <- inputDataReactive()$myPalette
  sampleNames <- inputDataReactive()$sampleNames
  
  methylRaw <- methylKit_rds$methylRawListDB
  methylAll <- unite(methylRaw)
  methylAll@sample.ids <- sampleNames
  myDiff <- methylKit_rds$myDiff
  myDiff@sample.ids <- sampleNames
    
  dataset <- inputDataReactive()$dataset
  factorNames <- inputDataReactive()$factorNames
  factorLevels <- inputDataReactive()$factorLevels
  factors <- inputDataReactive()$factors
  colourList <- inputDataReactive()$colourList
  
  testCovariate <- inputDataReactive()$testCovariate
  
  observeEvent( # Event assign colours groups
    {
      # input$color_select
      input$confirm_color_assignment
    },
    ignoreInit = F, # If TRUE, then, when the eventified object is first created/initialized, don't trigger the action or (compute the value). The default is FALSE.
    ignoreNULL = T, # default = TRUE
    {
      colorLevels <- c(factorLevels, rownames(dataset))
      for (i in seq_along(colorLevels)) {
        colourList[i] <- paste0(col2hex(input[[paste0("GroupColour", i)]]), "FF")
      }
    }
  )
  
  # str_end <- rep(paste0("_", sampleIDs), times = 3)
  # str_sub(colnames(methylAll)[5:length(methylAll)], start = -2, end = -1) <- str_end
  # sampleNames <- c("test1","test2","ctrl1","ctrl2")

  
  # observeEvent(input$pickFactorsPCA, {
  #   output$res_classic <- renderPrint(input$pickFactorsPCA)
  # })
  
  # output$res_classic <- renderPrint(input$classic)
  
  methylationList <- list()
  for(i in seq_along(sampleNames)) {
    dataMethylRaw <- getData(methylRaw[[i]])
    totalCoverage <- sum(dataMethylRaw$coverage)
    methylationDF <- data.frame(
      "Percent_Methylation" = 100*dataMethylRaw$numCs / (dataMethylRaw$numCs + dataMethylRaw$numTs),
      "Coverage" = dataMethylRaw$coverage
    )
    methylationList <- c(methylationList, list(methylationDF))
  }
  
  palettesCategorical <- brewer.pal.info[brewer.pal.info$category=="qual" & brewer.pal.info$maxcolors >= length(sampleNames) & brewer.pal.info$colorblind==TRUE, ]
  # display.brewer.pal(length(sampleNames), "Dark2")
  # display.brewer.pal(length(sampleNames), "Paired")
  # display.brewer.pal(length(sampleNames), "Set2")
  output$colorPalettesPlot <- renderPlot({
    display.brewer.all(n=length(sampleNames), select=rownames(palettesCategorical), exact.n=TRUE)
  })
  
  updateSelectInput(
    inputId = "colorPalettePCA",
    choices = rownames(palettesCategorical),
    selected = rownames(palettesCategorical)[1]
  )
  
  updateSelectInput(
    inputId = "sampleHistograms",
    # label = "Select sample to display",
    choices = sampleNames,
    selected = sampleNames[1]
  )
  
  observeEvent(input$methylKitTabCard, {
    updateCardSidebar("sidebarMethylKit")
  })
  
  # # use normalized coverage?
  # methylationMatrixAll <- c()
  # for(i in seq_along(sampleNames)) {
  #   
  #   # data_i <- getData(normalizedMethylRaw[[1]])
  #   # print(length(data_i[,1]))
  #   # totalCoverage_i <- sum(data_i$coverage)
  #   # methylationMatrixAll <- cbind(methylationMatrixAll,
  #   #     100*data_i$numCs / (data_i$numCs + data_i$numTs)
  #   # )
  # }
  # # methylationMatrixAll <- as.data.frame(methylationMatrixAll)
  # colnames(methylationMatrixAll) <- sampleNames
  # 
  # pcaResult <- prcomp(methylationMatrixAll, retx = T, center = TRUE, scale. = FALSE,
  #        tol = NULL, rank. = NULL)
  # 
  # dataMethylRaw <- getData(methylRaw[[1]])
  # totalCoverage <- sum(dataMethylRaw$coverage)
  # methylationMatrix <- data.frame(
  #   "Percent_Methylation" = 100*dataMethylRaw$numCs / (dataMethylRaw$numCs + dataMethylRaw$numTs),
  #   "Coverage" = dataMethylRaw$coverage
  # )
  
  resultsPCA <- PCASamples(methylAll, obj.return = T) # uses prcomp using percent methylation matrix as input
  # resultsPCA$x # positions of samples
  # resultsPCA$sdev
  # resultsPCA$rotation # PCs 1-4

  # pcaTableMeth <- data.frame(groupingVariables, pcaResults$rotation, stringsAsFactors = FALSE, row.names = rownames(datasetPCA))
  pcaTableMeth <- data.frame(resultsPCA$x, stringsAsFactors = FALSE)
  
  pcaVarprop <- tibble(PC = paste0("PC", factor(1:length(resultsPCA$sdev))), variance = (resultsPCA$sdev)^2) %>% 
    # mutate(pct = format(variance/sum(variance)*100, digits = 2)) %>%
    mutate(pct = variance/sum(variance)*100) %>%
    mutate(pct_cum = cumsum(pct))
  # tabVarpropMeth <- pcaVarprop
  # for (i in 1:nPC) {
  #   colnames(pcaTable)[i + nGrouping] <- paste0("PC", i)
  # }
  
  ############### Get the gene loadings (in dudi.pca: $c1) 
  # # here loadings=rotation??
  # pc_loadings <- resultsPCA$rotation # 479x4(PCs)
  # # colnames(pc_loadings) <- colnames(pcaTableMeth)
  # # colnames(pc_loadings) <- pcList
  # 
  # # pc_loadings <- pc_loadings %>%
  # #   as_tibble(rownames = "gene")
  # pc_loadings$gene <- rownames(pc_loadings)
  # top_genes <- pc_loadings %>%
  #   dplyr::select(gene, PC1, PC2) %>%
  #   # dplyr::select(gene, pcList) %>%
  #   pivot_longer(matches("PC"), names_to = "PC", values_to = "loading") %>%
  #   group_by(PC) %>%
  #   arrange(desc(abs(loading)))
  ###############
  
  updateSelectInput(
    session = session,
    "pickFactor1PCA",
    choices = colnames(resultsPCA$rotation),
    selected = colnames(resultsPCA$rotation)[1]
  )

  updateSelectInput(
    session = session,
    "pickFactor2PCA",
    choices = colnames(resultsPCA$rotation),
    selected = colnames(resultsPCA$rotation)[2]
  )
  
  # updatePickerInput(
  #   session = session ,
  #   inputId = "pickFactorsPCA",
  #   label = "Select PCs to plot",
  #   choices = colnames(resultsPCA$rotation),
  #   selected = list(colnames(resultsPCA$rotation)[1], colnames(resultsPCA$rotation)[2])
  #   # multiple = TRUE,
  #   # options =  list("max-options-group" = 1)
  # )
  
  # observeEvent( # Event sidebar; tab change
  #   {
  #     input$methylKitTabCard
  #   },
  #   ignoreInit = F, # If TRUE, then, when the eventified object is first created/initialized, don't trigger the action or (compute the value). The default is FALSE.
  #   ignoreNULL = T, # default = TRUE
  #   {
  #     # print(input$methylKitTabCard) # -> titles of tabPanels
  #     # [1] "PCA of samples"
  #     # [1] "Methylation Statistics"
  #     if(input$methylKitTabCard == "PCA of samples"){
  #       # updatebs4CardSidebar(
  #       #   id = "sidebarMethylKit", 
  #       #   session = session,
  #       #   )
  #       output$sidebarUI <- renderUI({
  #         tagList(
  #           sliderInput("n", "N", 1, 1000, 500),
  #           textInput("label", "Label")
  #         )
  #       })
  #     }
  #     
  #     # updatebs4CardSidebar(id = "sidebarMethylKit", session = shiny::getDefaultReactiveDomain())
  #   }
  # )
  
  # observeEvent( # Event manual colors PCA
  #   {
  #     input$applyManualcolorsPCA
  #   },
  #   ignoreInit = T, # If TRUE, then, when the eventified object is first created/initialized, don't trigger the action or (compute the value). The default is FALSE.
  #   ignoreNULL = T, # default = TRUE
  #   {
     
  clickCount <- reactiveVal(0) 
  
  observeEvent( # Event PCA
    {
      # input$sampleLabelsPCA
      input$pickFactor1PCA
      input$pickFactor2PCA
      # input$pickFactorsPCA
      input$sampleLabelsPCA
      input$textSizePCA
      input$pointSizePCA
      input$pcaTitle
      # input$actionButtoncolors
      #input$selectThemePCA
    },
    ignoreInit = T, # If TRUE, then, when the eventified object is first created/initialized, don't trigger the action or (compute the value). The default is FALSE.
    ignoreNULL = T, # default = TRUE
    {


      # print(input$colorPalettePCA)
      plotMethPCA <- pcaTableMeth %>%
        ggplot(aes(
          x = .data[[input$pickFactor1PCA]], 
          y = .data[[input$pickFactor2PCA]], 
          color = rownames(pcaTableMeth)
        )) +
        # Add confidence ellipses
        # stat_ellipse(aes(fill = rownames(pcaTableMeth)), 
        #              geom = "polygon", 
        #              alpha = 0.2, 
        #              show.legend = FALSE) +
        # Main points
        geom_point(size = as.numeric(input$pointSizePCA), 
                   alpha = 0.8) +
        # Custom colors
        scale_color_manual(values = unlist(colourList[rownames(pcaTableMeth)])) +
        scale_fill_manual(values = unlist(colourList[rownames(pcaTableMeth)])) +
        # Add cross at origin
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey70", alpha = 0.5) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "grey70", alpha = 0.5) +
        # Better theme
        theme_bw() +
        theme(
          panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(color = "grey20", fill = NA, linewidth = 1),
          
          # Title formatting
          plot.title = element_text(
            color = "grey20", 
            size = input$textSizePCA * 1.2, 
            face = "bold", 
            hjust = 0.5, 
            margin = margin(b = 20)
          ),
          
          # Axis formatting
          axis.text = element_text(
            color = "grey20", 
            size = input$textSizePCA,
            face = "plain"
          ),
          axis.title = element_text(
            color = "grey20", 
            size = input$textSizePCA,
            face = "plain",
            margin = margin(t = 10, r = 10, b = 10, l = 10)
          ),
          
          # Legend formatting
          legend.position = "right",
          legend.box.background = element_rect(color = "grey80", fill = "white"),
          legend.box.margin = margin(6, 6, 6, 6),
          legend.text = element_text(size = input$textSizePCA),
          legend.title = element_text(size = input$textSizePCA, face = "bold"),
          
          # Plot margins
          plot.margin = unit(c(1, 1, 1, 1), "cm")
        )
      
      # Add labels if requested
      if (input$sampleLabelsPCA) {
        plotMethPCA <- plotMethPCA +
          ggrepel::geom_text_repel(  # Using ggrepel instead of geom_text
            aes(label = rownames(pcaTableMeth)),
            size = (input$textSizePCA / 3),
            box.padding = 0.5,
            point.padding = 0.2,
            min.segment.length = 1,
            # segment.color = "grey50",
            # segment.length = 0,
            show.legend = FALSE
          )
      }
      
      # Add informative labels
      plotMethPCA <- plotMethPCA + labs(
        title = input$pcaTitle,
        x = paste0(
          input$pickFactor1PCA, 
          " (", 
          sprintf("%.1f", pcaVarprop$pct[pcaVarprop$PC == input$pickFactor1PCA]), 
          "% variance)"
        ),
        y = paste0(
          input$pickFactor2PCA, 
          " (", 
          sprintf("%.1f", pcaVarprop$pct[pcaVarprop$PC == input$pickFactor2PCA]), 
          "% variance)"
        ),
        color = "Samples"
      )
      
      # Add expanded limits with some padding
      plotMethPCA <- plotMethPCA +
        scale_x_continuous(
          limits = c(
            min(pcaTableMeth[[input$pickFactor1PCA]]) * 1.1,
            max(pcaTableMeth[[input$pickFactor1PCA]]) * 1.2
          ),
          expand = expansion(mult = c(0.1, 0.1))
        ) +
        scale_y_continuous(
          limits = c(
            min(pcaTableMeth[[input$pickFactor2PCA]]) * 1.1,
            max(pcaTableMeth[[input$pickFactor2PCA]]) * 1.2
          ),
          expand = expansion(mult = c(0.1, 0.1))
        )
      
      output$pcaPlot <- renderPlot({
        plotMethPCA
      })
      
    }
  ) # close Event PCA
  
  output$pcaScree <- renderPlot({
    pcaVarprop2 <- pcaVarprop
    pcaVarprop2$PC <- gsub("PC", "", pcaVarprop2$PC)
    pcaVarprop2$PC <- factor(pcaVarprop2$PC, levels = pcaVarprop2$PC)
    pcaVarprop2$variance <- as.numeric(pcaVarprop2$variance)
    pcaVarprop2$pct <- as.numeric(pcaVarprop2$pct)
    pcaVarprop2$pct_cum <- as.numeric(pcaVarprop2$pct_cum)
    
    if (nrow(pcaVarprop2) > 10) {
      pcaVarprop2 <- pcaVarprop2[1:10,]
    }
    
    pcaScree <- pcaVarprop2 %>%
      ggplot(aes(x = PC)) +
      # Add bars with custom appearance
      geom_col(aes(y = pct), 
               fill = "#3c8dbc",    # Nice blue color
               alpha = 0.7,         # Some transparency
               width = 0.7) +       # Slightly thinner bars
      # Add cumulative line
      geom_line(aes(y = pct_cum, group = 1),
                color = "#e74c3c",  # Contrasting red color
                linewidth = 1) +    # Slightly thicker line
      # Add points on the line
      geom_point(aes(y = pct_cum),
                 color = "#e74c3c", 
                 size = 3) +
      # Add percentage labels on top of bars
      geom_text(aes(y = pct, 
                    label = sprintf("%.1f%%", pct)),
                vjust = -0.5,
                size = 3.5) +
      # Better labels
      labs(title = "Scree Plot",
           x = "Principal Component",
           y = "Variance Explained (%)",
           caption = sprintf("Total variance explained (PC1-PC%s): %.1f%%", 
                             nrow(pcaVarprop2), 
                             max(pcaVarprop2$pct_cum))) +
      # Customize scales
      scale_y_continuous(
        limits = c(0, max(pcaVarprop2$pct_cum) * 1.1),  # Add some headroom for labels
        breaks = seq(0, 100, by = 10),
        # Add second axis for cumulative percentage
        sec.axis = sec_axis(~., name = "Cumulative Variance (%)")
      ) +
      # Theme customization
      theme_classic(base_size = as.numeric(input$textSizePCA)) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 1, face = "italic"),
        # Add margin around plot
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
      )
    
    pcaScree
  })
  
  output$pcaLoadings <- DT::renderDataTable({
    datatable(top_genes, rownames = F) %>% formatRound("loading", digits = 3)
    # datatable(top_genes[[c(input$pickFactor1PCA,input$pickFactor2PCA)]], rownames = F) %>% formatRound("loading", digits = 3)
  })
  

  observeEvent( # Event histograms (1)
    {
      input$sampleHistograms
    },
    ignoreInit = F, # If TRUE, then, when the eventified object is first created/initialized, don't trigger the action or (compute the value). The default is FALSE.
    ignoreNULL = T, # default = TRUE
    {

      if(input$sampleHistograms == "") {
        sampleIndex <- 1
        # sampleIndex <- 1:length(sampleNames)
      } else {
        sampleIndex <- which(sampleNames == input$sampleHistograms)
      }
      
      # dataMethylRaw <- getData(methylRaw[[sampleIndex]])
      # totalCoverage <- sum(dataMethylRaw$coverage)
      # methylationDF <- data.frame(
      #   "Percent_Methylation" = 100*dataMethylRaw$numCs / (dataMethylRaw$numCs + dataMethylRaw$numTs),
      #   "Coverage" = dataMethylRaw$coverage
      # )

      methylationDF <- methylationList[[sampleIndex]]
      # methylationDF <- methylationList[[1]]
      
      hist_data <- ggplot_build(
        ggplot(methylationDF, aes(x = Percent_Methylation)) +
          stat_bin(binwidth = 10, boundary = 0, closed = "left")
      )
      max_count <- max(hist_data$data[[1]]$count)
      # Round up to nearest 100
      max_count_rounded <- ceiling(max_count/100) * 100
      
      # Define a function to calculate dynamic breaks (adjust as needed)
      # y_breaks <- pretty(c(0, max_count), n = 10)  # 'pretty()' automatically generates "nice" breaks
      y_breaks <- pretty(c(0, max_count_rounded), n = 10)
      
      methylationHistogram <- methylationDF %>% 
        ggplot(aes(x = Percent_Methylation)) +
        stat_bin(
          color = "black",
          fill = "#3c8dbc",
          geom = "bar",
          position = "stack",
          binwidth = 10,
          # breaks = seq(0,100,10),
          na.rm = T,
          boundary = 0, 
          closed = "left"
        ) +
        stat_bin(binwidth = 10, geom='text', color='black', aes(label=after_stat(round(100 * count / sum(count), 1))),
                 vjust=-0.5, boundary = 0, closed = "left") +
        theme_classic() + labs(
        title = "Histogram of % CpG methylation",
        subtitle = input$sampleHistograms,
        x = "% methylation per base",
        y = "Count"
      ) + scale_x_continuous(breaks = seq(0, 100, by = 20)) +
        # scale_y_continuous(breaks = seq(0, after_stat(round_any(max(count), accuracy = 100, f = ceiling)), by = 100)
        scale_y_continuous(breaks = y_breaks, expand = expansion(mult = c(0, 0.1))
        # scale_y_continuous(breaks = seq(0, 900, by = 100)
      ) + theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 15, vjust = -0.5),
        # axis.ticks.x = 
        axis.ticks.length.x = unit(5, "pt"),
        axis.text.x = element_text(vjust = -0.5),
        axis.ticks.length.y = unit(5, "pt"),
        axis.title.y = element_text(size = 15, vjust = 1.5, hjust = 0.5)
      )
      
      output$methylationHistogram <- renderPlot({
        methylationHistogram
      }, height = function() {
            session$clientData$output_methylationHistogram_width * 0.5
         }
      )
      
      ########################## coverage Histogram ##########################
      
      coverageDF <- data.frame(
        "Coverage" = dataMethylRaw$coverage,
        "log10_Coverage" = log(dataMethylRaw$coverage, base = 10)
      )
      
      # cov_data <- ggplot_build(
      #   ggplot(methylationDF, aes(x = Percent_Methylation)) +
      #     stat_bin(binwidth = 10, boundary = 0, closed = "left")
      # )
      # max_count <- max(cov_data$data[[1]]$count)
      # # Round up to nearest 100
      # max_count_rounded <- ceiling(max_count/100) * 100
      
      coverageHistogram <- coverageDF %>% ggplot(aes(x = log10_Coverage)) +
        stat_bin(
          color = "black",
          fill = "#28a745",
          geom = "bar",
          position = "stack",
          bins = 10,
          # breaks = seq(0,100,10),
          na.rm = T,
          boundary = 1, 
          closed = "left"
        ) + stat_bin(bins = 10, 
                 geom='text', color='black', aes(label=after_stat(round(100 * count / sum(count), 1))),
                 vjust=-0.5, boundary = 1, closed = "left") +
        theme_classic() + labs(
        title = "Histogram of CpG coverage",
        subtitle = input$sampleHistograms,
        x = "log10 of read coverage per base",
        y = "Count"
      ) + scale_x_continuous(breaks = seq(1, ceiling(max(coverageDF$log10_Coverage)), by = 0.5),
                           expand = c(0, 0),
                           limits = c(1,3)
      ) + scale_y_continuous(expand = expansion(mult = c(0, 0.1))
      ) + theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 15, vjust = -0.5),
        # axis.ticks.x = 
        axis.ticks.length.x = unit(5, "pt"),
        axis.text.x = element_text(vjust = -0.5),
        axis.ticks.length.y = unit(5, "pt"),
        axis.title.y = element_text(size = 15, vjust = 1.5, hjust = 0.5)
      )
      
      output$coverageHistogram <- renderPlot({
        coverageHistogram
      }, height = function() {
        session$clientData$output_coverageHistogram_width * 0.5
      }
      )

    }
  ) # close Event number 1
  
}) # close observe
