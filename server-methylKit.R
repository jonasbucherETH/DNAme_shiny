observe({
  methylRaw <- inputDataReactive()$methylRaw
  methylAll <- inputDataReactive()$methylAll
  # str_end <- rep(paste0("_", sampleIDs), times = 3)
  # str_sub(colnames(methylAll)[5:length(methylAll)], start = -2, end = -1) <- str_end
  sampleNames <- c("test1","test2","ctrl1","ctrl2")
  
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
  
  updateSelectizeInput( # maybe this is not needed (always give all options)
    # session = getDefaultReactiveDomain(),
    session = session,
    inputId = "selectizeColoursPCA",
    choices = sampleNames,
    selected = sampleNames,
    # selected = character(0),
    server = TRUE
    # server = TRUE
  )
  
  output$colourPanelPCA <- renderUI({
    levPCA <- sort(unique(input$selectizeColoursPCA)) # sorting so that "things" are unambigious
    # levPCA <- sampleNames
    # colourValuesPCA <- gg_fill_hue(length(levPCA))
    colsPCA <- brewer.pal(length(sampleNames), input$colorPalettePCA)
    
    # New IDs "colX1" so that it partly coincide with input$selectizePCA...
    lapply(seq_along(levPCA), function(i) {
      colourInput(inputId = paste0("colPCA_", levPCA[i]),
                  label = paste0("Choose colour for ", levPCA[i]),
                  value = colsPCA[i]
      )
    })
  })
  
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
  
  # observeEvent( # Event manual colours PCA
  #   {
  #     input$applyManualColoursPCA
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
      input$colorPalettePCA
      input$manualColoursPCA
      input$sampleLabelsPCA
      input$textSizePCA
      input$pointSizePCA
      input$pcaTitle
      # input$actionButtonColours
      #input$selectThemePCA
    },
    ignoreInit = T, # If TRUE, then, when the eventified object is first created/initialized, don't trigger the action or (compute the value). The default is FALSE.
    ignoreNULL = T, # default = TRUE
    {
      
      # manualColoursPCA <- paste0("c(", paste0("input$colPCA_", sort(input$selectizeColoursPCA), collapse = ", "), ")")
      # manualColoursPCA <- eval(parse(text = manualColoursPCA))
      
      # paletteColoursPCA <- brewer.pal(length(sampleNames), input$colorPalettePCA)
      
      # colourValuesPCA <- reactive({
      #   if(input$manualColoursPCA==TRUE) {
      #     cPCA <- paste0("c(", paste0("input$colPCA_", sort(input$selectizeColoursPCA), collapse = ", "), ")")
      #     cPCA <- eval(parse(text = cPCA))
      #   } else {
      #     # cPCA <- paletteColoursPCA
      #     cPCA <- brewer.pal(length(sampleNames), input$colorPalettePCA)
      #   }
      #   cPCA
      # })
      
      colourValuesPCA <- reactive({
        # print("up")
        # input$actionButtonColours
        clickCount()
        # if (input$actionButtonColours == 0) {
        if (clickCount() == 0) {
          cPCA <- brewer.pal(length(sampleNames), input$colorPalettePCA)
          # print("if")
        } else {
          cPCA <- paste0("c(", paste0("input$colPCA_", sort(input$selectizeColoursPCA), collapse = ", "), ")")
          cPCA <- eval(parse(text = cPCA))
          # print("else")
        }
        cPCA
      })
      
      # Observe the action button click event and update the reactive expression
      observeEvent(input$actionButtonColours, {
        # Increment the click count by 1
        clickCount(clickCount() + 1)
      })
      
      # colourValuesPCA <- brewer.pal(length(sampleNames), input$colorPalettePCA)
      # selectedPCs <- sort(input$pickFactorsPCA)

      # print(input$colorPalettePCA)
      plotMethPCA <- pcaTableMeth %>%
        # ggplot(aes(x = .data[[input$pickFactor1PCA]], y = .data[[input$pickFactor2PCA]], color = .data[[input$colorGroupPCA]], fill = .data[[input$colorGroupPCA]], shape = .data[[input$shapeGroupPCA]])) +
        # ggplot(aes(x = .data[[selectedPCs[1]]], y = .data[[selectedPCs[2]]], color = rownames(pcaTableMeth))) +
        ggplot(aes(x = .data[[input$pickFactor1PCA]], y = .data[[input$pickFactor2PCA]], color = rownames(pcaTableMeth))) +
        geom_point(size = as.numeric(input$pointSizePCA)) +
        scale_color_manual(values = colourValuesPCA())
      
      plotMethPCA <- plotMethPCA + theme_classic()
      
      if (input$sampleLabelsPCA) {
        plotMethPCA <- plotMethPCA +
          geom_text(
            aes(label = rownames(pcaTableMeth)),
            size = (input$textSizePCA / 3),
            hjust = 0.2, vjust = -1.5, check_overlap = T,
            show.legend = FALSE
          ) 
      }

      ### themes, axis labels ,legend etc
      plotMethPCA <- plotMethPCA + labs(
        title = input$pcaTitle,
        x = paste0(input$pickFactor1PCA, " (", sprintf("%.2f", pcaVarprop$pct[pcaVarprop$PC == input$pickFactor1PCA]), "% variance explained)"),
        y = paste0(input$pickFactor2PCA, " (", sprintf("%.2f", pcaVarprop$pct[pcaVarprop$PC == input$pickFactor2PCA]), "% variance explained)"),
        # x = paste0(selectedPCs[1], " (", sprintf("%.2f", pcaVarprop$pct[pcaVarprop$PC == selectedPCs[1]]), "% variance explained)"),
        # y = paste0(selectedPCs[2], " (", sprintf("%.2f", pcaVarprop$pct[pcaVarprop$PC == selectedPCs[2]]), "% variance explained)"),
        # color = input$colorGroupPCA, shape = input$shapeGroupPCA
        color = "Samples"
      ) + theme(
        axis.text.x = element_text(
          colour = "grey20", size = input$textSizePCA, angle = 0, hjust = .5,
          vjust = .5, face = "plain"
        ),
        axis.text.y = element_text(
          colour = "grey20", size = input$textSizePCA, angle = 0, hjust = 1,
          vjust = 0.5, face = "plain"
        ),
        axis.title.x = element_text(
          colour = "grey20", size = input$textSizePCA, angle = 0, hjust = .5,
          vjust = 0, face = "plain"
        ),
        axis.title.y = element_text(
          colour = "grey20", size = input$textSizePCA, angle = 90,
          hjust = .5, vjust = .5, face = "plain"
        ),
        legend.text = element_text(
          colour = "grey20", size = input$textSizePCA
        ),
        legend.title = element_text(
          colour = "grey20", size = input$textSizePCA
        ),
        title = element_text(colour = "grey20", size = input$textSizePCA, face = "bold", hjust = 0.5, margin = margin(b = 20))
      )
      
      plotMethPCA <- plotMethPCA +
        ylim(c(min(pcaTableMeth[[input$pickFactor2PCA]] * 1.1), max(pcaTableMeth[[input$pickFactor2PCA]] * 1.2))) +
        xlim(c(min(pcaTableMeth[[input$pickFactor1PCA]] * 1.1), max(pcaTableMeth[[input$pickFactor1PCA]] * 1.2)))
        # ylim(c(min(pcaTableMeth[[selectedPCs[2]]] * 1.1), max(pcaTableMeth[[selectedPCs[2]]] * 1.2))) +
        # xlim(c(min(pcaTableMeth[[selectedPCs[1]]] * 1.1), max(pcaTableMeth[[selectedPCs[1]]] * 1.2)))
      
        
      
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
      geom_col(aes(y = pct)) +
      geom_line(aes(y = pct_cum, group = 1)) +
      geom_point(aes(y = pct_cum)) +
      labs(x = "Principal component", y = "Fraction variance explained (%)") +
      scale_y_continuous(n.breaks = 20) +
      theme_classic(base_size = as.numeric(input$textSizePCA))
    pcaScree
    
  } 
  # , width = 500, height = 400
  )
  
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
      
      methylationHistogram <- methylationDF %>% ggplot(aes(x = Percent_Methylation)) +
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
        # scale_y_continuous(breaks = seq(0, after_stat(round_any(max(count), accuracy = 100, f = ceiling)), by = 100))
        scale_y_continuous(breaks = seq(0, 900, by = 100)
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
                           # scale_y_continuous(breaks = seq(0, after_stat(max(count)), by = 100),
                           #                    expand = c(0, 0),
                           #                    limits = c(0, after_stat(max(count)))
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
