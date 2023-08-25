observe({
  # dmRegions <- inputDataReactive()$dmRegions
  # significantRegions <- inputDataReactive()$significantRegions
  # bsseq <- inputDataReactive()$bsseq
  # bsseqFiltered <- inputDataReactive()$bsseqFiltered
  nestedResults <- inputDataReactive()$nestedResults
  dataset <- inputDataReactive()$dataset
  factorNames <- inputDataReactive()$factorNames
  factorLevels <- inputDataReactive()$factorLevels
  factors <- inputDataReactive()$factors
  colourList <- inputDataReactive()$colourList
  
  bsseqFiltered <- nestedResults[["CpG"]][["bsseqFiltered"]]
  dmRegions <- nestedResults[["CpG"]][["regions"]]
  
  observeEvent( # Event contexts
    {
      input$selectContext
    },
    ignoreInit = F, # If TRUE, then, when the eventified object is first created/initialized, don't trigger the action or (compute the value). The default is FALSE.
    ignoreNULL = T, # default = TRUE
    {
      bsseqFiltered <- nestedResults[[input$selectContext]][["bsseqFiltered"]]
      dmRegions <- nestedResults[[input$selectContext]][["regions"]]
    }
  )
  # significantRegions <- nestedResults["significant"]
  
  # regions_dmrseq <- inputDataReactive()$regions_dmrseq
  # BS.cancer.ex.red <- inputDataReactive()$BS.cancer.ex.red
  myPalette <- inputDataReactive()$myPalette
  testCovariate <- param$grouping
  
  updateCardSidebar("sidebardmrseq")
  observeEvent(input$dmrseqTabCard, {
    # print(input$dmrseqTabCard)
    updateCardSidebar("sidebardmrseq")
  })
  
  # observeEvent(
  #   {
  #     input$dmrseqTabCard
  #   },
  #   ignoreInit = F, 
  #   ignoreNULL = F,
  #   {
  #     print(input$dmrseqTabCard)
  #     updateCardSidebar("sidebardmrseq")
  # })
  
  
  updateNumericInput(
    inputId = "selectRegion",
    label = "Select Region to plot",
    value = 1,
    min = 1,
    max = length(dmRegions),
    step = 1
    # width = NULL
  )
  
  # pData(bsseqFiltered)$col <- c(rep("#E69F00", ), rep("#0072B2", 3))
  

  updateSelectInput(
    session = session,
    inputId = "dmrFactor1",
    choices = c(colnames(dataset)[factors], "None")
    # choices = factorNames,
  )
  updateCheckboxGroupInput(
    session = session,
    inputId = paste0("dmrGroups"),
    choices = levels(as.factor(dataset[, factors])),
    selected = levels(as.factor(dataset[, factors]))
    # choices = dataset[, factorNames[i]],
    # selected = dataset[, factorNames[i]]
  )

  
  observeEvent(input[["dmrFactor1"]], ignoreInit = T, {
    updateCheckboxGroupInput(
      session = session,
      inputId = "dmrGroups",
      choices = levels(as.factor(dataset[[input[["dmrFactor1"]]]])),
      selected = levels(as.factor(dataset[[input[["dmrFactor1"]]]]))
    )
  })

  observeEvent( # Event plot DMRs
    {
      input$selectRegion
      input$dmrFactor1
      input$dmrGroups
      input$dmrFactor2
    },
    ignoreInit = F, # If TRUE, then, when the eventified object is first created/initialized, don't trigger the action or (compute the value). The default is FALSE.
    ignoreNULL = T, # default = TRUE
    {
      for (i in seq_along(factorLevels)) {
        colourList[i] <- paste0(col2hex(input[[paste0("GroupColour", i)]]), "FF")
      }
      colours <- NULL
      for (i in levels(as.factor(dataset[[input$dmrFactor1]]))) {
        colours[i] <- colourList[i]
      }
      colours <- colours[names(colours) %in% input$dmrGroups]
      pData(bsseqFiltered)$col <- colours
      
      output$dmrPlot <- renderPlot({
        plotDMRs(bsseqFiltered, regions=dmRegions[input$selectRegion,], testCovariate=testCovariate,
                 annoTrack=NULL)
      }, height = function() {
        session$clientData$output_dmrPlot_width * 0.4
        }
      )
    }
  ) # close Event plot DMRs

  testCovariate <- param$grouping
  # testCovariate <- paste(testCovariate, "[Factor]")
  print(testCovariate)
  
  observeEvent( # Event EmpiricalDistribution
    {
      input$xFactorEmpiricalDistribution
      input$bySample
      input$textSizeDMRseq
    },
    ignoreInit = F, # If TRUE, then, when the eventified object is first created/initialized, don't trigger the action or (compute the value). The default is FALSE.
    ignoreNULL = T, # default = TRUE
    {
      # waiter_show("waiterEmpiricalDistribution")
      
      # waiter_show(
      #   id = NULL,
      #   html = spin_1(),
      #   color = "#333e48",
      #   logo = "",
      #   image = "",
      #   hide_on_render = !is.null(id)
      # )
      
      
      ##### ----- Plot distribution of methylation values and coverage (plotEmpiricalDistribution)
      M <- Cov <- group <- wt <- NULL

      # if (is.null(testCovariate) & !input$bySample) {
      #   message("No testCovariate specified; plotting each sample separately.")
      #   input$bySample = TRUE
      # }
      
      bs <- bsseqFiltered
      meth.mat <- getCoverage(bs, type = "M")
      unmeth.mat <- getCoverage(bs, type = "Cov") - meth.mat
      meth.levelsm <- data.frame(meth.mat/(meth.mat + unmeth.mat))
      cov.matm <- data.frame((meth.mat + unmeth.mat))
      #### note: get testCovariate
      if (!is.null(testCovariate)) {
        if (sum(grepl(testCovariate, colnames(pData(bs)))) == 
            0) {
          stop("Error: no column in pData() found ", "that matches the testCovariate")
        }
        else if (length(grep(testCovariate, colnames(pData(bs)))) > 
                 1) {
          stop("Error: testCovariate matches more ", "than one column in pData()")
        }
        mC <- grep(testCovariate, colnames(pData(bs)))
        grouplab <- pData(bs)[, mC]
      }
      else {
        if (is.null(sampleNames(bs))) {
          grouplab <- as.character(seq_len(ncol(bs)))
        }
        else {
          grouplab <- sampleNames(bs)
        }
      }
      meth.levelsm <- utils::stack(meth.levelsm)
      colnames(meth.levelsm)[1] <- "M"
      meth.levelsm$Cov <- utils::stack(cov.matm)$values
      if (is.null(sampleNames(bs))) {
        meth.levelsm$sample <- sort(rep(seq_len(ncol(bs)), nrow(bs)))
      }
      else {
        meth.levelsm$sample <- unlist(lapply(sampleNames(bs), 
                                             function(x) rep(x, nrow(bs))))
      }
      if (!is.null(testCovariate)) {
        meth.levelsm$group <- unlist(lapply(seq_len(ncol(bs)), 
                                            function(x) rep(pData(bs)[x, mC], nrow(bs))))
        meth.levelsm$group <- as.factor(meth.levelsm$group)
      }
      else {
        meth.levelsm$group <- meth.levelsm$sample
        meth.levelsm$group <- as.factor(meth.levelsm$group)
      }
      if (!input$bySample) {
        if (input$xFactorEmpiricalDistribution == "M") {
          covtots <- rep(NA, ncol(cov.matm))
          names(covtots) <- grouplab
          for (l in unique(grouplab)) {
            covtots[names(covtots) == l] <- sum(colSums(cov.matm)[grouplab == 
                                                                    l])
          }
          wt.matm <- data.frame(t(t(cov.matm)/covtots))
          meth.levelsm$wt <- utils::stack(wt.matm)$values
          # p1 <- ggplot(meth.levelsm, aes(M, colour = group, 
          #                                group = group, weight = wt)) + geom_line(adjust = adj, 
          #                                                                         alpha = 0.6, stat = "density", size = 1.3) + 
          #   xlab("Methylation Proportion") + theme_bw()
          p1 <- ggplot(meth.levelsm, aes(M, colour = group, 
                                         group = group, weight = wt)) + geom_line(
                                                                                  alpha = 0.6, stat = "density", size = 1.3) + 
            xlab("Methylation Proportion") + theme_bw() +
            # scale_color_gradientn(
            #   colors = myPalette(n = length(unique(meth.levelsm$group)))
            # ) 
            # scale_color_gradientn(
            #   colors = brewer.pal(n = length(unique(meth.levelsm$group)), name = "Dark2")
            # ) 
            scale_color_manual(
              values = brewer.pal(n = length(unique(meth.levelsm$group)), name = "Dark2")
            ) 
        }
        else {
          # p1 <- ggplot(meth.levelsm, aes(Cov + 0.1, colour = group, 
          #                                group = group)) + geom_line(adjust = adj, alpha = 0.6, 
          #                                                            stat = "density", size = 1.3) + scale_x_continuous(trans = "log2") + 
          #   xlab("Coverage") + theme_bw()
          p1 <- ggplot(meth.levelsm, aes(Cov + 0.1, colour = group, 
                                         group = group)) + geom_line(alpha = 0.6, 
                                                                     stat = "density", size = 1.3) + scale_x_continuous(trans = "log2") + 
            xlab("Coverage") + theme_bw()
        }
        p1 <- p1 + labs(colour = "Group")
      }
      else {
        if (input$xFactorEmpiricalDistribution == "M") {
          wt.matm <- data.frame(t(t(cov.matm)/colSums(cov.matm)))
          meth.levelsm$wt <- utils::stack(wt.matm)$values
          if (identical(meth.levelsm$group, meth.levelsm$sample)) {
            p1 <- ggplot(meth.levelsm, aes(M, colour = group, 
                                           group = sample, weight = wt)) + labs(color = "Sample")
          }
          else {
            if (ncol(bs) <= 12) {
              p1 <- ggplot(meth.levelsm, aes(M, colour = group, 
                                             group = sample, weight = wt, linetype = sample)) + 
                labs(color = "Group", linetype = "Sample")
            }
            else {
              p1 <- ggplot(meth.levelsm, aes(M, colour = group, 
                                             group = sample, weight = wt)) + labs(color = "Group")
            }
          }
          # p1 <- p1 + geom_line(adjust = adj, alpha = 0.6, stat = "density", 
          #                      size = 1.3) + xlab("Methylation Proportion") + 
          #   theme_bw()
          p1 <- p1 + geom_line(alpha = 0.6, stat = "density", 
                               size = 1.3) + xlab("Methylation Proportion") + 
            theme_bw()

        }
        else {
          if (identical(meth.levelsm$group, meth.levelsm$sample)) {
            p1 <- ggplot(meth.levelsm, aes(Cov + 0.1, colour = group, 
                                           group = sample)) + labs(color = "Sample")
          }
          else {
            if (ncol(bs) <= 12) {
              p1 <- ggplot(meth.levelsm, aes(Cov + 0.1, colour = group, 
                                             group = sample, linetype = sample)) + labs(color = "Group", 
                                                                                        linetype = "Sample")
            }
            else {
              p1 <- ggplot(meth.levelsm, aes(Cov + 0.1, colour = group, 
                                             group = sample)) + labs(color = "Group")
            }
          }
          # p1 <- p1 + geom_line(adjust = adj, alpha = 0.6, stat = "density", 
          #                      size = 1.3) + scale_x_continuous(trans = "log2") + 
          #   xlab("Coverage") + theme_bw()
          
          
          p1 <- p1 + geom_line(alpha = 0.6, stat = "density", 
                               size = 1.3) + scale_x_continuous(trans = "log2") + 
            xlab("Coverage") + theme_bw()
        }
      }
      
      p1 <- p1 + theme(
        axis.text.x = element_text(
          colour = "grey20", size = input$textSizeDMRseq, angle = 0, hjust = .5,
          vjust = .5, face = "plain"
        ),
        axis.text.y = element_text(
          colour = "grey20", size = input$textSizeDMRseq, angle = 0, hjust = 1,
          vjust = 0.5, face = "plain"
        ),
        axis.title.x = element_text(
          colour = "grey20", size = input$textSizeDMRseq, angle = 0, hjust = .5,
          vjust = 0, face = "plain"
        ),
        axis.title.y = element_text(
          colour = "grey20", size = input$textSizeDMRseq, angle = 90,
          hjust = .5, vjust = .5, face = "plain"
        ),
        legend.text = element_text(
          colour = "grey20", size = input$textSizeDMRseq
        ),
        legend.title = element_text(
          colour = "grey20", size = input$textSizeDMRseq
        ),
        title = element_text(colour = "grey20", size = input$textSizeDMRseq, face = "bold", hjust = 0.5, margin = margin(b = 20))
      )
      
      # waiter_hide("waiterEmpiricalDistribution")
      
      # return(p1)
      output$empiricalDistributionPlot <- renderPlot({
        p1
      }, height = function() {
          session$clientData$output_empiricalDistributionPlot_width * 0.4
        }
      )
      
      output$downloadBtn <- downloadHandler(
        filename = paste0("empiricalDistributionPlot_", format(Sys.time(), "%Y%m%d%H%M%S"), ".", input$format),
        # content = function(file) {
        #   # Generate the plot with the desired size
        #   ggsave(file, width = input$width, height = input$height, device = file_ext)
        # },
        content = p1,
        contentType = input$format
      )

    }
  ) # close Event EmpiricalDistribution
  
  ######################################## download 
  # output$downloadBtn <- downloadHandler(
  #   filename = paste0("empiricalDistributionPlot_", format(Sys.time(), "%Y%m%d%H%M%S"), ".", input$format),
  #   # content = function(file) {
  #   #   # Generate the plot with the desired size
  #   #   ggsave(file, width = input$width, height = input$height, device = file_ext)
  #   # },
  #   content = p1,
  #   contentType = input$format
  # )
  
  # Handle download button click event
  # observeEvent(input$downloadBtn, {
  #   # Generate the filename based on the current timestamp
  #   filename <- paste0("empiricalDistributionPlot_", format(Sys.time(), "%Y%m%d%H%M%S"), ".", input$format)
  #   
  #   # Set the appropriate file extension and content type
  #   file_ext <- switch(input$format, png = "png", pdf = "pdf", svg = "svg")
  #   content_type <- switch(input$format, png = "image/png", pdf = "application/pdf", svg = "image/svg+xml")
  #   
  #   # Create the downloadable file
  #   output$downloadPlot <- downloadHandler(
  #     filename = filename,
  #     # content = function(file) {
  #     #   # Generate the plot with the desired size
  #     #   ggsave(file, width = input$width, height = input$height, device = file_ext)
  #     # },
  #     content = p1,
  #     contentType = content_type
  #   
  #   )
  #   
  #   # Trigger the download
  #   session$sendCustomMessage(type = "download", message = filename)
  # })
  
}) # close observe
