observe({
  dmrseq_rds <- inputDataReactive()$dmrseq_rds
  myPalette <- inputDataReactive()$myPalette
  sampleNames <- inputDataReactive()$sampleNames

  bsseqFiltered <- dmrseq_rds$bs_combined
  dmRegions <- dmrseq_rds$dmRegions
  significantRegions <- dmrseq_rds$significantRegions
  
  dataset <- inputDataReactive()$dataset
  factorNames <- inputDataReactive()$factorNames
  factorLevels <- inputDataReactive()$factorLevels
  factors <- inputDataReactive()$factors
  colourList <- inputDataReactive()$colourList
  
  testCovariate <- inputDataReactive()$testCovariate

  print("###---### dmrseq - Reached checkpoint 1 ###---###")
  
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
    choices = c(colnames(dataset[factors]), "None")
    # choices = factorNames,
  )
  updateCheckboxGroupInput(
    session = session,
    inputId = paste0("dmrGroups"),
    choices = levels(as.factor(dataset[, testCovariate])),
    selected = levels(as.factor(dataset[, testCovariate]))
    # choices = levels(as.factor(dataset[factors])),
    # selected = levels(as.factor(dataset[factors]))
    # choices = dataset[, factorNames],
    # selected = dataset[, factorNames]
  )

  
  observeEvent(input[["dmrFactor1"]], ignoreInit = T, {
    updateCheckboxGroupInput(
      session = session,
      inputId = "dmrGroups",
      choices = levels(as.factor(dataset[[input[["dmrFactor1"]]]])),
      selected = levels(as.factor(dataset[[input[["dmrFactor1"]]]]))
    )
  })
  
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
        colourList[i] <<- paste0(col2hex(input[[paste0("GroupColour", i)]]), "FF")
      }
      print("colourList")
      print(colourList)
    }
  )

  observeEvent( # Event plot DMRs
    {
      input$selectRegion
      # input$dmrFactor1
      # input$dmrGroups
      input$confirm_color_assignment
      input$annotation_track
      # input$dmrFactor2
    },
    ignoreInit = F, # If TRUE, then, when the eventified object is first created/initialized, don't trigger the action or (compute the value). The default is FALSE.
    ignoreNULL = T, # default = TRUE
    {
      print("###---### dmrseq - Reached checkpoint observeEvent plotDMRs ###---###")
      # colorLevels <- c(factorLevels, rownames(dataset))
      # for (i in seq_along(colorLevels)) {
      #   colourList[i] <- paste0(col2hex(input[[paste0("GroupColour", i)]]), "FF")
      # }
      colours <- NULL
      # for (i in levels(as.factor(dataset[[input$dmrFactor1]]))) {
      for (i in levels(as.factor(dataset[,testCovariate]))) {
        colours[i] <- colourList[i]
      }
      # colours <- colours[names(colours) %in% input$dmrGroups]
      print("colors dmrseq")
      for (i in seq_along(colours)) {
        print(i)
      }
      print(colours)
      
      if (any(sapply(colours, is.na))) {
        print("There are NA values in the list.")
      } else {
        print("There are NO NA values in the list.")
        pData(bsseqFiltered)$col <- colours
      }
      
      print(pData(bsseqFiltered)$col)
      
      # pData(bsseqFiltered)$col <- colours
      if (input$annotation_track == "") {
        annoTrack <- NULL
      } else {
        annoTrack <- getAnnot(input$annotation_track)
      }
      
      output$dmrPlot <- renderPlot({
        plotDMRs(bsseqFiltered, regions = dmRegions[input$selectRegion,], testCovariate = testCovariate,
                 annoTrack=annoTrack)
      }, height = function() {
        session$clientData$output_dmrPlot_width * 0.4
        }
      )
    }
  ) # close Event plot DMRs

  # testCovariate <- param$grouping
  # testCovariate <- paste(testCovariate, "[Factor]")
  # print(testCovariate)
  
  observeEvent( # Event EmpiricalDistribution
    {
      input$xFactorEmpiricalDistribution
      input$bySample
      debounce(input$textSizeDMRseq, millis = 1000)
      input$confirm_color_assignment
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
      # Extract current colourList
      currentColourList <- colourList()
      
      ##### ----- Plot distribution of methylation values and coverage (plotEmpiricalDistribution)
      M <- Cov <- group <- wt <- NULL
      
      bs <- bsseqFiltered
      meth.mat <- getCoverage(bs, type = "M")
      unmeth.mat <- getCoverage(bs, type = "Cov") - meth.mat
      meth.levelsm <- data.frame(meth.mat / (meth.mat + unmeth.mat))
      cov.matm <- data.frame((meth.mat + unmeth.mat))
      
      if (!is.null(testCovariate)) {
        if (sum(grepl(testCovariate, colnames(pData(bs)))) == 0) {
          stop("Error: no column in pData() found that matches the testCovariate")
        } else if (length(grep(testCovariate, colnames(pData(bs)))) > 1) {
          stop("Error: testCovariate matches more than one column in pData()")
        }
        mC <- grep(testCovariate, colnames(pData(bs)))
        grouplab <- pData(bs)[, mC]
      } else {
        if (is.null(sampleNames(bs))) {
          grouplab <- as.character(seq_len(ncol(bs)))
        } else {
          grouplab <- sampleNames(bs)
        }
      }
      
      meth.levelsm <- utils::stack(meth.levelsm)
      colnames(meth.levelsm)[1] <- "M"
      meth.levelsm$Cov <- utils::stack(cov.matm)$values
      
      if (is.null(sampleNames(bs))) {
        meth.levelsm$sample <- sort(rep(seq_len(ncol(bs)), nrow(bs)))
      } else {
        meth.levelsm$sample <- unlist(lapply(sampleNames(bs), function(x) rep(x, nrow(bs))))
      }
      
      if (!is.null(testCovariate)) {
        meth.levelsm$group <- unlist(lapply(seq_len(ncol(bs)), function(x) rep(pData(bs)[x, mC], nrow(bs))))
        meth.levelsm$group <- as.factor(meth.levelsm$group)
      } else {
        meth.levelsm$group <- meth.levelsm$sample
        meth.levelsm$group <- as.factor(meth.levelsm$group)
      }
      
      # Proceed with the plotting code
      if (!input$bySample) {
        if (input$xFactorEmpiricalDistribution == "M") {
          covtots <- rep(NA, ncol(cov.matm))
          names(covtots) <- grouplab
          for (l in unique(grouplab)) {
            covtots[names(covtots) == l] <- sum(colSums(cov.matm)[grouplab == l])
          }
          wt.matm <- data.frame(t(t(cov.matm) / covtots))
          meth.levelsm$wt <- utils::stack(wt.matm)$values
          p1 <- ggplot(meth.levelsm, aes(M, colour = group, group = group, weight = wt)) +
            geom_line(alpha = 0.6, stat = "density", size = 1.3) +
            xlab("Methylation Proportion") +
            theme_bw()
        } else {
          p1 <- ggplot(meth.levelsm, aes(Cov + 0.1, colour = group, group = group)) +
            geom_line(alpha = 0.6, stat = "density", size = 1.3) +
            scale_x_continuous(trans = "log2") +
            xlab("Coverage") +
            theme_bw()
        }
        p1 <- p1 + labs(colour = "Group")
      } else {
        if (input$xFactorEmpiricalDistribution == "M") {
          wt.matm <- data.frame(t(t(cov.matm) / colSums(cov.matm)))
          meth.levelsm$wt <- utils::stack(wt.matm)$values
          if (identical(meth.levelsm$group, meth.levelsm$sample)) {
            p1 <- ggplot(meth.levelsm, aes(M, colour = group, group = sample, weight = wt)) + labs(color = "Sample")
          } else {
            if (ncol(bs) <= 12) {
              p1 <- ggplot(meth.levelsm, aes(M, colour = group, group = sample, weight = wt, linetype = sample)) +
                labs(color = "Group", linetype = "Sample")
            } else {
              p1 <- ggplot(meth.levelsm, aes(M, colour = group, group = sample, weight = wt)) +
                labs(color = "Group")
            }
          }
          p1 <- p1 + geom_line(alpha = 0.6, stat = "density", size = 1.3) +
            xlab("Methylation Proportion") +
            theme_bw()
        } else {
          if (identical(meth.levelsm$group, meth.levelsm$sample)) {
            p1 <- ggplot(meth.levelsm, aes(Cov + 0.1, colour = group, group = sample)) + labs(color = "Sample")
          } else {
            if (ncol(bs) <= 12) {
              p1 <- ggplot(meth.levelsm, aes(Cov + 0.1, colour = group, group = sample, linetype = sample)) +
                labs(color = "Group", linetype = "Sample")
            } else {
              p1 <- ggplot(meth.levelsm, aes(Cov + 0.1, colour = group, group = sample)) +
                labs(color = "Group")
            }
          }
          p1 <- p1 + geom_line(alpha = 0.6, stat = "density", size = 1.3) +
            scale_x_continuous(trans = "log2") +
            xlab("Coverage") +
            theme_bw()
        }
      }
      
      # Apply color scale using currentColourList
      p1 <- p1 + scale_color_manual(
        values = unlist(currentColourList[levels(meth.levelsm$group)])
      )
      
      # Adjust font size
      font_size_ED <- debounce(reactive(input$textSizeDMRseq), 1000)()
      p1 <- p1 + theme(
        axis.text.x = element_text(size = font_size_ED),
        axis.text.y = element_text(size = font_size_ED),
        axis.title.x = element_text(size = font_size_ED),
        axis.title.y = element_text(size = font_size_ED),
        legend.text = element_text(size = font_size_ED),
        legend.title = element_text(size = font_size_ED),
        plot.title = element_text(size = font_size_ED)
      )
      
      # Render the empirical distribution plot
      output$empiricalDistributionPlot <- renderPlot({
        p1
      }, height = function() {
        session$clientData[[paste0("output_", ns("empiricalDistributionPlot"), "_width")]] * 0.4
      })
      
      # Download handler for the plot
      output$downloadBtn <- downloadHandler(
        filename = function() {
          paste0(
            "empiricalDistributionPlot_",
            format(Sys.time(), "%Y%m%d%H%M%S"),
            ".",
            input$format
          )
        },
        content = function(file) {
          ggsave(
            filename = file,
            plot = p1,
            width = input$width,
            height = input$height,
            device = input$format
          )
        }
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
