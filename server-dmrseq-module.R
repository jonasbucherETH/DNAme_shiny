dmrseqServer <- function(id, dataValues, sharedValues) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns  # Namespace function for IDs
      
      # Access variables from dataValues and sharedValues
      # dmrseq_rds <- dataValues$dmrseq_rds
      myPalette <- sharedValues$myPalette
      colourList <- sharedValues$colourList
      
      sampleNames <- dataValues$sampleNames
      bsseqFiltered <- dmrseq_rds$bs_combined
      # dmRegions <- dmrseq_rds$dmRegions
      # significantRegions <- dmrseq_rds$significantRegions
      # sampleDataSheet <- dataValues$sampleDataSheet
      factorNames <- dataValues$factorNames
      factorLevels <- dataValues$factorLevels
      factors <- dataValues$factors
      # testCovariate <- dataValues$testCovariate
      
      # colourList is in sharedValues
      # colourList <- reactiveVal(sharedValues$colourList)
      
      # print("###---### dmrseq - Reached checkpoint 1 ###---###")
      
      # Update the card sidebar (assuming 'sidebardmrseq' is a global ID)
      updateCardSidebar("sidebardmrseq")
      observeEvent(input$dmrseqTabCard, {
        updateCardSidebar("sidebardmrseq")
      })
      
      # Update numeric input for selecting the region
      updateNumericInput(
        session = session,
        inputId = "selectRegion",
        label = "Select Region to plot",
        value = 1,
        min = 1,
        max = length(dataValues$candidateRegions),
        step = 1
      )
      
      # Update select input for 'dmrFactor1'
      updateSelectInput(
        session = session,
        inputId = "dmrFactor1",
        choices = c(colnames(dataValues$sampleDataSheet[factors]), "None")
      )
      
      # Update checkbox group input for 'dmrGroups'
      updateCheckboxGroupInput(
        session = session,
        inputId = "dmrGroups",
        choices = levels(as.factor(dataValues$sampleDataSheet[, dataValues$testCovariate])),
        selected = levels(as.factor(dataValues$sampleDataSheet[, dataValues$testCovariate]))
      )
      
      # Update 'dmrGroups' when 'dmrFactor1' changes
      observeEvent(input$dmrFactor1, ignoreInit = TRUE, {
        updateCheckboxGroupInput(
          session = session,
          inputId = "dmrGroups",
          choices = levels(as.factor(dataValues$sampleDataSheet[[input$dmrFactor1]])),
          selected = levels(as.factor(dataValues$sampleDataSheet[[input$dmrFactor1]]))
        )
      })

      # Event to assign colors to groups
      ### unnecessary? already in overview
      # observeEvent(
      #   {
      #   input$confirm_color_assignment
      #   }, 
      #   ignoreInit = FALSE, 
      #   { 
      #   colorLevels <- c(factorLevels, rownames(dataValues$sampleDataSheet))
      #   newColourList <- sharedValues$colourList  # Copy existing colourList
      #   
      #   for (i in seq_along(colorLevels)) {
      #     color_input_id <- paste0("GroupColour", i)
      #     color_value <- input[[color_input_id]]
      #     newColourList[[colorLevels[i]]] <- paste0(col2hex(color_value), "FF")
      #   }
      #   
      #   # Update sharedValues$colourList
      #   sharedValues$colourList <- newColourList
      #   colourList(newColourList)  # Update reactiveVal
      #   print("Updated colourList:")
      #   print(sharedValues$colourList)
      # })
      
      observeEvent( # Event plot DMRs
        {
          input$selectRegion
          # input$dmrFactor1
          # input$dmrGroups
          # input$confirm_color_assignment
          sharedValues$colourList
          input$annotation_track
          # input$dmrFactor2
        },
        ignoreInit = F, # If TRUE, then, when the eventified object is first created/initialized, don't trigger the action or (compute the value). The default is FALSE.
        ignoreNULL = T, # default = TRUE
        {
          print("###---### dmrseq - Reached checkpoint observeEvent plotDMRs ###---###")
          
          # Get colours for the levels of testCovariate
          # currentColourList <- colourList()
          # colours <- currentColourList[levels(as.factor(dataValues$sampleDataSheet[, dataValues$testCovariate]))]
          colours <- sharedValues$colourList[levels(as.factor(dataValues$sampleDataSheet[, dataValues$testCovariate]))]

          
          if (any(sapply(colours, is.na))) {
            print("There are NA values in the color list.")
          } else {
            print("There are NO NA values in the color list.")
            pData(bsseqFiltered)$col <- colours
          }
          
          
          # Handle annotation track
          if (input$annotation_track == "") {
            annoTrack <- NULL
          } else {
            annoTrack <- getAnnot(input$annotation_track)
          }
          
          # Render the DMR plot
          output$dmrPlot <- renderPlot({
            plotDMRs(
              bsseqFiltered,
              regions = dataValues$significantRegions[input$selectRegion, ],
              testCovariate = dataValues$testCovariate,
              annoTrack = annoTrack
            )
          }, height = function() {
            session$clientData[[paste0("output_", ns("dmrPlot"), "_width")]] * 0.4
          })
        }
      ) # end Event dmrPlot
      
      # print(dataValues$testCovariate)
      color_inputs <- reactive({
        color_names <- names(input)[grepl("^GroupColour", names(input))]
        setNames(
          lapply(color_names, function(name) input[[name]]),
          sub("^GroupColour", "", color_names)
        )
      })
      # Event to plot Empirical Distribution
      observeEvent( # Event EmpiricalDistribution
        {
          input$xFactorEmpiricalDistribution
          input$bySample
          debounce(input$textSizeDMRseq, millis = 1000)
          # color_inputs()
          # input$confirm_color_assignment
          sharedValues$colourList
        },
        ignoreInit = F, # If TRUE, then, when the eventified object is first created/initialized, don't trigger the action or (compute the value). The default is FALSE.
        ignoreNULL = T, # default = TRUE
        {
          
          # print("start EmpiricalDistribution -------------")
        
          # Extract current colourList
          # currentColourList <- colourList()
          currentColourList <- sharedValues$colourList
          # print("colourList -------------")
          # print(currentColourList)
          
          # currentColourList <- colourList
          
          ##### ----- Plot distribution of methylation values and coverage (plotEmpiricalDistribution)
          M <- Cov <- group <- wt <- NULL
          
          bs <- bsseqFiltered
          meth.mat <- getCoverage(bs, type = "M")
          unmeth.mat <- getCoverage(bs, type = "Cov") - meth.mat
          meth.levelsm <- data.frame(meth.mat / (meth.mat + unmeth.mat))
          cov.matm <- data.frame((meth.mat + unmeth.mat))
          
          if (!is.null(dataValues$testCovariate)) {
            if (sum(grepl(dataValues$testCovariate, colnames(pData(bs)))) == 0) {
              stop("Error: no column in pData() found that matches the testCovariate")
            } else if (length(grep(dataValues$testCovariate, colnames(pData(bs)))) > 1) {
              stop("Error: testCovariate matches more than one column in pData()")
            }
            mC <- grep(dataValues$testCovariate, colnames(pData(bs)))
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
          
          if (!is.null(dataValues$testCovariate)) {
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
                xlab("Methylation Proportion") #+
                # theme_bw()
            } else {
              p1 <- ggplot(meth.levelsm, aes(Cov + 0.1, colour = group, group = group)) +
                geom_line(alpha = 0.6, stat = "density", size = 1.3) +
                scale_x_continuous(trans = "log2") +
                xlab("Coverage") #+
                # theme_bw()
            }
            p1 <- p1 + labs(colour = "Group")
          } else {
            if (input$xFactorEmpiricalDistribution == "M") {
              wt.matm <- data.frame(t(t(cov.matm) / colSums(cov.matm)))
              meth.levelsm$wt <- utils::stack(wt.matm)$values
              if (identical(meth.levelsm$group, meth.levelsm$sample)) {
                p1 <- ggplot(meth.levelsm, aes(M, colour = group, group = sample, weight = wt)) + labs(colour = "Sample")
              } else {
                if (ncol(bs) <= 12) {
                  p1 <- ggplot(meth.levelsm, aes(M, colour = group, group = sample, weight = wt, linetype = sample)) +
                    labs(colour = "Group", linetype = "Sample")
                } else {
                  p1 <- ggplot(meth.levelsm, aes(M, colour = group, group = sample, weight = wt)) +
                    labs(colour = "Group")
                }
              }
              p1 <- p1 + geom_line(alpha = 0.6, stat = "density", size = 1.3) +
                xlab("Methylation Proportion")# +
                # theme_bw()
            } else {
              if (identical(meth.levelsm$group, meth.levelsm$sample)) {
                p1 <- ggplot(meth.levelsm, aes(Cov + 0.1, colour = group, group = sample)) + labs(colour = "Sample")
              } else {
                if (ncol(bs) <= 12) {
                  p1 <- ggplot(meth.levelsm, aes(Cov + 0.1, colour = group, group = sample, linetype = sample)) +
                    labs(colour = "Group", linetype = "Sample")
                } else {
                  p1 <- ggplot(meth.levelsm, aes(Cov + 0.1, colour = group, group = sample)) +
                    labs(colour = "Group")
                }
              }
              p1 <- p1 + geom_line(alpha = 0.6, stat = "density", size = 1.3) +
                scale_x_continuous(trans = "log2") +
                xlab("Coverage") #+
                # theme_bw()
            }
          }
          
          # print(sharedValues$colourList)
          # Apply color scale using currentColourList
          p1 <- p1 + scale_colour_manual(
            values = unlist(sharedValues$colourList[levels(meth.levelsm$group)])
          )
          # p1 <- p1 + scale_colour_manual(
          #   values = unlist(currentColourList[levels(meth.levelsm$group)])
          # )
          
          # Adjust font size
          font_size_ED <- debounce(reactive(input$textSizeDMRseq), 1000)()
          # font_size_ED <- input$textSizeDMRseq
          
          # print("emp ---------")
          p1 <- p1 + theme_bw()

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
          # }, height = function() {
          #   session$clientData[[paste0("output_", ns("empiricalDistributionPlot"), "_width")]] * 0.4
          })
          
          # Download handler for the plot
          output$download_empiricalDistribution <- downloadHandler(
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
          ) # end of downloadBtn
      }) # end of Event to plot Empirical Distribution
      
    }
  )
}
