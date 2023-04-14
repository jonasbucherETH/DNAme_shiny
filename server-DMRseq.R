observe({
  dmRegions <- inputDataReactive()$dmRegions
  significantRegions <- inputDataReactive()$significantRegions
  bsseq <- inputDataReactive()$bsseq
  bsseqFiltered <- inputDataReactive()$bsseqFiltered
  testCovariate <- "Treatment"
  
  observeEvent( # Event number 0
    {
      input$xFactorEmpiricalDistribution
      input$bySample
    },
    ignoreInit = F, # If TRUE, then, when the eventified object is first created/initialized, don't trigger the action or (compute the value). The default is FALSE.
    ignoreNULL = T, # default = TRUE
    {
      
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
      }
      else {
        meth.levelsm$group <- meth.levelsm$sample
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
            xlab("Methylation Proportion") + theme_bw()
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
      # return(p1)
      output$empiricalDistributionPlot <- renderPlot({
        p1
      })
      

    }
  ) # close Event number 0
  
}) # close observe
