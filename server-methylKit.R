observe({
  methylRaw <- inputDataReactive()$methylRaw
  methylAll <- inputDataReactive()$methylAll
  sampleNames <- c("test1","test2","ctrl1","ctrl2")
  
  updateSelectInput(
    inputId = "sample",
    # label = "Select sample to display",
    choices = sampleNames,
    selected = sampleNames[1]
  )
  
  observeEvent( # Event number 1
    {
      input$sample
    },
    ignoreInit = F, # If TRUE, then, when the eventified object is first created/initialized, don't trigger the action or (compute the value). The default is FALSE.
    ignoreNULL = T, # default = TRUE
    {
      
      sampleIndex <- which(sampleNames == input$sample)
      
      dataMethylRaw <- getData(methylRaw[[sampleIndex]])
      totalCoverage <- sum(dataMethylRaw$coverage)
      methylationDF <- data.frame(
        "Percent_Methylation" = 100*dataMethylRaw$numCs / (dataMethylRaw$numCs + dataMethylRaw$numTs),
        "Coverage" = dataMethylRaw$coverage
      )
      
      
      methylationHistogram <- methylationDF %>% ggplot(aes(x = Percent_Methylation)) +
        stat_bin(
          color = "black",
          fill = "#0099F8",
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
        subtitle = input$sample,
        x = "% methylation per base",
        y = "Count"
      ) + scale_x_continuous(breaks = seq(0, 100, by = 20)) +
        # scale_y_continuous(breaks = seq(0, after_stat(round_any(max(count), accuracy = 100, f = ceiling)), by = 100))
        scale_y_continuous(breaks = seq(0, 900, by = 100)
      ) + theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5),
        axis.title.x = element_text(vjust = -0.5),
        # axis.ticks.x = 
        axis.ticks.length.x = unit(5, "pt"),
        axis.text.x = element_text(vjust = -0.5),
        axis.ticks.length.y = unit(5, "pt"),
        axis.title.y = element_text(vjust = 1.5, hjust = 0.5)
      )
      
      output$methylationHistogram <- renderPlot({
        methylationHistogram
      })
      
      # getMethylationStats(methylRaw[[sampleIndex]], plot=F, both.strands=FALSE)
      
    }
  ) # close Event number 1
  
}) # close observe
