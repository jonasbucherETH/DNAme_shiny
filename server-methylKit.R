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
      
      # sampleIndex <- which(sampleNames == input$sample)
      # getMethylationStats(methylRaw[[sampleIndex]], plot=F, both.strands=FALSE)
      
    }
  ) # close Event number 1
  
}) # close observe
