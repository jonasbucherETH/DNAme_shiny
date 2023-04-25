observe({
  dmRegions <- inputDataReactive()$dmRegions
  significantRegions <- inputDataReactive()$significantRegions
  
  observeEvent( # Event number 1
    {

    },
    ignoreInit = F, # If TRUE, then, when the eventified object is first created/initialized, don't trigger the action or (compute the value). The default is FALSE.
    ignoreNULL = T, # default = TRUE
    {
    
    }
  ) # close Event number 1
  
}) # close observe
