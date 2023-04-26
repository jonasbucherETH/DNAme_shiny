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
  
  # getPercMethylation <- function(methylBase.obj){
  #   x = getData(methylBase.obj)
  #   meth.mat = 100 * x[, methylBase.obj@numCs.index]/(
  #     x[,methylBase.obj@numCs.index] + x[,methylBase.obj@numTs.index] )                                      
  #   names(meth.mat)=methylBase.obj@sample.ids
  #   rownames(meth.mat) <- NULL
  #   if(rowids){
  #     rownames(meth.mat)=as.character(paste(x[,1],x[,2],x[,3],sep=".") )
  #   }
  #   return(as.matrix(meth.mat))
  # }
  # 
  # g <- getCoverageStats(methylRaw[[2]],plot=F,both.strands=FALSE)
  # 
  # d1 <- getData(methylRaw[[1]])
  # meth.mat1 <- d1$numCs / (d1$numCs + d1$numTs)
  # 
  # mp <- getPercMethylation(methylRaw[[1]])
  # 
  # summary(methylRaw[[1]]$coverage)
  
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
