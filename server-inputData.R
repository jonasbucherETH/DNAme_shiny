inputDataReactive <- reactive({
  ###### read test data ######
  dmrseq_rds <- readRDS("~/Desktop/methylator-galaxy/DNAme_shiny/data/test_data/combined_full/dmrseq.rds")
  rGREAT_rds <- readRDS("~/Desktop/methylator-galaxy/DNAme_shiny/data/test_data/combined_full/rGREAT.rds")
  monaLisa_rds <- readRDS("~/Desktop/methylator-galaxy/DNAme_shiny/data/test_data/combined_full/monaLisa.rds")
  ### output methylKit galaxy:
  # differential methylation: The bedgraph file contains differentially methylated bases/regions and the corresponding statistics.
  # differential methylation - subset: The bedgraph file contains the subset of differentially methylated bases/regions that satisfies the user defined thresholds with qvalue.cutoff and meth.cutoff.
  # number of hyper/hypo sites: The tabular file contains number of hyper/hypo methylated regions/bases.
  methylKit_rds <- readRDS("~/Desktop/methylator-galaxy/DNAme_shiny/data/test_data/combined_full/methylKit.rds")
  # dmLoci <- read.bed()
  # significantLoci <- read.bed()
  
  dataset <- pData(dmrseq_rds$bs_combined)
  
  myPalette <- colorRampPalette(colors = viridis::plasma(n = 7)[c(2, 4, 6)])
  sampleNames <- rownames(colData(dmrseq_rds$bs_combined))
  
  factors <- colnames(dataset)
  # factorNames <- colnames(dataset)[grep("\\[Factor]", colnames(dataset))]
  factorNames <- factors
  colnames(dataset) <- gsub(" \\[.*", "", colnames(dataset)) %>% gsub(" ", "_", .)
  factorNames <- gsub(" \\[.*", "", factorNames) %>% gsub(" ", "_", .)
  factorLevels <- NULL
  for (i in seq_along(dataset[factors])){
    factorLevels[[i]] <- paste0(
      colnames(dataset[factors[[i]]]),
      ": ",
      levels(as.factor(dataset[, factors[i]])))
  }
  # factorLevels <- c(unlist(factorLevels), rownames(dataset))
  factorLevels <- unlist(factorLevels)
  
  colourList <- list()
  for (i in factors) {
    for (l in levels(as.factor(dataset[, i]))) {
      colourList[l] <- NA
    }
  }
  # len_colourList <- length(colourList)
  for (i in rownames(dataset)) {
    colourList[i] <- NA
  }

  ### fill with Dark2 as default
  for (i in 1:length(colourList)) {
    colourList[i] <- brewer.pal(8, "Dark2")[i]
    # print(colourList[i])
  }
  
  
  print("###---### Reached checkpoint inputData ###---###")
  
  # print("factorLevels:", factorLevels)
  
  testCovariate <- "CellType"
  
  se <- monaLisa_rds
  
return(list(
    "dmrseq_rds" = dmrseq_rds,
    "rGREAT_rds" = rGREAT_rds,
    "monaLisa_rds" = monaLisa_rds,
    "methylKit_rds" = methylKit_rds,
    "myPalette" = myPalette,
    "dataset" = dataset,
    "sampleNames" = sampleNames,
    "factors" = factors,
    "factorNames" = factorNames,
    "factorLevels" = factorLevels,
    "colourList" = colourList,
    "testCovariate" = testCovariate,
    "se" <- se
  )
  )
})
