inputDataReactive <- reactive({

  ### for testing; from /scratch/DMRseq_JB_test4_2023-04-11--11-08-38_temp11099/dmrseq
  
  ##### test data dmrseq
  # generated from bash script
  dataDirDMRseq <- "~/data/dmrseq"
  dmRegionsFilePath <- file.path(dataDirDMRseq, "dmRegions.rds")
  significantRegionsFilePath <- file.path(dataDirDMRseq, "significantRegions.rds")
  bsseqFilePath <- file.path(dataDirDMRseq, "bsseq.rds")
  bsseqFilteredFilePath <- file.path(dataDirDMRseq, "bsseqFiltered.rds")
  dmRegions <- readRDS(dmRegionsFilePath)
  significantRegions <- readRDS(significantRegionsFilePath)
  bsseq <- readRDS(bsseqFilePath)
  bsseqFiltered <- readRDS(bsseqFilteredFilePath)
  
  ##### test data rGEAT
  dataDirGreat <- "~/data/great"
  greatResultFilePath <- file.path(dataDirGreat, "greatResult.rds")
  enrichmentTableFilePath <- file.path(dataDirGreat, "enrichmentTable.rds")
  greatResult <- readRDS(greatResultFilePath)
  enrichmentTable <- readRDS(enrichmentTableFilePath)

  return(list(
    "dmRegions" = dmRegions,
    "significantRegions" = significantRegions,
    "bsseq" = bsseq,
    "bsseqFiltered" = bsseqFiltered,
    "greatResult" = greatResult,
    "enrichmentTable" = enrichmentTable
    )
  )
})