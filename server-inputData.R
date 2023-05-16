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
  
  ##### test data great
  # generated from bash script
  dataDirGreat_mm <- "~/data/great/mm"
  greatResult_BP <- readRDS(file.path(dataDirGreat_mm, paste0("greatResult_BP", ".rds")))
  greatResult_CC <- readRDS(file.path(dataDirGreat_mm, paste0("greatResult_CC", ".rds")))
  greatResult_MF <- readRDS(file.path(dataDirGreat_mm, paste0("greatResult_MF", ".rds")))
  enrichmentTable_BP <- readRDS(file.path(dataDirGreat_mm, paste0("enrichmentTable_BP", ".rds")))
  enrichmentTable_CC <- readRDS(file.path(dataDirGreat_mm, paste0("enrichmentTable_CC", ".rds")))
  enrichmentTable_MF <- readRDS(file.path(dataDirGreat_mm, paste0("enrichmentTable_MF", ".rds")))
  dataDirGreat_ath <- "~/data/great/ath"
  greatResult_RE <- readRDS(file.path(dataDirGreat_ath, paste0("greatResult_RE", ".rds")))
  greatResult_KE <- readRDS(file.path(dataDirGreat_ath, paste0("greatResult_KE", ".rds")))
  enrichmentTable_RE <- readRDS(file.path(dataDirGreat_ath, paste0("enrichmentTable_RE", ".rds")))
  enrichmentTable_KE <- readRDS(file.path(dataDirGreat_ath, paste0("enrichmentTable_KE", ".rds")))
  
  ##### test data methylKit
  # generated from RStudio
  dataDirMethylKit <- "~/data/MethylKit/doc"
  methylRaw <- readRDS(file.path(dataDirMethylKit, paste0("methylRaw", ".rds")))
  filteredMethylRaw <- readRDS(file.path(dataDirMethylKit, paste0("filteredMethylRaw", ".rds")))
  normalizedMethylRaw <- readRDS(file.path(dataDirMethylKit, paste0("normalizedMethylRaw", ".rds")))
  methylAll <- readRDS(file.path(dataDirMethylKit, paste0("methylAll", ".rds")))
  clusterDendrogram <- readRDS(file.path(dataDirMethylKit, paste0("clusterDendrogram", ".rds")))
  batchAssociation <- readRDS(file.path(dataDirMethylKit, paste0("batchAssociation", ".rds")))
  diffMethLoci <- readRDS(file.path(dataDirMethylKit, paste0("diffMethLoci", ".rds")))
  diffMeth_25p <- readRDS(file.path(dataDirMethylKit, paste0("diffMeth_25p", ".rds")))
  diffMethPerChr <- readRDS(file.path(dataDirMethylKit, paste0("diffMethPerChr", ".rds")))
  diffMeth_covariates <- readRDS(file.path(dataDirMethylKit, paste0("diffMeth_covariates", ".rds")))
  gene.obj <- readRDS(file.path(dataDirMethylKit, paste0("gene.obj", ".rds")))
  diffMeth_25p_annotated <- readRDS(file.path(dataDirMethylKit, paste0("diffMeth_25p_annotated", ".rds")))
  
  
  ##### test data Homer
  # generated from RStudio
  all_motifs <- readRDS("~/data/homer/all_motifs.rds")
  
  
  # motifPlots <- c(
  #   "~/git/DNAme_shiny/data/homerResults/motif10.logo.svg",
  #   "~/git/DNAme_shiny/data/homerResults/motif11.logo.svg",
  #   "~/git/DNAme_shiny/data/homerResults/motif12.logo.svg"
  # )
  
  # saveRDS(motifPlots, "~/git/DNAme_shiny/data/motifPlots.rds")
  # motifPlots <- readRDS("~/git/DNAme_shiny/data/motifPlots.rds")
  
  
  ### additional objects to use in several/all apps
  myPalette <- colorRampPalette(colors = viridis::plasma(n = 7)[c(2, 4, 6)])
  
  return(list(
    "dmRegions" = dmRegions,
    "significantRegions" = significantRegions,
    "bsseq" = bsseq,
    "bsseqFiltered" = bsseqFiltered,
    "greatResult_BP" = greatResult_BP,
    "greatResult_CC" = greatResult_CC,
    "greatResult_MF" = greatResult_MF,
    "enrichmentTable_BP" = enrichmentTable_BP,
    "enrichmentTable_CC" = enrichmentTable_CC,
    "enrichmentTable_MF" = enrichmentTable_MF,
    "greatResult_RE" = greatResult_RE,
    "greatResult_KE" = greatResult_KE,
    "enrichmentTable_RE" = enrichmentTable_RE,
    "enrichmentTable_KE" = enrichmentTable_KE,
    "methylRaw" = methylRaw,
    "methylAll" = methylAll,
    # "motifPlots" = motifPlots,
    "all_motifs" = all_motifs,
    "myPalette" = myPalette
    )
  )
})
