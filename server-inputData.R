inputDataReactive <- reactive({
  # waiter <- waiter::Waiter$new()
  # waiter$show()
  # on.exit(waiter$hide())
  # 
  # # Start from here:
  # queryList = parseQueryString(session$clientData$url_search)
  # if (is.list(queryList)){
  #   dataUrl <- queryList$data
  # } else {
  #   dataUrl <- NULL
  # }
  # urlDataRoot = c("/srv/gstore/projects", "/srv/GT/analysis/course_sushi/public/gstore/projects")
  # 
  # if (!is.null(dataUrl)) {
  #   dataDir <- file.path(urlDataRoot, dataUrl)
  #   dataDir <- dataDir[file.exists(dataDir)][1]
  #   if (!file.exists(dataDir)){
  #     # ezMail(paste("invalid dataDir: ", dataDir), subject="PopGen_Structure failed", to="gxtx_data_mngt@fgcz.ethz.ch")
  #     stop(paste("invalid dataDir", dataDir))
  #   }
  # } else {
  #   # dataDir <- "/srv/gstore/projects/p1535/DNAme_fun_mm_test1_60--over--40_2023-07-19--13-40-27/DNAme/DNAme"
    dataDir <- "/srv/gstore/projects/p1535/DNAme_fun_mm_test6_60--over--40_2023-07-20--13-14-42"
  # }
  
  # dataDir <- "/srv/gstore/projects/p1535/DNAme_fun_mm_test6_60--over--40_2023-07-20--13-14-42"
  # list.files(file.path(dataDir, "DNAme"))
  # fileList <- list.files(file.path(dataDir, "DNAme/CpG"), full.names = T)
  # readRDS("/srv/gstore/projects/p1535/DNAme_fun_mm_test6_60--over--40_2023-07-20--13-14-42/DNAme/CpG/dmRegions.rds")
  
  paramDF <- read_tsv(file.path(dataDir, "parameters.tsv"), skip = 1, col_names = F)
  param <- data.frame(t(paramDF[,2]), row.names = NULL)
  colnames(param) <- t(paramDF[,1])
  
  # list.files(dataDir)
  dataset <- read_tsv(file.path(dataDir, "input_dataset.tsv"), skip = 0, col_names = T)
  dataset <- as.data.frame(dataset)
  factors <- grep("\\[Factor]", colnames(dataset), value = FALSE)
  factorNames <- colnames(dataset)[grep("\\[Factor]", colnames(dataset))]
  colnames(dataset) <- gsub(" \\[.*", "", colnames(dataset)) %>% gsub(" ", "_", .)
  factorNames <- gsub(" \\[.*", "", factorNames) %>% gsub(" ", "_", .)
  factorLevels <- NULL
  for (i in seq_along(dataset[factors])){
    factorLevels[[i]] <- paste0(
      colnames(dataset[factors[[i]]]),
      ": ",
      levels(as.factor(dataset[, factors[i]])))
  }
  factorLevels <- unlist(factorLevels)
  
  colourList <- list()
  for (i in factors) {
    for (l in levels(as.factor(dataset[, i]))) {
      colourList[l] <- NA
    }
  }
  
  
  # dataDirMe <- file.path(dataDir, param$name)
  # dataDirMe <- file.path(dataDir, "DNAme")
  
  ## test for allCytosineContexts
  # better way?
  if (param$allCytosineContexts=="true") {
    contexts <- c("CpG", "CHG", "CHH")
    # returnListCHG <- list()
    # returnListCpG <- list()
  } else {
    contexts <- c("CpG")
  }

  # returnList <- list()
  # for (i in seq_along(contexts)) {
  #   rdsFiles <- list.files(file.path(dataDirMe, contexts[i]), pattern = ".rds", full.names = T)
  #   for (j in seq_along(rdsFiles)) {
  #     cn <- sub("rds", contexts[i], basename(rdsFiles[j]))
  #     cf <- readRDS(rdsFiles[j])
  #     returnList[[cn]] <- cf
  #   }
  # }
  
  # returnList <- list()
  # for (i in seq_along(contexts)) {
  #   currentList <- list()
  #   rdsFiles <- list.files(file.path(dataDirMe, contexts[i]), pattern = ".rds", full.names = T)
  #   for (j in seq_along(rdsFiles)) {
  #     cn <- sub("rds", contexts[i], basename(rdsFiles[j]))
  #     cf <- readRDS(rdsFiles[j])
  #     currentList[[cn]] <- cf
  #   }
  #   returnList[[contexts[i]]] <- currentList
  # }
  
  # returnList <- list()
  # for (i in seq_along(contexts)) {
  #   currentList <- list()
  #   rdsFiles <- list.files(file.path(dataDirMe, contexts[i]), pattern = ".rds", full.names = T)
  #   for (j in seq_along(rdsFiles)) {
  #     cn <- sub(".rds", "", basename(rdsFiles[j]))
  #     cf <- readRDS(rdsFiles[j])
  #     currentList[[cn]] <- cf
  #   }
  #   returnList[[contexts[i]]] <- currentList
  # }
  
  # list.files(file.path(dataDirMe, "CpG"))
  # dmRegions <- readRDS(file.path(dataDirMe, "CpG", "dmRegions.rds"))
  # bsseq <- readRDS(file.path(dataDirMe, "CpG", "bsseq.rds"))
  # significantLoci <- readRDS(file.path(dataDirMe, "CpG", "significantLoci.rds"))
  # dmLoci <- readRDS(file.path(dataDirMe, "CpG", "dmLoci.rds"))
  
  # assign(paste0("dmRegions", contexts[1]), readRDS(file.path(dataDirCpG, paste0("dmRegions", ".rds"))))
    
  

  # dataDirCpG <- "/srv/gstore/projects/p1535/DNAme_fun_mm_test1_60--over--40_2023-07-19--13-40-27/DNAme/DNAme/CpG"
  # greatResult_BP <- readRDS(file.path(dataDirCpG, paste0("greatResultBP_CpG", ".rds")))
  # greatResult_CC <- readRDS(file.path(dataDirCpG, paste0("greatResultCC_CpG", ".rds")))
  # greatResult_MF <- readRDS(file.path(dataDirCpG, paste0("greatResultMF_CpG", ".rds")))
  # ### for testing; from /scratch/DMRseq_JB_test4_2023-04-11--11-08-38_temp11099/dmrseq
  # 
  # ##### test data dmrseq
  # # generated from bash script
  # dataDirDMRseq <- "~/data/dmrseq"
  # # dataDirDMRseq <- "dmrseq"
  # dmRegionsFilePath <- file.path(dataDirDMRseq, "dmRegions.rds")
  # significantRegionsFilePath <- file.path(dataDirDMRseq, "significantRegions.rds")
  # bsseqFilePath <- file.path(dataDirDMRseq, "bsseq.rds")
  # bsseqFilteredFilePath <- file.path(dataDirDMRseq, "bsseqFiltered.rds")
  # dmRegions <- readRDS(dmRegionsFilePath)
  # significantRegions <- readRDS(significantRegionsFilePath)
  # bsseq <- readRDS(bsseqFilePath)
  # bsseqFiltered <- readRDS(bsseqFilteredFilePath)
  # saveRDS(BS.cancer.ex, file.path("~/data/dmrseq/bsseq_ex_data", "BS.cancer.ex.rds"))
  # saveRDS(BS.cancer.ex.fit, file.path("~/data/dmrseq/bsseq_ex_data", "BS.cancer.ex.fit.rds"))
  # saveRDS(dmrs0, file.path("~/data/dmrseq/bsseq_ex_data", "dmrs0.rds"))
  # saveRDS(dmrs, file.path("~/data/dmrseq/bsseq_ex_data", "dmrs.rds"))
  # 
  # # regions_dmrseq <- readRDS(file.path("~/data/dmrseq/bsseq_ex_data", "regions_dmrseq.rds"))
  # # BS.cancer.ex.red <- readRDS(file.path("~/data/dmrseq/bsseq_ex_data", "BS.cancer.ex.red.rds"))
  # 
  # ##### test data great
  # # generated from bash script
  # dataDirGreat_mm <- "~/data/great/mm"
  # # dataDirGreat_mm <- "great"
  # greatResult_BP <- readRDS(file.path(dataDirGreat_mm, paste0("greatResult_BP", ".rds")))
  # greatResult_CC <- readRDS(file.path(dataDirGreat_mm, paste0("greatResult_CC", ".rds")))
  # greatResult_MF <- readRDS(file.path(dataDirGreat_mm, paste0("greatResult_MF", ".rds")))
  # enrichmentTable_BP <- readRDS(file.path(dataDirGreat_mm, paste0("enrichmentTable_BP", ".rds")))
  # enrichmentTable_CC <- readRDS(file.path(dataDirGreat_mm, paste0("enrichmentTable_CC", ".rds")))
  # enrichmentTable_MF <- readRDS(file.path(dataDirGreat_mm, paste0("enrichmentTable_MF", ".rds")))
  # dataDirGreat_ath <- "~/data/great/ath"
  # greatResult_RE <- readRDS(file.path(dataDirGreat_ath, paste0("greatResult_RE", ".rds")))
  # greatResult_KE <- readRDS(file.path(dataDirGreat_ath, paste0("greatResult_KE", ".rds")))
  # enrichmentTable_RE <- readRDS(file.path(dataDirGreat_ath, paste0("enrichmentTable_RE", ".rds")))
  # enrichmentTable_KE <- readRDS(file.path(dataDirGreat_ath, paste0("enrichmentTable_KE", ".rds")))
  # 
  # ##### test data methylKit
  # # generated from RStudio
  # dataDirMethylKit <- "~/data/MethylKit/doc"
  # # dataDirMethylKit <- "methylKit"
  # methylRaw <- readRDS(file.path(dataDirMethylKit, paste0("methylRaw", ".rds")))
  # filteredMethylRaw <- readRDS(file.path(dataDirMethylKit, paste0("filteredMethylRaw", ".rds")))
  # normalizedMethylRaw <- readRDS(file.path(dataDirMethylKit, paste0("normalizedMethylRaw", ".rds")))
  # methylAll <- readRDS(file.path(dataDirMethylKit, paste0("methylAll", ".rds")))
  # # clusterDendrogram <- readRDS(file.path(dataDirMethylKit, paste0("clusterDendrogram", ".rds")))
  # # batchAssociation <- readRDS(file.path(dataDirMethylKit, paste0("batchAssociation", ".rds")))
  # diffMethLoci <- readRDS(file.path(dataDirMethylKit, paste0("diffMethLoci", ".rds")))
  # diffMeth_25p <- readRDS(file.path(dataDirMethylKit, paste0("diffMeth_25p", ".rds")))
  # diffMethPerChr <- readRDS(file.path(dataDirMethylKit, paste0("diffMethPerChr", ".rds")))
  # diffMeth_covariates <- readRDS(file.path(dataDirMethylKit, paste0("diffMeth_covariates", ".rds")))
  # gene.obj <- readRDS(file.path(dataDirMethylKit, paste0("gene.obj", ".rds")))
  # diffMeth_25p_annotated <- readRDS(file.path(dataDirMethylKit, paste0("diffMeth_25p_annotated", ".rds")))
  
  
  ##### test data Homer
  # generated from RStudio
  # all_motifs <- read_motif("~/data/homer/test3/homerMotifs.all.motifs")
  # all_motifs$fold_enrichment <- all_motifs$tgt_num / all_motifs$bgd_num
  
  # all_motifs <- readRDS("~/data/homer/all_motifs.rds")
  
  
  # motifPlots <- c(
  #   "~/git/DNAme_shiny/data/homerResults/motif10.logo.svg",
  #   "~/git/DNAme_shiny/data/homerResults/motif11.logo.svg",
  #   "~/git/DNAme_shiny/data/homerResults/motif12.logo.svg"
  # )
  
  # saveRDS(motifPlots, "~/git/DNAme_shiny/data/motifPlots.rds")
  # motifPlots <- readRDS("~/git/DNAme_shiny/data/motifPlots.rds")
  
  
  ### additional objects to use in several/all apps
  sampleNames <- unlist(strsplit(param$samples, ","))
  # returnList$sampleNames <- param$sampleNames
  myPalette <- colorRampPalette(colors = viridis::plasma(n = 7)[c(2, 4, 6)])
  # returnList$myPalette <- myPalette
  # returnList$testCovariate <- param$testCovariate
  # returnList$allCytosineContexts <- param$allCytosineContexts
  # returnList$dataDirMe <- param$dataDirMe
  # returnList$contexts <- contexts
  # returnList$param <- param
  
  ####### testData
  # data(BS.chr21)
  # 
  # # reorder samples to create a null comparison 
  # BS.null <- BS.chr21[1:20000,c(1,3,2,4)]
  # 
  # # add 100 DMRs
  # BS.chr21.sim <- simDMRs(bs=BS.null, num.dmrs=300)
  # regions <- BS.chr21.sim$gr.dmrs
  # regions$qval <- runif(300, min = 0, max = 0.5)
  # significant <- regions[regions$qval < 0.05, ]
  # # bsseq object with original null + simulated DMRs
  # saveRDS(regions, paste0("~/data/DNAme/CpG/", "regions", ".rds"))
  # saveRDS(significant, paste0("~/data/DNAme/CpG/hypo/regions/", "significant", ".rds"))
  # saveRDS(significant, paste0("~/data/DNAme/CpG/hyper/regions/", "significant", ".rds"))
  # saveRDS(BS.null, paste0("~/data/DNAme/CpG/", "bsseq", ".rds"))
  
  # pData(BS.chr21.sim$bs)$Treatment <- c(40, 40, 60, 60)
  # rownames(pData(BS.chr21.sim$bs)) <- c("test1", "test2", "control1", "control2")
  # saveRDS(BS.chr21.sim$bs, paste0("~/data/DNAme/CpG/", "bsseqFiltered", ".rds"))
  
  # 
  # methylBase <- dataSim(
  #   replicates = 4,
  #   sites = 10000,
  #   treatment = c(1,1,0,0),
  #   percentage = 10,
  #   effect = 25,
  #   alpha = 0.4,
  #   beta = 0.5,
  #   theta = 10,
  #   covariates = NULL,
  #   sample.ids = c("test1", "test2", "control1", "control2"),
  #   assembly = "hg18",
  #   context = "CpG",
  #   add.info = TRUE
  # )
  # 
  # loci <- calculateDiffMeth(methylBase[[1]])
  # significantLoci <- getMethylDiff(loci, difference=25, qvalue=0.01, type="all")
  # significantLoci <- as(significantLoci,"GRanges")
  # seqlevelsStyle(significantLoci) <- "UCSC"
  # loci <- as(loci,"GRanges")
  # seqlevelsStyle(loci) <- "UCSC"
  # 
  # significantLoci_hyper <- significantLoci[significantLoci$meth.diff > 0, ]
  # significantLoci_hypo <- significantLoci[significantLoci$meth.diff < 0, ]
  # 
  # saveRDS(significantLoci_hypo, paste0("~/data/DNAme/CpG/hypo/loci/", "significant", ".rds"))
  # saveRDS(significantLoci_hyper, paste0("~/data/DNAme/CpG/hyper/loci/", "significant", ".rds"))
  # 
  # 
  # 
  # greatResult_BP <- great(gr = significant, gene_sets = "BP", biomart_dataset = "hsapiens_gene_ensembl",
  #                         background = regions)
  # 
  # greatResult_CC <- great(gr = significant, gene_sets = "CC", biomart_dataset = "hsapiens_gene_ensembl",
  #                         background = regions)
  # greatResult_MF <- great(gr = significant, gene_sets = "MF", biomart_dataset = "hsapiens_gene_ensembl",
  #                         background = regions)
  # 
  # saveRDS(greatResult_BP, paste0("~/data/DNAme/CpG/hypo/regions/", "greatResult_BP", ".rds"))
  # saveRDS(greatResult_CC, paste0("~/data/DNAme/CpG/hypo/regions/", "greatResult_CC", ".rds"))
  # saveRDS(greatResult_MF, paste0("~/data/DNAme/CpG/hypo/regions/", "greatResult_MF", ".rds"))
  ####### 

  # methylationList <- c("hyper", "hypo")
  # extentList <- c("region", "loci")

  read_nested_rds_files <- function(folder_path) {
    files <- list.files(path = folder_path, full.names = TRUE, recursive = FALSE)
    objects <- vector("list", length(files))
    
    for (i in seq_along(files)) {
      file <- files[i]
      is_dir <- file.info(file)$isdir
      
      if (is_dir) {
        folder_name <- tools::file_path_sans_ext(basename(file))
        objects[[folder_name]] <- read_nested_rds_files(file)
      } else if (tolower(tools::file_ext(file)) == "rds") {
        file_name <- tools::file_path_sans_ext(basename(file))
        objects[[file_name]] <- readRDS(file)
      } else {
        objects[[i]] <- file
      }
    }
    return(objects)
  }
  
  # nestedResults <- read_nested_rds_files(file.path(dataDir, "DNAme"))
  # nestedResults <- nestedResults[contexts]
  
  nestedResults <- read_nested_rds_files("~/data/DNAme")
  nestedResults <- nestedResults[contexts]

  # nestedResults <- nestedResults[sapply(names(nestedResults), is.character)]
  
  # subset_results <- nestedResults[sapply(names(nestedResults), function(x) nchar(x) > 0)]
  # subset_results <- subset_results[sapply(names(subset_results[contexts]), function(x) nchar(x) > 0)]
  # 
  # filter_nested <- function(lst) {
  #   lapply(lst, function(x) {
  #     if (is.list(x)) {
  #       filtered <- filter_nested(x)
  #       filtered[sapply(names(filtered), function(name) nchar(name) > 0)]
  #     } else {
  #       x
  #     }
  #   })
  # }
  
  # subset_results <- filter_nested(nestedResults)
  # nestedResults <- nestedResults[contexts]
  
  # clean_empty_elements <- function(list_to_clean) {
  #   cleaned_list <- list()
  #   
  #   for (i in seq_along(list_to_clean)) {
  #     if (!is.null(list_to_clean[[i]])) {
  #       cleaned_list[[i]] <- list_to_clean[[i]]
  #     }
  #   }
  #   
  #   return(cleaned_list)
  # }
  
  
  # Specify the path to your "results" folder
  # results_path <- "path_to_results_folder"
  
  
  # loaded_nested_results_cleaned <- clean_empty_elements(loaded_nested_results)
  
  return(list(
    "sampleNames" = sampleNames,
    # "allCytosineContexts" = allCytosineContexts,
    "contexts" = contexts,
    "param" = param,
    "fileList" = fileList,
    "myPalette" = myPalette,
    "nestedResults" = nestedResults,
    "dataset" = dataset,
    "factors" = factors,
    "factorNames" = factorNames,
    # "countList" = countList,
    "factorLevels" = factorLevels,
    "colourList" = colourList
    )
  )
  # return(returnList)
  
  # return(list(
  #   "dmRegions" = dmRegions,
  #   "significantRegions" = significantRegions,
  #   "bsseq" = bsseq,
  #   "bsseqFiltered" = bsseqFiltered,
  #   "greatResult_BP" = greatResult_BP,
  #   "greatResult_CC" = greatResult_CC,
  #   "greatResult_MF" = greatResult_MF,
  #   "enrichmentTable_BP" = enrichmentTable_BP,
  #   "enrichmentTable_CC" = enrichmentTable_CC,
  #   "enrichmentTable_MF" = enrichmentTable_MF,
  #   "greatResult_RE" = greatResult_RE,
  #   "greatResult_KE" = greatResult_KE,
  #   "enrichmentTable_RE" = enrichmentTable_RE,
  #   "enrichmentTable_KE" = enrichmentTable_KE,
  #   "methylRaw" = methylRaw,
  #   "methylAll" = methylAll,
  #   # "motifPlots" = motifPlots,
  #   "all_motifs" = all_motifs,
  #   "myPalette" = myPalette,
  #   "testCovariate" = testCovariate
  #   # "BS.cancer.ex.red" = BS.cancer.ex.red,
  #   # "regions_dmrseq" = regions_dmrseq
  #   )
  # )
})
