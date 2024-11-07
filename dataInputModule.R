# UI function (if needed)
dataInputUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        bs4Card(
          title = "Upload Data",
          width = NULL,
          id = ns("dataInputBox"),
          status = "primary",
          headerBorder = TRUE,
          solidHeader = TRUE,
          fluidRow(
            column(
              width = 6,
              h3("Upload dmrseq Data"),
              fileInput(ns("bsseqFile"), "Upload BSseq object (RDS file)", accept = c(".rds")),
              fileInput(ns("candidateRegionsFile"), "Upload candidateRegions (RDS file)", accept = c(".rds")),
              fileInput(ns("significantRegionsFile"), "Upload significantRegions (RDS file)", accept = c(".rds")),
              
              h3("Upload methylKit Data"),
              fileInput(ns("methylRawListDBFile"), "Upload methylRawListDB (RDS file)", accept = c(".rds")),
              fileInput(ns("methylDiffFile"), "Upload methylDiff (RDS file)", accept = c(".rds"))
            ),
            column(
              width = 6,
              h3("Upload rGREAT Data"),
              fileInput(ns("rGREAT_BP_File"), "Upload rGREAT Biological Process (RDS file)", accept = c(".rds")),
              fileInput(ns("rGREAT_CC_File"), "Upload rGREAT Cellular Component (RDS file)", accept = c(".rds")),
              fileInput(ns("rGREAT_MF_File"), "Upload rGREAT Molecular Function (RDS file)", accept = c(".rds")),
              
              h3("Upload monaLisa Data"),
              fileInput(ns("monaLisa_se_File"), "Upload monaLisa summarizedExperiment (RDS file)", accept = c(".rds")),
              
              h3("Upload Sample Datasheet"),
              fileInput(ns("sampleDataSheetFile"), "Upload sample datasheet (CSV or RDS file)", accept = c(".csv"))
            )
          ),
          fluidRow(
            column(
              width = 6,
              # UI output for testCovariate selectInput
              uiOutput(ns("testCovariateUI")),
            ),
            column(
              width = 6,
              # Action button to load data
              actionButton(ns("loadData"), "Load Data")
            )
          )
        )
      )
    )
  )
}

# Server function
dataInputServer <- function(id, dataValues, sharedValues) {
  moduleServer(
    id, 
    function(input, output, session) {
      ns <- session$ns
      
      testing_mode <- TRUE
      
      loadRDSFile <- function(file) {
        if (is.null(file)) {
          return(NULL)
        }
        readRDS(file$datapath)
      }
      
      # Function to load dataset (CSV or RDS)
      loadDatasetFile <- function(file) {
        if (is.null(file)) {
          return(NULL)
        }
        ext <- tools::file_ext(file$name)
        switch(ext,
               csv = read.csv(file$datapath, stringsAsFactors = FALSE),
               rds = readRDS(file$datapath),
               {
                 showNotification("Invalid file type for dataset. Please upload a CSV or RDS file.", type = "error")
                 return(NULL)
               }
        )
      }
      
      if (testing_mode) {
        dmrseq_rds <- readRDS("~/Desktop/methylator-galaxy/DNAme_shiny/data/test_data/combined_full/dmrseq.rds")
        rGREAT_rds <- readRDS("~/Desktop/methylator-galaxy/DNAme_shiny/data/test_data/combined_full/rGREAT.rds")
        monaLisa_rds <- readRDS("~/Desktop/methylator-galaxy/DNAme_shiny/data/test_data/combined_full/monaLisa.rds")
        methylKit_rds <- readRDS("~/Desktop/methylator-galaxy/DNAme_shiny/data/test_data/combined_full/methylKit.rds")
        testCovariate <- "CellType"
        se_monaLisa <- monaLisa_rds
        bsseq <- dmrseq_rds$bs_combined
        candidateRegions <- dmrseq_rds$dmRegions
        significantRegions <- dmrseq_rds$significantRegions
        methylRawListDB <- methylKit_rds$methylRawListDB
        methylDiff <- methylKit_rds$myDiff
        greatResult_BP <- rGREAT_rds$greatResult_BP
        greatResult_CC <- rGREAT_rds$greatResult_CC
        greatResult_MF <- rGREAT_rds$greatResult_MF

      } else { # if not testing mode
        # observeEvent(input$loadData, {
        #   # Load uploaded files or use default data
        #   
        #   # For dmrseq
        #   bsseq <- if (!is.null(input$bsseqFile)) {
        #     loadRDSFile(input$bsseqFile)
        #   } 
        #   
        #   candidateRegions <- if (!is.null(input$candidateRegionsFile)) {
        #     loadRDSFile(input$candidateRegionsFile)
        #   
        #   significantRegions <- if (!is.null(input$significantRegionsFile)) {
        #     loadRDSFile(input$significantRegionsFile)
        #   } 
        #     
        #   # For methylKit
        #   methylRawListDB <- if (!is.null(input$methylRawListDBFile)) {
        #     loadRDSFile(input$methylRawListDBFile)
        #   }
        #   
        #   methylDiff <- if (!is.null(input$methylDiffFile)) {
        #     loadRDSFile(input$methylDiffFile)
        #   }
        #   
        #   # For rGREAT
        #   rGREAT_BP <- if (!is.null(input$rGREAT_BP_File)) {
        #     loadRDSFile(input$rGREAT_BP_File)
        #   }
        #   
        #   rGREAT_CC <- if (!is.null(input$rGREAT_CC_File)) {
        #     loadRDSFile(input$rGREAT_CC_File)
        #   }
        #   
        #   rGREAT_MF <- if (!is.null(input$rGREAT_MF_File)) {
        #     loadRDSFile(input$rGREAT_MF_File)
        #   }
        #   
        #   # For monaLisa
        #   se_monaLisa <- if (!is.null(input$monaLisa_se_File)) {
        #     loadRDSFile(input$monaLisa_se_File)
        #   }
        #   
        #   # For dataset
        #   dataset <- if (!is.null(input$datasetFile)) {
        #     loadDatasetFile(input$datasetFile)
        #   } 
      } # end of if-else (testing data)
     
      

      
      sampleDataSheet <- pData(dmrseq_rds$bs_combined)
      myPalette <- colorRampPalette(colors = viridis::plasma(n = 7)[c(2, 4, 6)])
      sampleNames <- rownames(colData(dmrseq_rds$bs_combined))
      factors <- colnames(sampleDataSheet)
      factorNames <- factors
      colnames(sampleDataSheet) <- gsub(" \\[.*", "", colnames(sampleDataSheet)) %>% gsub(" ", "_", .)
      factorNames <- gsub(" \\[.*", "", factorNames) %>% gsub(" ", "_", .)
      factorLevels <- NULL
      for (i in seq_along(sampleDataSheet[factors])){
        factorLevels[[i]] <- paste0(
          colnames(sampleDataSheet[factors[[i]]]),
          ": ",
          levels(as.factor(sampleDataSheet[, factors[i]])))
      }
      factorLevels <- unlist(factorLevels)
      colourList <- list()
      for (i in factors) {
        for (l in levels(as.factor(sampleDataSheet[, i]))) {
          colourList[l] <- NA
        }
      }
      for (i in rownames(sampleDataSheet)) {
        colourList[i] <- NA
      }
      for (i in 1:length(colourList)) {
        colourList[i] <- brewer.pal(8, "Dark2")[i]
      }
      
      colorLevels <- c(factorLevels, rownames(sampleDataSheet))

      methylAll <- unite(methylRawListDB)
      
      # Render the testCovariate selectInput after dataset is loaded
      output$testCovariateUI <- renderUI({
        req(sampleDataSheet)
        # dataset <- datasetData()
        selectInput(ns("testCovariate"), "Select testCovariate", choices = colnames(sampleDataSheet))
      })
      
      # Ensure all required data is available
      req(bsseq, candidateRegions, significantRegions, methylRawListDB, methylDiff, greatResult_BP, greatResult_CC, greatResult_MF, se_monaLisa, sampleDataSheet)
      
      # Update sharedValues
      sharedValues$colourList <- colourList
      sharedValues$myPalette <- myPalette
      # Update dataValues
      dataValues$bsseq <- bsseq
      dataValues$candidateRegions <- candidateRegions
      dataValues$significantRegions <- significantRegions
      dataValues$methylRawListDB <- methylRawListDB
      dataValues$methylAll <- methylAll
      dataValues$methylDiff <- methylDiff
      dataValues$greatResult_BP <- greatResult_BP
      dataValues$greatResult_CC <- greatResult_CC
      dataValues$greatResult_MF <- greatResult_MF
      dataValues$se_monaLisa <- se_monaLisa
      dataValues$sampleDataSheet <- sampleDataSheet
      dataValues$sampleNames <- sampleNames
      dataValues$factors <- factors
      dataValues$factorNames <- factorNames
      dataValues$factorLevels <- factorLevels
      dataValues$colorLevels <- colorLevels
      dataValues$testCovariate <- testCovariate

    }
  )
}