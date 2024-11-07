### libraries ###
library(dmrseq)
library(bsseq)
library(rGREAT)
library(methylKit)
library(monaLisa)
library("BSgenome")
library(BSgenome.Hsapiens.UCSC.hg19)
library(JASPAR2020)
library(TFBSTools)
library(GenomicRanges)
library(SummarizedExperiment)
library(ComplexHeatmap)
library(circlize)


###### dmrseq ######
# Load example BSseq data from the dmrseq package
data(BS.chr21) # human

# Run dmrseq on the example data (this might take some time)
regions <- dmrseq(bs = BS.chr21, testCovariate = "CellType")
dmRegions <- regions

# Extract candidate and significant regions
# candidate_regions <- regions
significant_regions <- regions[regions$qval < 0.05]
significantRegions <- significant_regions

###### rGREAT ######
greatResult_BP <- great(gr = significantRegions, gene_sets = "BP",
                        biomart_dataset = "hsapiens_gene_ensembl",
                        background = dmRegions, min_gene_set_size = 3)
greatResult_CC <- great(gr = significantRegions, gene_sets = "CC",
                        biomart_dataset = "hsapiens_gene_ensembl",
                        background = dmRegions, min_gene_set_size = 3)
greatResult_MF <- great(gr = significantRegions, gene_sets = "MF",
                        biomart_dataset = "hsapiens_gene_ensembl",
                        background = dmRegions, min_gene_set_size = 3)

###### methylKit ######
data(methylKit)

# The object 'methylRawList.obj' is a methylRawList
methylRawListDB <- methylRawList.obj
methylBaseDB <- unite(methylRawListDB, destrand = FALSE)

# Perform differential methylation analysis
myDiff <- calculateDiffMeth(methylBaseDB)

###### monaLisa ######

# Load motifs from JASPAR database
opts <- list(species = 9606, all_versions = FALSE, matrixtype = "PWM")
pwms <- getMatrixSet(JASPAR2020, opts)

overlaps <- findOverlaps(dmRegions, significantRegions)
bins <- rep("unchanged", length(dmRegions))
bins[queryHits(overlaps)] <- "up"
bins <- factor(bins)

lmrseqs <- getSeq(BSgenome.Hsapiens.UCSC.hg19, dmRegions)

# Run motif enrichment analysis
se <- calcBinnedMotifEnrR(seqs = lmrseqs, bins = bins,
                          genome = BSgenome.Hsapiens.UCSC.hg19,
                          pwmL = pwms)

plotMotifHeatmaps(x = se[1:20], which.plots = c("log2enr", "negLog10Padj"), 
                  width = 1.8, cluster = TRUE, maxEnr = 2, maxSig = 10,
                  show_seqlogo = TRUE)

# View the SummarizedExperiment object
# se[1:20]

###### save data ######
saveRDS(list(bs_combined = BS.chr21, 
             # bs_filtered = BS.chr21, 
             dmRegions = dmRegions,
             significantRegions = significantRegions),
             # significantRegions_hypo = significantRegions_hypo,
             # significantRegions_hyper = significantRegions_hyper), 
        file = "~/Desktop/methylator-galaxy/DNAme_shiny/data/test_data/combined_full/dmrseq.rds")

saveRDS(list(greatResult_BP = greatResult_BP, 
             greatResult_CC = greatResult_CC, 
             greatResult_MF = greatResult_MF), 
        file = "~/Desktop/methylator-galaxy/DNAme_shiny/data/test_data/combined_full/rGREAT.rds")

saveRDS(list(methylRawListDB = methylRawListDB, 
             myDiff = myDiff), 
        file = "~/Desktop/methylator-galaxy/DNAme_shiny/data/test_data/combined_full/methylKit.rds")

saveRDS(se,
        file = "~/Desktop/methylator-galaxy/DNAme_shiny/data/test_data/combined_full/monaLisa.rds")


