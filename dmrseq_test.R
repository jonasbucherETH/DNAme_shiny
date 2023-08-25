### to test stuff

##### -------- libraries
library("tidyverse")
# library("methylKit")
library("dmrseq")
library("ezRun")

# infile <- system.file("extdata/test_data.fastq_bismark.bismark.cov.gz",
#                       package = 'bsseq')
dataDirBismark <- "/srv/gstore/projects/p1535/Bismark_JBmm_test3_2023-03-27--15-58-43"

clumped_data_dir <- "~/data"
clumped_files <- list.files(path=clumped_data_dir, pattern = "clumped_",
                            recursive = T, full.names = T)

filesBismark <- list.files(dataDirBismark, pattern = "cov", full.names = T)
input_dataset <- read_tsv(file.path(dataDirBismark, "input_dataset.tsv"))
bsseqColData <- data.frame(input_dataset$Treatment, row.names = input_dataset$Name)
colnames(bsseqColData) <- "Treatment"
bsseq <- bsseq::read.bismark(files = filesBismark,
                             rmZeroCov = FALSE,
                             strandCollapse = FALSE,
                             verbose = FALSE,
                             colData = bsseqColData)


# for (i in clumped_files) {
#   file_name <- str_match(i, "data/\\s*(.*?)\\s*/clumped")[2]
#   file <- 
# }

input_dataset <- read_tsv(file.path(dataDirBismark, "input_dataset.tsv"))
# samples <- input_dataset$Name
files <- paste0(dataDirBismark, "/", samples, ".gz.bismark.cov.gz")
# yall <- readBismark2DGE(files, sample.names=samples) # dim = 20664 14 
bismarkBSseq <- read.bismark(files = files,
                             rmZeroCov = TRUE,
                             strandCollapse = FALSE,
                             verbose = TRUE)

bismarkBSseq
# An object of type 'BSseq' with
# 20664 methylation loci
# 7 samples
# has not been smoothed
# All assays are in-memory
sampleNames <- input_dataset$Name
sampleGroup <- factor(c(rep("BB", 3), rep("AN", 4)))
pData(bismarkBSseq)$Group <- sampleGroup


pData(bismarkBSseq)

loci.idx <- which(DelayedMatrixStats::rowSums2(getCoverage(bismarkBSseq, type="Cov")==0) == 0)
bismarkBSseq.filtered <- bismarkBSseq[loci.idx]
bismarkBSseq.filtered
# An object of type 'BSseq' with
# 104 methylation loci
# 7 samples
# has not been smoothed
# All assays are in-memory

pData(bismarkBSseq.filtered)

testCovariate <- "Group"
regions <- dmrseq(bs=bismarkBSseq.filtered,
                  cutoff = 0.05,
                  testCovariate=testCovariate)

library(GenomicFeatures)
library(RMariaDB)
txdb <- makeTxDbFromEnsembl(organism="Mus musculus",
                    release=NA,
                    circ_seqs=NULL,
                    server="ensembldb.ensembl.org",
                    username="anonymous", password=NULL, port=0L,
                    tx_attrib=NULL)
# txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene::TxDb.Hsapiens.UCSC.hg19.knownGene
broads <- GenomicFeatures::genes(txdb)
