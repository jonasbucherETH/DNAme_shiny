### to test stuff

##### -------- libraries
library("tidyverse")
# library("methylKit")
library("edgeR")
library("ezRun")
library("rtracklayer")


dataDirBismark <- "/srv/gstore/projects/p1535/Bismark_JB_test1_2023-03-07--15-47-45"


##### -------- edgeR 
# guide: https://bioconductor.org/packages/release/bioc/vignettes/edgeR/inst/doc/edgeRUsersGuide.pdf
# filter by count & DML (diff. methylated locus) analysis

A04_BB <- read.delim(file.path(dataDirBismark, "A04_BB.gz.bismark.cov.gz"), header=FALSE, nrows=6)

input_dataset <- read_tsv(file.path(dataDirBismark, "input_dataset.tsv"))
samples <- input_dataset$Name
files <- paste0(dataDirBismark, "/", samples, ".gz.bismark.cov.gz")
dataset <- read_tsv(file.path(dataDirBismark, "dataset.tsv"))
yall <- readBismark2DGE(files, sample.names=samples) # dim = 20664 14 

head(yall$counts)
head(yall$genes)
table(yall$genes$Chr) # 1-5

# order them (here already ordered)
ChrNames <- c(1:5)
yall$genes$Chr <- factor(yall$genes$Chr, levels=ChrNames)

o <- order(yall$genes$Chr, yall$genes$Locus)
yall <- yall[o,]

# yall$samples$group <- factor(input_dataset$Population)
yall$samples$group <- factor(c(rep("BB", 6), rep("AN", 8)))

# It is convenient to remove genomic segments that have not been assembled
# into any of the recognized chromosomes:

keep <- rep(TRUE, nrow(yall))
Chr <- as.character(yall$genes$Chr)
keep[ grep("random",Chr) ] <- FALSE
keep[ grep("chrUn",Chr) ] <- FALSE

table(keep) # all
yall <- yall[keep,, keep.lib.sizes=FALSE] # large DEGList

# We now annotate the CpG loci with the identity of the nearest gene.
# We search for the gene transcriptional start site (TSS) closest to each our CpGs:
TSS <- nearestTSS(yall$genes$Chr, yall$genes$Locus, species="Mm")
# Possible values are:
# "Hs" (human hg38), "Mm" (mouse mm10), "Rn" (rat), "Dm" (fly),
# "Dr" (zebra fish), "Ce" (worm), "Bt" (bovine), "Gg" (chicken),
#"Mmu" (rhesus), "Cf" (canine) or "Pt" (chimpanzee).

# alternative: (?)
pathReferenceBuild <- file.path("/srv/GT/reference", dataset$refBuild[1], "Genes")

### from app-HOMER
# param$ezRef <- NULL
# param <- ezParam(param)
# param <- pathReferenceBuild
# localAnnotation <- ezFeatureAnnotation(param, dataFeatureType="gene")
# localAnnotation <- unique(localAnnotation[, grep('^gene_id$|^description$|name$|symbol$|^type$',colnames(localAnnotation),ignore.case=TRUE)])
# 
# gtfFile = param$ezRef["refFeatureFile"]
gtfFile = file.path(pathReferenceBuild, "genes.gtf")
gtf = rtracklayer::import(gtfFile)
idx = gtf$type == 'gene'
if(!any(idx)){
  idx = gtf$type =='start_codon'
}
gtf = gtf[idx]
if(grepl('gtf$',gtfFile)){
  names_gtf = make.unique(gtf$'gene_id')
} else {
  names_gtf = make.unique(gtf$'ID')
}
names(gtf) = names_gtf
### end app-HOMER


library(BSgenome.Athaliana.TAIR.TAIR9)
seqNms <- seqnames(Athaliana)
## "Chr1" "Chr2" "Chr3" "Chr4" "Chr5" "ChrM" "ChrC"
AthalianaSeq <- getSeq(Athaliana, seqNms[1:5])

nearestReftoX_Athaliana <- nearestReftoX(yall$genes$Locus, AthalianaSeq)

# refBuild = "Saccharomyces_cerevisiae/Ensembl/EF4/Annotation/Version-2013-03-18"
# genomesRoot = "./refExample"
# param = ezParam(list(refBuild=refBuild, genomesRoot=genomesRoot))
# gtf = system.file("extdata/genes.gtf", package="ezRun", mustWork = TRUE)
# fp = system.file("extdata/genome.fa", package="ezRun", mustWork = TRUE)
# buildRefDir(param$ezRef, fp, gtf)
# buildIgvGenome(param$ezRef)
# seqAnno = writeAnnotationFromGtf(param=param)

refBuild = dataset$refBuild[1]
# genomesRoot = "./refExample"
param = ezParam(list(refBuild=refBuild))
gtf = system.file("extdata/genes.gtf", package="ezRun", mustWork = TRUE)
fp = system.file("extdata/genome.fa", package="ezRun", mustWork = TRUE)
buildRefDir(param$ezRef, fp, gtf)
buildIgvGenome(param$ezRef)
seqAnno = writeAnnotationFromGtf(param=param)

# featureFile=param$ezRef["refFeatureFile"], featAnnoFile=param$ezRef["refAnnotationFile"]
# referenceBuild <- 
# TSS <- nearestReftoX(yall$genes$Chr, referenceBuild) 

###
yall$genes$EntrezID <- TSS$gene_id
yall$genes$Symbol <- TSS$symbol
yall$genes$Strand <- TSS$strand
yall$genes$Distance <- TSS$distance
yall$genes$Width <- TSS$width
head(yall$genes)

Methylation <- gl(2,1,ncol(yall), labels=c("Me","Un"))
Me <- yall$counts[, Methylation=="Me"]
Un <- yall$counts[, Methylation=="Un"]
Coverage <- Me + Un
head(Coverage)

HasCoverage <- rowSums(Coverage >= 1) == 7 
head(HasCoverage)

HasBoth <- rowSums(Me) > 0 & rowSums(Un) > 0
table(HasCoverage, HasBoth)

y <- yall[HasCoverage & HasBoth,, keep.lib.sizes=FALSE]

# set the library sizes for each sample to be the average of the total
# read counts for the methylated and unmethylated libraries
TotalLibSize <- y$samples$lib.size[Methylation=="Me"] +
  y$samples$lib.size[Methylation=="Un"]
y$samples$lib.size <- rep(TotalLibSize, each=2)
y$samples


##### -------- Data exploration
Me <- y$counts[, Methylation=="Me"]
Un <- y$counts[, Methylation=="Un"]
M <- log2(Me + 2) - log2(Un + 2)
colnames(M) <- samples
plotMDS(M, col=rep(1:3, each=2), main="M-values")

# design matrix (4.7.5)
# from here on: missing "data"
designSL <- model.matrix(~0+group, data=y$samples)
colnames(designSL) <- levels(y$samples$group)
# colnames(designSL) <- c("AN", "BB")
designSL
# designSL <- cbind(Sample1 = c(1,1,0,0),
#                   Sample2 = c(0,0,1,1),
#                   A = c(1,0,0,0),
#                   B = c(0,0,1,0))
# designSL

design <- modelMatrixMeth(designSL)
design
# modelMatrixMeth()
# getCounts()

##### -------- Dispersion estimation (not essential; doesn't work)
# We estimate the NB dispersion for each CpG site using the estimateDisp function
y <- estimateDisp(y, design=design, trend="none")
y$common.dispersion
summary(y$prior.df)

##### -------- Testing for differentially methylated CpG loci
fit <- glmFit(y, design) # error: nrow(design) != ncol(y) (28, 14)
contr <- makeContrasts(LvsB=AN-BB,levels=design)
lrt<- glmLRT(fit,contrast=contr)
topTags(lrt)