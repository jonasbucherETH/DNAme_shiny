library(ComplexHeatmap)
library(enrichplot)
library(GO.db)
library(AnnotationDbi)
library(ontologyPlot)  # For plotting the DAG
library(dplyr)
library(UpSetR)
# enrichplot::dotplot()

# goplot(greatResult_BP)
go_results <- getEnrichmentTables(greatResult_BP)
colnames(go_results)

significant_terms <- go_results[1:20,]
go_ids <- significant_terms$id

go_info <- AnnotationDbi::select(
  GO.db,
  keys = go_ids,
  columns = c("GOID", "TERM", "ONTOLOGY", "DEFINITION", "ONTOLOGY"),
  keytype = "GOID"
)

# Remove duplicates
go_info <- unique(go_info)

# Use 'ontologyPlot' to plot the DAG
ontologyPlot::onto_plot(go_ids, ontology = "BP", show.parents = F)
# Error in ontology$ancestors : $ operator is invalid for atomic vectors
