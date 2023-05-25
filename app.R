library("shiny")
library("shinymanager")
library("shinyjs")
library("tidyverse")
library("ggpubr")
library("plotly")
library("RColorBrewer")
# library("ComplexHeatmap")
# library("tximport")
# library("vsn")
# library("clusterProfiler")
library("DT")
# library("colourpicker")
# library("writexl")
# library("circlize")
# library("GO.db")
# library("shinydashboard")
# library("shinyBS")
# library("pixiedust")
library("ezRun")
# library("kableExtra")
# library("ggrepel")
# library("gplots")
# library("sortable")
# library("waiter")
# library("shinycssloaders")
# library("ggprism")
# library("ggbeeswarm")
# library("rstatix")
# library("gridExtra")
# library("shinytitle")
# library("shinylogs")

# JB libraries
library("dmrseq")
library("rGREAT")
library("circlize")
library("GetoptLong")
library("KEGGREST")
library("biomaRt")
library("BioMartGOGeneSets")
library("parallel")
library("AnnotationHub")
library("GenomicFeatures")
library("gridExtra")
library("cowplot")
# library("shinydashboardPlus")
library("bs4Dash")
library("dplyr")
library("plyr")
library("readxl")
library("methylKit")
library("shinyWidgets")
library("colourpicker")
library(htmltools)
library(data.table)
library(scales) # heatmap
library(svglite) # for Homer
library(ComplexHeatmap) # conflift with monaLisa
library(monaLisa) # for homerToPFMatrixList
library(InteractiveComplexHeatmap)

# console.error = function () {
#   require("system").stderr.write(Array.prototype.join.call(arguments, ' ') + '\n');
# };

reactiveConsole(TRUE)

# For secure login:
# library(digest)
# digest("password1", algo = "md5")
# credentials <- data.frame(
#   user = c("user1", "user2"),
#   password = c(
#     "password1", "password2"),
#   admin = c(FALSE, TRUE),
#   comment = "Login Page.",
#   stringsAsFactors = FALSE
# )

########
# spinner <- tagList(
#   spin_chasing_dots(),
#   span("Loading stuff...", style="color:white;")
# )
source("output_module.R")


ui <- dashboardPage(
  # skin = "black",
  fullscreen = T,
  # help = T,
  header = dashboardHeader(
    title = "DNAme"
    # titleWidth = 200
    # socialButton(
    #   url = "https://github.com/jonasbucherETH",
    #   type = "github"
    # )
    # tags$li(
    #   a(
    #     href = 'mailto:sequencing@fgcz.ethz.ch?subject=exploreDEG-shiny-app-feedback',
    #     "Request Features/Report Bugs"),
    #   class = "dropdown"
    # ),
    # tags$li(
    #   a(href = 'http://www.fgcz.ch',
    #     target = "_blank",
    #     img(src = 'fgcz_logo.png', title = "FGCZ", height = "30px"),
    #     style = "padding-top:10px; padding-bottom:5px;"),
    #   class = "dropdown"),
    # tags$li(
    #   a(href = 'http://www.ethz.ch/en.html',
    #     target = "_blank",
    #     img(src = 'eth_logo.png', title = "FGCZ", height = "22px"),
    #     style = "padding-top:13px; padding-bottom:10px;"),
    #   class = "dropdown"),
    # tags$li(
    #   a(href = 'http://www.uzh.ch/en.html',
    #     target = "_blank",
    #     img(src = 'University_of_Zurich_Logo.png', title = "FGCZ", height = "30px"),
    #     style = "padding-top:10px; padding-bottom:5px;"),
    #   class = "dropdown")
  ),
  sidebar = dashboardSidebar(
    width = 200,
    shinyjs::useShinyjs(),
    sidebarMenu(
      id = "tabs",
      menuItem(
        text = "DMR",
        tabName = "tab-DMRseq",
        icon = icon("chart-area")
      ),
      menuItem(
        text = "rGREAT",
        tabName = "tab-great",
        # startExpanded = TRUE,
        icon = icon("dna"),
        startExpanded = TRUE,
        # bs4SidebarMenuSubItem(
        #   text,
        #   tabName = NULL,
        #   href = NULL,
        #   newTab = NULL,
        #   icon = shiny::icon("angles-right"),
        #   selected = NULL
        # )
        menuSubItem(
          text = "Biological Processes",
          tabName = "BP",
          icon = icon("dna")
        ),
        menuSubItem(
          text = "Cellular Component",
          tabName = "CC",
          icon = icon("dna")
        ),
        menuSubItem(
          text = "Molecular Function",
          tabName = "MF",
          icon = icon("dna")
        )
      ),
      # menuItem(
      #   text = "Biological Processes",
      #   tabName = "BP",
      #   icon = icon("dna")
      # ),
      # menuItem(
      #   text = "Cellular Component",
      #   tabName = "CC",
      #   icon = icon("dna")
      # ),
      # menuItem(
      #   text = "Molecular Function",
      #   tabName = "MF",
      #   icon = icon("dna")
      # ),
        # menuItemOutput("menuItemReactome"),
        # menuItemOutput("menuItemKegg")
      menuItem(
        text = "methylKit",
        tabName = "tab-methylKit",
        icon = icon("chart-simple")
      ),
      menuItem(
        text = "Motif analysis",
        tabName = "tab-HOMER",
        icon = icon("chart-simple")
      )
    )
  ),
  body = dashboardBody(
    # use_tracking(),
    # tags$head(tags$link(rel = "shortcut icon", href = "sushi.png")),
    # tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "main.css")),
    # use_waiter(),
    # tags$head(
    #   tags$style(
    #     "body {overflow-y: hidden;}"
    #   )
    # ),
    tabItems(
      source("ui-DMRseq.R", local = TRUE)$value,
      # source("ui-great.R", local = TRUE)$value,
      # output_module_UI("output_BP"),
      # output_module_UI("output_CC"),
      # output_module_UI("output_MF"),
      tabItem(
        tabName = "BP",
        greatUI("output_BP")
      ),
      tabItem(
        tabName = "CC",
        greatUI("output_CC")
      ),
      tabItem(
        tabName = "MF",
        greatUI("output_MF")
      ),
      source("ui-methylKit.R", local = TRUE)$value,
      source("ui-HOMER.R", local = TRUE)$value
    )
  ),
  controlbar = dashboardControlbar()
  
  # controlbar = dashboardControlbar(
  #   id = "controlbar",
  #   controlbarMenu(
  #     id = "controlbarMenu",
  #     controlbarItem(
  #       "BP",
  #       "BP selected"
  #     ),
  #     controlbarItem(
  #       "MF",
  #       "MF selected"
  #     )
  #   )
  # )
)

server <- function(input, output, session) {
  source("server-inputData.R", local = TRUE)
  source("server-DMRseq.R", local = TRUE)
  # source("server-great.R", local = TRUE)
  # callModule(output_module, "output_BP", greatResult_BP, enrichmentTable_BP)
  # callModule(output_module, "output_CC", greatResult_CC, enrichmentTable_CC)
  # callModule(output_module, "output_MF", greatResult_MF, enrichmentTable_MF)
  
  greatResult_BP <- inputDataReactive()$greatResult_BP
  enrichmentTable_BP <- inputDataReactive()$enrichmentTable_BP
  greatResult_CC <- inputDataReactive()$greatResult_CC
  enrichmentTable_CC <- inputDataReactive()$enrichmentTable_CC
  greatResult_MF <- inputDataReactive()$greatResult_MF
  enrichmentTable_MF <- inputDataReactive()$enrichmentTable_MF
  
  greatServer(id = "output_BP", greatResult = greatResult_BP, enrichmentTable = enrichmentTable_BP)
  greatServer(id = "output_CC", greatResult = greatResult_CC, enrichmentTable = enrichmentTable_CC)
  greatServer(id = "output_MF", greatResult = greatResult_MF, enrichmentTable = enrichmentTable_MF)
  # if(!is.null(greatResult_RE)) {
  #   greatServer(id = "output_RE", greatResult = greatResult_RE, enrichmentTable = enrichmentTable_RE)
  # }
  # if(!is.null(greatResult_MF)) {
  #   greatServer(id = "output_MF", greatResult = greatResult_MF, enrichmentTable = enrichmentTable_MF)
  # }
  source("server-methylKit.R", local = TRUE)
  source("server-HOMER.R", local = TRUE)
}

breakStrings <- function(x, minSizeForBreak = 20, lb = "\n", nb = 2) {
  sapply(x, minSizeForBreak = minSizeForBreak, lb = lb, FUN = function(x, minSizeForBreak, lb) {
    if (nchar(x) <= minSizeForBreak) {
      return(x)
    }
    
    g <- gregexpr(" ", x)[[1]]
    if (length(g) == 0) {
      return(x)
    }
    if (length(g) == 1 & all(g == -1)) {
      return(x)
    }
    
    if (nb == 2) {
      mid <- nchar(x) / 2
      mid <- g[order(abs(g - mid))[1]]
      substr(x, mid, mid) <- lb
    } else if (nb == 3) {
      mid1 <- round(nchar(x) / 3)
      mid2 <- mid1 * 2
      mid1 <- g[order(abs(g - mid1))[1]]
      mid2 <- g[order(abs(g - mid2))[1]]
      substr(x, mid1, mid1) <- lb
      substr(x, mid2, mid2) <- lb
    }
    
    return(x)
  })
}

# ggplot_great = function(gr_all, gr_term = NULL, gr_full_len, term_id = NULL) {
#   
#   # op = par(no.readonly = TRUE)
#   # on.exit(suppressWarnings(par(op)))
#   # par(mfrow = c(1, 3), mar = c(6, 4, 4, 1), xpd = NA)
#   
#   using_term = !is.null(gr_term)
#   
#   df_all = data.frame(distTSS = unlist(gr_all$dist_to_TSS))
#   if(using_term) {
#     df_term = data.frame(distTSS = unlist(gr_term$dist_to_TSS))
#   }
#   
#   # make plots
#   if(using_term) {
#     tb = table(table(unlist(gr_term$annotated_genes)))
#     vt = numeric(10)
#     vt[as.numeric(names(tb))] = tb
#     vt[is.na(vt)] = 0
#     v = c(vt[1:9], sum(vt[10:length(vt)]))
#     names(v) = c(as.character(1:9), ">= 10")
#     v[is.na(v)] = 0
#     p = v/sum(v)
#     # pos = barplot(p, col = "black", xlab = "Number of associated regions per gene", ylab = "This term's genes", ylim = c(0, max(p)*1.5), main = qq("Number of associated regions per gene\nTerm: @{term_id}"))
#     # text(pos[, 1], p + 0.01, v, adj = c(0.5, 0), cex = 0.8)
#     df1 <- data.frame(x = names(p), y = p, v = v)
#     df1$x <- factor(df1$x, levels = df1$x)
#     p1 <- ggplot(df1, aes(x, y)) +
#       geom_col(fill = "black", color = "black") +
#       labs(x = "Number of associated regions per gene",
#            y = "This term's genes",
#            title = paste("Number of associated regions per gene\nTerm:", term_id)) +
#       ylim(0, max(p) * 1.25) +
#       geom_text(aes(label = v), vjust = -0.5, size = 3)
#   } else {
#     tb = table(table(unlist(gr_all$annotated_genes)))
#     v = c(gr_full_len - length(gr_all), tb["1"], tb["2"], sum(tb[as.numeric(names(tb)) > 2]))
#     names(v) = c("0", "1", "2", "> 3")
#     v[is.na(v)] = 0
#     p = v/sum(v)
#     df1 <- data.frame(x = names(p), y = p, v = v)
#     df1$x <- factor(df1$x, levels = df1$x)
#     # p1 <- ggplot(data.frame(x = names(p), y = p, v = v), aes(x, y)) +
#     p1 <- ggplot(df1, aes(x, y)) +
#       geom_col(fill = c("red", "grey", "grey", "grey"), color = "black") +
#       labs(x = "Number of associated genes per region",
#            y = "Genomic regions",
#            title = "Number of associated genes per region") +
#       ylim(0, max(p) * 1.25) +
#       geom_text(aes(label = v, color = x), vjust = -0.5, size = 3) +
#       scale_color_manual(values = c("red", "black", "black", "black")) +
#       guides(color = "none", fill = guide_legend(override.aes = list(color = c("grey", "red")), title = NULL))
#   }
#   p1 <- p1 + theme_classic()
#   p1 <- p1 + theme(
#     axis.title.x = element_text(vjust = -1),
#     axis.text.x = element_text(size = 10, vjust = -1)
#   )
#   ################ 2
#   v = cbind(
#     c("<-500"       = sum(df_all$distTSS < -500000),
#       "[-500, -50)" = sum(df_all$distTSS >= -500000 & df_all$distTSS < -50000),
#       "[-50, -5)"   = sum(df_all$distTSS >= -50000  & df_all$distTSS < -5000),
#       "[-5, 0]"     = sum(df_all$distTSS >= -5000   & df_all$distTSS < 0),
#       "0"           = sum(df_all$distTSS == 0),
#       "(0, 5]"      = sum(df_all$distTSS > 0       & df_all$distTSS <= 5000),
#       "(5, 50]"     = sum(df_all$distTSS > 5000    & df_all$distTSS <= 50000),
#       "(50, 500]"   = sum(df_all$distTSS > 50000   & df_all$distTSS <= 500000),
#       "> 500"       = sum(df_all$distTSS > 500000)))
#   
#   if(using_term) {
#     v = cbind( 
#       c("<-500"       = sum(df_term$distTSS < -500000),
#         "[-500, -50)" = sum(df_term$distTSS >= -500000 & df_term$distTSS < -50000),
#         "[-50, -5)"   = sum(df_term$distTSS >= -50000  & df_term$distTSS < -5000),
#         "[-5, 0]"     = sum(df_term$distTSS >= -5000   & df_term$distTSS < 0),
#         "0"           = sum(df_term$distTSS == 0),
#         "(0, 5]"      = sum(df_term$distTSS > 0       & df_term$distTSS <= 5000),
#         "(5, 50]"     = sum(df_term$distTSS > 5000    & df_term$distTSS <= 50000),
#         "(50, 500]"   = sum(df_term$distTSS > 50000   & df_term$distTSS <= 500000),
#         "> 500"       = sum(df_term$distTSS > 500000)), v)
#   }
#   
#   bins <- data.frame(
#     bin = c("<-500", "[-500, -50)", "[-50, -5)", "[-5, 0]", "0", "(0, 5]", "(5, 50]", "(50, 500]", "> 500"),
#     lower_bound = c(-Inf, -500000, -50000, -5000, 0, 0, 5000, 50000, 500000),
#     upper_bound = c(-500000, -50000, -5000, 0, 0, 5000, 50000, 500000, Inf)
#   )
#   
#   # Create a function to compute the bin counts
#   compute_counts <- function(df, bins) {
#     counts <- sapply(seq_len(nrow(bins)), function(i) {
#       sum(df$distTSS >= bins[i, "lower_bound"] & df$distTSS < bins[i, "upper_bound"])
#     })
#     counts <- as.data.frame(counts)
#     rownames(counts) <- bins$bin
#     return(counts)
#   }
#   
#   # Compute the bin counts
#   counts_all <- compute_counts(df_all, bins)
#   if (using_term) {
#     counts_term <- compute_counts(df_term, bins)
#   }
#   
#   # Compute the fractions
#   p_all <- counts_all / colSums(counts_all)
#   p_all[is.na(p_all)] <- 0
#   if (using_term) {
#     p_term <- counts_term / colSums(counts_term)
#     p_term[is.na(p_term)] <- 0
#   }
#   
#   # Combine the fractions into a data frame
#   if(using_term) {
#     # df_plot <- data.frame(
#     #   bin = rep(bins$bin, 2),
#     #   fraction = c(p_all, p_term),
#     #   set = rep(c("Set-wide", "This term"), each = nrow(bins)))
#     # colnames(df_plot) <- c("bin", "fraction", "set")
#     df_plot <- data.frame(
#       rep(bins$bin, 2),
#       rbind(p_all, p_term),
#       rep(c("Set-wide", "This term"), each = nrow(bins)),
#       rbind(counts_all, counts_term))
#     colnames(df_plot) <- c("bin", "fraction", "set", "count")  
#   } else {
#     df_plot <- data.frame(
#       rep(bins$bin, 1),
#       p_all,
#       counts_all)
#     colnames(df_plot) <- c("bin", "fraction", "count")
#   }
#   
#   df_plot$bin <- factor(df_plot$bin, levels = unique(df_plot$bin), ordered=TRUE)
#   
#   if(using_term) {
#     p2 <- ggplot(df_plot, aes(x = bin, y = fraction, fill = set)) +
#       geom_bar(position = "dodge", stat = "identity", color = "black") +
#       labs(x = "Distance to TSS (kb)",
#            y = "Region-gene associations (fraction)",
#            title = paste("Binned by orientation and distance to TSS\nTerm:", term_id)) +
#       ylim(0, max(df_plot$fraction) * 1.5) +
#       geom_text(aes(label = count), position = position_dodge(0.90), vjust = -0.5, size = 3) +
#       scale_fill_manual(values = c("blue", "green"))
#   } else {
#     p2 <- ggplot(df_plot, aes(x = bin, y = fraction)) +
#       geom_bar(position = "dodge", stat = "identity", fill = "blue", color = "black") +
#       labs(x = "Distance to TSS (kb)",
#            y = "Region-gene associations (fraction)",
#            title = "Binned by orientation and distance to TSS") +
#       ylim(0, max(df_plot$fraction) * 1.5) +
#       geom_text(aes(label = count), position = position_dodge(0.90), vjust = -0.5, size = 3)
#     # + scale_fill_manual(values = c("blue", "green"))
#   }
#   
#   ### arrow
#   p2 <- p2 + geom_segment(aes(x = 5, y = 0, xend = 5, yend = max(df_plot$fraction) * 1.25))
#   p2 <- p2 + geom_segment(aes(x = 5, y = max(df_plot$fraction) * 1.25, xend = 6, yend = max(df_plot$fraction) * 1.25),
#                           arrow = arrow(angle = 20, length = unit(0.5, "cm")))
#   p2 <- p2 + annotate("text", x = 5.5, y = max(df_plot$fraction) * 1.3, label = "TSS")
#   
#   p2 <- p2 + theme_classic()
#   p2 <- p2 + theme(
#     axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
#     legend.position = "topright") +
#     guides(fill="none")
#   
#   ######################################   3    #################################
#   # Compute the bin counts
#   
#   df_counts <- df_plot
#   df_counts$fraction[6] <- df_plot$fraction[4] + df_plot$fraction[6]
#   df_counts$fraction[7] <- df_plot$fraction[3] + df_plot$fraction[7]
#   df_counts$fraction[8] <- df_plot$fraction[2] + df_plot$fraction[8]
#   df_counts$fraction[9] <- df_plot$fraction[1] + df_plot$fraction[9]
#   if(using_term) {
#     df_counts$fraction[15] <- df_plot$fraction[13] + df_plot$fraction[15]
#     df_counts$fraction[16] <- df_plot$fraction[12] + df_plot$fraction[16]
#     df_counts$fraction[17] <- df_plot$fraction[11] + df_plot$fraction[17]
#     df_counts$fraction[18] <- df_plot$fraction[10] + df_plot$fraction[18]
#     df_counts <- df_counts[c(5:9, 14:18), ]
#   } else {
#     df_counts <- df_counts[c(5:9), ]
#   }
#   # df_counts <- df_counts[c(5:9, 14:18), ]
#   df_counts$bin <- factor(df_counts$bin, levels = unique(df_counts$bin), ordered=TRUE)
#   
#   if(using_term) {
#     p3 <- ggplot(df_counts, aes(x = bin, y = fraction, fill = set)) +
#       geom_bar(position = "dodge", stat = "identity", color = "black") +
#       labs(x = "Absolute distance to TSS (kb)",
#            y = "Region-gene associations (fraction)",
#            title = paste("Binned by absolute distance to TSS\nTerm:", term_id)) +
#       ylim(0, max(df_counts$fraction) * 1.5) +
#       scale_fill_manual(values = c("blue", "green"))
#   } else {
#     p3 <- ggplot(df_counts, aes(x = bin, y = fraction)) +
#       geom_bar(position = "dodge", stat = "identity", fill = "blue", color = "black") +
#       labs(x = "Absolute distance to TSS (kb)",
#            y = "Region-gene associations (fraction)",
#            title = "Binned by absolute distance to TSS") +
#       ylim(0, max(df_counts$fraction) * 1.5)
#     
#     # + scale_fill_manual(values = c("blue", "green"))
#   }
#   
#   ### arrows
#   # p3 <- p3 + geom_segment(aes(x = 0, y = 0, xend = 0, yend = max(df_counts$fraction) * 1.25),
#   #                         arrow = arrow(angle = 90, length = unit(0.5, "cm")))
#   
#   p3 <- p3 + theme_classic()
#   p3 <- p3 + theme(
#     axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
#     legend.position = "topright")
#   
#   ################ arrange plots
#   # p_all <- grid.arrange(p1, p2, p3, ncol = 3)
#   p_all <- list("p1" = p1, "p2" = p2, "p3" = p3)
#   
#   return(p_all)
#   # grid.arrange(p1, p2, ncol = 2)
# }

shinyApp(ui = ui, server = server)
