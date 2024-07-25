
suppressMessages(library("plyr", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("data.table", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("crayon", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("withr", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("ggplot2", lib.loc = "/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("farver", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("labeling", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("optparse", lib.loc = "/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("dplyr", lib.loc = "/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("withr", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("backports", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("broom", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("rstudioapi", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("cli", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("tzdb", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("svglite", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("ggeasy", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("sandwich", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("digest", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("tidyverse", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("BiocGenerics", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("S4Vectors", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("IRanges", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("GenomeInfoDb", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("GenomicRanges", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("Biobase", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("AnnotationDbi", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("GO.db", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("org.Hs.eg.db", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("TxDb.Hsapiens.UCSC.hg19.knownGene", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("rtracklayer", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("cowplot", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("splitstackshape", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))


opt = NULL

options(warn = 1)

data_wrangling_MPRA = function(option_list)
{

  opt_in = option_list
  opt <<- option_list
  
  cat("All options:\n")
  printList(opt)
  
  
  #### READ and transform type ----
  
  type = opt$type
  
  cat("TYPE_\n")
  cat(sprintf(as.character(type)))
  cat("\n")

  
  #### READ and transform out ----
  
  out = opt$out
  
  cat("out_\n")
  cat(sprintf(as.character(out)))
  cat("\n")
  
  
  #### READ Multiome_DE ----
  
  Multiome_DE<-readRDS(file=opt$Multiome_DE)
  
  
  Multiome_DE$Analysis<-'DE'
  
  cat("Multiome_DE_0\n")
  cat(str(Multiome_DE))
  cat("\n")
  cat(str(unique(Multiome_DE$Symbol)))
  cat("\n")
 
  
  #### READ Multiome_DA ----
  
  Multiome_DA<-readRDS(file=opt$Multiome_DA)
  
  Multiome_DA$Analysis<-'DA'
  
  colnames(Multiome_DA)[which(colnames(Multiome_DA) == 'value_string')]<-'K562_Reg_build_overlap'
  
  
  
  cat("Multiome_DA_0\n")
  cat(str(Multiome_DA))
  cat("\n")
  cat(str(unique(Multiome_DA$Symbol_string)))
  cat("\n")
  
  Multiome_DA<-unique(as.data.frame(cSplit(Multiome_DA,sep = ';', direction = "long",
                                                     splitCols = "Symbol_string"),stringsAsFactors=F))
  
  DEBUG <- 1
  
  if(DEBUG ==1)
  {
    cat("Multiome_DA_1\n")
    cat(str(Multiome_DA))
    cat("\n")
    
  }
  
  colnames(Multiome_DA)[which(colnames(Multiome_DA) == 'Symbol_string')]<-'Symbol'
  
  if(DEBUG ==1)
  {
    cat("Multiome_DA_2\n")
    cat(str(Multiome_DA))
    cat("\n")
    cat(str(unique(Multiome_DA$Symbol)))
    cat("\n")
  }
  
  indx.dep<-c(which(colnames(Multiome_DA) == 'feature'),which(colnames(Multiome_DA) == 'activity'),which(colnames(Multiome_DA) == 'Total_peaks'))
  
  
  
  Multiome_DA_subset<-unique(Multiome_DA[,-indx.dep])
  
  if(DEBUG ==1)
  {
    cat("Multiome_DA_subset_0\n")
    cat(str(Multiome_DA_subset))
    cat("\n")
    cat(str(unique(Multiome_DA_subset$Symbol)))
    cat("\n")
  }
  
 
  #### Merge ----
  
  
  DEF<-merge(Multiome_DE,
             Multiome_DA_subset,
             by=c("Symbol","Analysis","baseMean","log2FoldChange","lfcSE","pvalue","padj","comparison","Minus_logpval","seurat_cluster"),
             all=T)
  
  if(DEBUG ==1)
  {
    cat("DEF_2\n")
    cat(str(DEF))
    cat("\n")
    cat(str(unique(DEF$Symbol)))
    cat("\n")
  }
  
  DEF_SIG<-DEF[which(DEF$Minus_logpval >= 1.3),]
  
  
  if(DEBUG ==1)
  {
    cat("DEF_SIG_0\n")
    cat(str(DEF_SIG))
    cat("\n")
    cat(str(unique(DEF_SIG$Symbol)))
    cat("\n")
  }
 
  
  #### path for graphs ----

  path_provisional_Tables<-paste(out,'Provisional_Tables','/',sep='')

  if (file.exists(path_provisional_Tables)){

  }else{

    dir.create(file.path(path_provisional_Tables))

  }#path_provisional_Tables

  setwd(path_provisional_Tables)


  saveRDS(file="Table_S8_Provisional.rds", DEF)
  saveRDS(file="Table_S8_SIG_Provisional.rds", DEF_SIG)
  
  
}




printList = function(l, prefix = "    ") {
  list.df = data.frame(val_name = names(l), value = as.character(l))
  list_strs = apply(list.df, MARGIN = 1, FUN = function(x) { paste(x, collapse = " = ")})
  cat(paste(paste(paste0(prefix, list_strs), collapse = "\n"), "\n"))
}


#### main script ----

main = function() {
  cmd_line = commandArgs()
  cat("Command line:\n")
  cat(paste(gsub("--file=", "", cmd_line[4], fixed=T),
            paste(cmd_line[6:length(cmd_line)], collapse = " "),
            "\n\n"))
  option_list <- list(
    make_option(c("--Multiome_DE"), type="numeric", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--Multiome_DA"), type="numeric", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--type"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--out"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required.")
  )
  parser = OptionParser(usage = "140__Rscript_v106.R
                        --subset type
                        --TranscriptEXP FILE.txt
                        --cadd FILE.txt
                        --ncboost FILE.txt
                        --type type
                        --out filename",
                        option_list = option_list)
  opt <<- parse_args(parser)
  
  data_wrangling_MPRA(opt)
  
  
}


###########################################################################

system.time( main() )