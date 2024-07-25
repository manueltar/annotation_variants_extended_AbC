
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
  
  
  #### READ Table_S5 ----
  
  Table_S5<-readRDS(file=opt$Table_S5)
  
  
  cat("Table_S5_0\n")
  cat(str(Table_S5))
  cat("\n")
  cat(str(unique(Table_S5$VAR)))
  cat("\n")
  
  colnames(Table_S5)[which(colnames(Table_S5) == 'Rsid')]<-'rs'
  
  cat("Table_S5_1\n")
  cat(str(Table_S5))
  cat("\n")
  cat(str(unique(Table_S5$VAR)))
  cat("\n")
  
  Table_S5$chr_VAR<-gsub("_.+$","",Table_S5$VAR)
  Table_S5$pos<-gsub("^[^_]+_","",Table_S5$VAR)
  Table_S5$pos<-as.integer(gsub("_.+$","",Table_S5$pos))
  Table_S5$ref<-gsub("^[^_]+_[^_]+_","",Table_S5$VAR)
  Table_S5$ref<-gsub("_.+$","",Table_S5$ref)
  Table_S5$alt<-gsub("^[^_]+_[^_]+_[^_]+_","",Table_S5$VAR)
  
  Table_S5$chr_VAR<-factor(Table_S5$chr_VAR,
                       levels=c("chr1","chr2","chr3","chr4","chr5","chr6","chr7","chr8","chr9","chr10","chr11",
                                "chr12","chr13","chr14","chr15","chr16","chr17","chr18","chr19","chr20","chr21",
                                "chr22","chr23","chrX","chrY"), ordered=T)
  
  Table_S5$Allele<-factor(Table_S5$Allele,
                           levels=c("REF","ALT"), ordered=T)
  
  
  cat("Table_S5_1\n")
  cat(str(Table_S5))
  cat("\n")
  cat(str(unique(Table_S5$chr_VAR)))
  cat("\n")
  cat(str(unique(Table_S5$pos)))
  cat("\n")
  cat(str(unique(Table_S5$ref)))
  cat("\n")
  cat(str(unique(Table_S5$alt)))
  cat("\n")
  
  Table_S5<-Table_S5[order(Table_S5$chr_VAR,Table_S5$pos,Table_S5$start,Table_S5$stop,Table_S5$carried_variants,Table_S5$Allele),]
  
  cat("Table_S5_2\n")
  cat(str(Table_S5))
  cat("\n")
  cat(str(unique(Table_S5$chr_VAR)))
  cat("\n")
  cat(str(unique(Table_S5$pos)))
  cat("\n")
  cat(str(unique(Table_S5$ref)))
  cat("\n")
  cat(str(unique(Table_S5$alt)))
  cat("\n")
  
  
  indx.dep<-c(which(colnames(Table_S5) =='chr_VAR'),which(colnames(Table_S5) =='pos'),which(colnames(Table_S5) =='ref'),which(colnames(Table_S5) =='alt'))
  
  Table_S5_subset<-unique(Table_S5[,-indx.dep])
  
  cat("Table_S5_subset_0\n")
  cat(str(Table_S5_subset))
  cat("\n")
  cat(str(unique(Table_S5_subset$VAR)))
  cat("\n")



  #### path for graphs ----

  path_provisional_Tables<-paste(out,'Provisional_Tables','/',sep='')

  if (file.exists(path_provisional_Tables)){

  }else{

    dir.create(file.path(path_provisional_Tables))

  }#path_provisional_Tables

  setwd(path_provisional_Tables)


  saveRDS(file="Table_S5_Provisional.rds", Table_S5_subset)
  
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
    make_option(c("--Table_S7"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--Table_S5"), type="numeric", default=NULL, 
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