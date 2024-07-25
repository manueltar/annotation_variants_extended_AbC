
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

data_wrangling = function(option_list)
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
  
  #### READ VAR_to_gene_body_correspondence ----
  
  VAR_to_gene_body_correspondence<-readRDS(file=opt$VAR_to_gene_body_correspondence)
  
  # colnames(VAR_to_gene_body_correspondence)[which(colnames(VAR_to_gene_body_correspondence) == 'Symbol')]<-'HGNC'
  
  cat("VAR_to_gene_body_correspondence_0\n")
  cat(str(VAR_to_gene_body_correspondence))
  cat("\n")
  cat(str(unique(VAR_to_gene_body_correspondence$VAR)))
  cat("\n")
  
  indx.int<-c(which(colnames(VAR_to_gene_body_correspondence) == 'VAR'),which(colnames(VAR_to_gene_body_correspondence) == 'ensembl_gene_id'),
              which(colnames(VAR_to_gene_body_correspondence) == 'HGNC'), which(colnames(VAR_to_gene_body_correspondence) == 'regulated_gene_in_Gene_body_CLASS'))
  
  VAR_to_gene_body_correspondence_subset<-unique(VAR_to_gene_body_correspondence[,indx.int])
  
  cat("VAR_to_gene_body_correspondence_subset_0\n")
  cat(str(VAR_to_gene_body_correspondence_subset))
  cat("\n")
  cat(str(unique(VAR_to_gene_body_correspondence_subset$VAR)))
  cat("\n")
  cat(str(unique(VAR_to_gene_body_correspondence_subset$HGNC)))
  cat("\n")
  
  #### READ Table_S6 ----
  
  Table_S6<-readRDS(file=opt$Table_S6)
  
  
  cat("Table_S6_0\n")
  cat(str(Table_S6))
  cat("\n")
  cat(str(unique(Table_S6$VAR)))
  cat("\n")
  
  Table_S6$chr<-gsub("_.+$","",Table_S6$VAR)
  Table_S6$pos<-gsub("^[^_]+_","",Table_S6$VAR)
  Table_S6$pos<-as.integer(gsub("_.+$","",Table_S6$pos))
  Table_S6$ref<-gsub("^[^_]+_[^_]+_","",Table_S6$VAR)
  Table_S6$ref<-gsub("_.+$","",Table_S6$ref)
  Table_S6$alt<-gsub("^[^_]+_[^_]+_[^_]+_","",Table_S6$VAR)
  
  Table_S6$chr<-factor(Table_S6$chr,
                       levels=c("chr1","chr2","chr3","chr4","chr5","chr6","chr7","chr8","chr9","chr10","chr11",
                                "chr12","chr13","chr14","chr15","chr16","chr17","chr18","chr19","chr20","chr21",
                                "chr22","chr23","chrX","chrY"), ordered=T)
  
  
  cat("Table_S6_1\n")
  cat(str(Table_S6))
  cat("\n")
  cat(str(unique(Table_S6$chr)))
  cat("\n")
  cat(str(unique(Table_S6$pos)))
  cat("\n")
  cat(str(unique(Table_S6$ref)))
  cat("\n")
  cat(str(unique(Table_S6$alt)))
  cat("\n")
  
  indx.int<-c(which(colnames(Table_S6) == 'VAR'),which(colnames(Table_S6) == 'rs'),which(colnames(Table_S6) == 'chr'),which(colnames(Table_S6) == 'pos'),which(colnames(Table_S6) == 'ref'),which(colnames(Table_S6) == 'alt'))
  
  Table_S6_subset<-unique(Table_S6[,indx.int])
  
  cat("Table_S6_subset_0\n")
  cat(str(Table_S6_subset))
  cat("\n")
  cat(str(unique(Table_S6_subset$chr)))
  cat("\n")
  cat(str(unique(Table_S6_subset$pos)))
  cat("\n")
  cat(str(unique(Table_S6_subset$ref)))
  cat("\n")
  cat(str(unique(Table_S6_subset$alt)))
  cat("\n")
  
  
  #### READ Table_S7 ----
  
  Table_S7<-readRDS(file=opt$Table_S7)
  
  cat("Table_S7_0\n")
  cat(str(Table_S7))
  cat("\n")
  cat(str(unique(Table_S7$VAR)))
  cat("\n")
  cat(str(unique(Table_S7$ensembl_gene_id)))
  cat("\n")
  
  Table_S7$Analysis[which(Table_S7$Analysis == 'DTU')]<-'ATU'
  
  Table_S7<-merge(Table_S6_subset,
                  Table_S7,
                  by='VAR')
  
  cat("Table_S7_1\n")
  cat(str(Table_S7))
  cat("\n")
  cat(str(unique(Table_S7$VAR)))
  cat("\n")
  cat(str(unique(Table_S7$ensembl_gene_id)))
  cat("\n")
  
  Table_S7<-Table_S7[order(Table_S7$chr,Table_S7$pos),]
  
  
  
  rs_vector<-unique(Table_S7$rs)
  
  cat("Table_S7_1.5\n")
  cat(str(Table_S7))
  cat("\n")
  cat(str(unique(Table_S7$VAR)))
  cat("\n")
  cat(str(unique(Table_S7$ensembl_gene_id)))
  cat("\n")
  cat("rs_vector_0\n")
  cat(str(rs_vector))
  cat("\n")
  
  
  
  Table_S7$RNASeq_source_2<-revalue(Table_S7$RNASeq_source, 
                                                               c("Whole blood"="Whole blood",
                                                                 "Monocyte"="Monocytes",
                                                                 "Neutrophil"="Neutrophils",
                                                                 "Tcell"="naive T-CD4 Cells"))
  
  Table_S7<-Table_S7[,-which(colnames(Table_S7) == 'RNASeq_source')]
  
  colnames(Table_S7)[which(colnames(Table_S7) == 'RNASeq_source_2')]<-'RNASeq_source'
  
  Table_S7$Analysis<-factor(Table_S7$Analysis,
                                                       levels = c('DE','ATU'),
                                                       ordered=T)
  
  Table_S7$RNASeq_source<-factor(Table_S7$RNASeq_source,
                                                              levels=c("Whole blood","Monocytes","Neutrophils","naive T-CD4 Cells"),
                                                              ordered=T)
  
  Table_S7$rs<-factor(Table_S7$rs,
                                 levels=rs_vector,
                                 ordered=T)
  
  Table_S7<-Table_S7[order(Table_S7$chr,Table_S7$pos,Table_S7$RNASeq_source,Table_S7$Analysis),]
  
  cat("Table_S7_2\n")
  cat(str(Table_S7))
  cat("\n")
  cat(str(unique(Table_S7$VAR)))
  cat("\n")
  cat(str(unique(Table_S7$ensembl_gene_id)))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S7$RNASeq_source)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S7$RNASeq_source))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S7$RNASeq_source)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S7$RNASeq_source))))
  cat("\n")
  
  
  indx.dep<-c(which(colnames(Table_S7) =='chr'),which(colnames(Table_S7) =='pos'),which(colnames(Table_S7) =='ref'),which(colnames(Table_S7) =='alt'))
  
  Table_S7_subset<-unique(Table_S7[,-indx.dep])
  
  cat("Table_S7_subset_0\n")
  cat(str(Table_S7_subset))
  cat("\n")
  cat(str(unique(Table_S7_subset$VAR)))
  cat("\n")
  cat(str(unique(Table_S7_subset$ensembl_gene_id)))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S7_subset$RNASeq_source)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S7_subset$RNASeq_source))))
  cat("\n")
 
  

  indx.int.reorder<-c(which(colnames(Table_S7_subset) == 'rs'),which(colnames(Table_S7_subset) == 'VAR'),
                      which(colnames(Table_S7_subset) == 'RNASeq_source'),which(colnames(Table_S7_subset) == 'Analysis'),
                      which(colnames(Table_S7_subset) == 'ensembl_gene_id'),which(colnames(Table_S7_subset) == 'HGNC'),which(colnames(Table_S7_subset) == 'transcript_id'),
                      which(colnames(Table_S7_subset) == 'Beta'),which(colnames(Table_S7_subset) == 'Beta_Z_score'),
                      which(colnames(Table_S7_subset) == 'adjusted_pval'),which(colnames(Table_S7_subset) == 'adjusted_minus_logpval'))

  Table_S7_subset<-unique(Table_S7_subset[,indx.int.reorder])
  
  cat("Table_S7_subset_1\n")
  cat(str(Table_S7_subset))
  cat("\n")
  cat(str(unique(Table_S7_subset$VAR)))
  cat("\n")
  cat(str(unique(Table_S7_subset$ensembl_gene_id)))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S7_subset$RNASeq_source)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S7_subset$RNASeq_source))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S7_subset$RNASeq_source)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S7_subset$RNASeq_source))))
  cat("\n")
  
  
  Table_S7_subset<-merge(Table_S7_subset,
                         VAR_to_gene_body_correspondence_subset,
                         by=c('VAR','HGNC','ensembl_gene_id'), all.x=T)
  
  
  cat("Table_S7_subset_2\n")
  cat(str(Table_S7_subset))
  cat("\n")
  cat(str(unique(Table_S7_subset$VAR)))
  cat("\n")
  cat(str(unique(Table_S7_subset$ensembl_gene_id)))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S7_subset$RNASeq_source)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S7_subset$RNASeq_source))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S7_subset$RNASeq_source)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S7_subset$RNASeq_source))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S7_subset$regulated_gene_in_Gene_body_CLASS)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S7_subset$regulated_gene_in_Gene_body_CLASS))))
  cat("\n")

  #### path for graphs ----
  
  path_provisional_Tables<-paste(out,'Provisional_Tables','/',sep='')
  
  if (file.exists(path_provisional_Tables)){
    
  }else{
    
    dir.create(file.path(path_provisional_Tables))
    
  }#path_provisional_Tables
  
  setwd(path_provisional_Tables)
  
  
  saveRDS(file="Table_S7_Provisional.rds", Table_S7_subset)

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
    make_option(c("--Table_S6"), type="numeric", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--VAR_to_gene_body_correspondence"), type="numeric", default=NULL, 
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
  
  data_wrangling(opt)
  
  
}


###########################################################################

system.time( main() )