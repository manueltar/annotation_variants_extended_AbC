
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
  
  #### Read Table_S6 ----
  
  Table_S6<-as.data.frame(readRDS(file=opt$Table_S6) , stringsAsFactors=F)
  
  cat("Table_S6_0\n")
  cat(str(Table_S6))
  cat("\n")
  cat(str(unique(Table_S6$VAR)))
  cat("\n")
  
  #### READ Table_S1 ----
  
  Table_S1<-as.data.frame(fread(file=opt$Table_S1, sep="\t", header=T), stringsAsFactors=F)
  
  
  cat("Table_S1_0\n")
  cat(str(Table_S1))
  cat("\n")
  cat(str(unique(Table_S1$VAR)))
  cat("\n")
  
  colnames(Table_S1)[which(colnames(Table_S1) == 'Prioritisation_bins')]<-'Variant_classification'
  
  cat("Table_S1_1\n")
  cat(str(Table_S1))
  cat("\n")
  cat(str(unique(Table_S1$VAR)))
  cat("\n")
  
  Table_S1$chr<-gsub("_.+$","",Table_S1$VAR)
  Table_S1$pos<-gsub("^[^_]+_","",Table_S1$VAR)
  Table_S1$pos<-as.integer(gsub("_.+$","",Table_S1$pos))
  Table_S1$ref<-gsub("^[^_]+_[^_]+_","",Table_S1$VAR)
  Table_S1$ref<-gsub("_.+$","",Table_S1$ref)
  Table_S1$alt<-gsub("^[^_]+_[^_]+_[^_]+_","",Table_S1$VAR)
  
  Table_S1$chr<-factor(Table_S1$chr,
                       levels=c("chr1","chr2","chr3","chr4","chr5","chr6","chr7","chr8","chr9","chr10","chr11",
                                "chr12","chr13","chr14","chr15","chr16","chr17","chr18","chr19","chr20","chr21",
                                "chr22","chr23","chrX","chrY"), ordered=T)
  
  
  cat("Table_S1_1\n")
  cat(str(Table_S1))
  cat("\n")
  cat(str(unique(Table_S1$chr)))
  cat("\n")
  cat(str(unique(Table_S1$pos)))
  cat("\n")
  cat(str(unique(Table_S1$ref)))
  cat("\n")
  cat(str(unique(Table_S1$alt)))
  cat("\n")
  
  Table_S1<-Table_S1[order(Table_S1$chr,Table_S1$pos),]
  
  cat("Table_S1_2\n")
  cat(str(Table_S1))
  cat("\n")
  cat(str(unique(Table_S1$chr)))
  cat("\n")
  cat(str(unique(Table_S1$pos)))
  cat("\n")
  cat(str(unique(Table_S1$ref)))
  cat("\n")
  cat(str(unique(Table_S1$alt)))
  cat("\n")
  

  indx.dep<-c(which(colnames(Table_S1) =='chr'),which(colnames(Table_S1) =='pos'),which(colnames(Table_S1) =='ref'),which(colnames(Table_S1) =='alt'))
  
  
  
  Table_S1_subset<-unique(Table_S1[,-indx.dep])
  
  
  ADD_index_variants<-Table_S1_subset[which(Table_S1_subset$VAR%in%Table_S6$VAR),]
  
  ADD_index_variants$Variant_classification<-'MPRA_screened_variants'
  
  cat("ADD_index_variants_0\n")
  cat(str(ADD_index_variants))
  cat("\n")
  
  Table_S1_subset<-rbind(Table_S1_subset,ADD_index_variants)
  
  cat("Table_S1_subset_0\n")
  cat(str(Table_S1))
  cat("\n")
  cat(str(unique(Table_S1$VAR)))
  cat("\n")
 
  
  
  
  
  
  
  Table_S1_subset$Variant_classification<-factor(Table_S1_subset$Variant_classification,
                                                 levels=c("Common_variant","RV_C","RV_NC_lowPP","RV_NC_highPP_lowEffectSize","index_variants",'MPRA_screened_variants'),
                                                 ordered=T)
  
  
  cat("Table_S1_subset_1\n")
  cat(str(Table_S1_subset))
  cat("\n")
  cat(str(unique(Table_S1_subset$VAR)))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S1_subset$Variant_classification)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S1_subset$Variant_classification))))
  cat("\n")
  
  Table_S1_subset$Variant_classification<-revalue(Table_S1_subset$Variant_classification,
                               c('Common_variant' = 'Tier 1',
                                 'RV_C' = 'Tier 2',
                                 'RV_NC_lowPP' = 'Tier 3',
                                 'RV_NC_highPP_lowEffectSize' = 'Tier 4',
                                 'index_variants' = 'index_variants',
                                 'MPRA_screened_variants' = 'index_variants'))
  

  
  cat("Table_S1_subset_1\n")
  cat(str(Table_S1_subset))
  cat("\n")
  cat(str(unique(Table_S1_subset$VAR)))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S1_subset$Variant_classification)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S1_subset$Variant_classification))))
  cat("\n")
  
  #### Add the paper label ----
  
  Table_S1_subset$Paper_group<-NA
  
  Table_S1_subset$Paper_group[which(Table_S1_subset$Variant_classification == 'Tier 1')]<-'Common'
  Table_S1_subset$Paper_group[which(Table_S1_subset$Variant_classification == 'Tier 2')]<-'Rare coding'
  Table_S1_subset$Paper_group[which(Table_S1_subset$Variant_classification == 'Tier 3')]<-'Rare non-coding lowPP'
  Table_S1_subset$Paper_group[which(Table_S1_subset$Variant_classification == 'Tier 4')]<-'Rare non-coding lowbeta'
  
  
  #### select the minimum MAF ---- 
  
  
  Table_S1_subset.dt<-data.table(Table_S1_subset, key="VAR")
  
  
  Table_S1_subset_min_MAF<-as.data.frame(Table_S1_subset.dt[,.SD[which.min(maf_origin)], by=key(Table_S1_subset.dt)], stringsAsFactors=F)
  
  
  cat("Table_S1_subset_min_MAF_0\n")
  cat(str(Table_S1_subset_min_MAF))
  cat("\n")
  cat(str(unique(Table_S1_subset_min_MAF$VAR)))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S1_subset_min_MAF$Variant_classification)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S1_subset_min_MAF$Variant_classification))))
  cat("\n")
  
  
  #### path for graphs ----

  path_provisional_Tables<-paste(out,'Provisional_Tables','/',sep='')

  if (file.exists(path_provisional_Tables)){

  }else{

    dir.create(file.path(path_provisional_Tables))

  }#path_provisional_Tables

  setwd(path_provisional_Tables)


  saveRDS(file="Table_S1_Provisional.rds", Table_S1_subset_min_MAF)
  
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
    make_option(c("--Table_S6"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--Table_S1"), type="numeric", default=NULL, 
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