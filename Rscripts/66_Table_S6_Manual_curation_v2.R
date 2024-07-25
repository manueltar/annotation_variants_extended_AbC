
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
  
  #### READ and transform check_SNPS ----
  
  check_SNPS = unlist(strsplit(opt$check_SNPS, split=","))
  
  cat("check_SNPS_\n")
  cat(sprintf(as.character(check_SNPS)))
  cat("\n")
  
  
  #### READ refined_manual_curation ----
  
  refined_manual_curation<-as.data.frame(fread(file=opt$refined_manual_curation, sep="\t", header=T), stringsAsFactors=F)
  
  cat("refined_manual_curation_0\n")
  cat(str(refined_manual_curation))
  cat("\n")
  cat(str(unique(refined_manual_curation$VAR)))
  cat("\n")
  
  indx.int<-c(which(colnames(refined_manual_curation) == 'VAR'),which(colnames(refined_manual_curation) == 'rs'),which(colnames(refined_manual_curation) == 'Mechanistic_Class'),
              which(colnames(refined_manual_curation) == 'Manual_curation'),which(colnames(refined_manual_curation) == 'Candidate_effector'))
  
  refined_manual_curation_subset<-unique(refined_manual_curation[,indx.int])
  

  refined_manual_curation_subset$Mechanistic_Class[which(refined_manual_curation_subset$Mechanistic_Class == 'No RNA effect')]<-'No effect'
  cat("refined_manual_curation_subset_0\n")
  cat(str(refined_manual_curation_subset))
  cat("\n")
  cat(str(unique(refined_manual_curation_subset$VAR)))
  cat("\n")
  cat(str(length(refined_manual_curation_subset$Candidate_effector[which(refined_manual_curation_subset$Candidate_effector == 'ABSENT')])))
  cat("\n")
  
  # cat(sprintf(as.character(names(summary(refined_manual_curation_subset$Manual_curation)))))
  # cat("\n")
  # cat(sprintf(as.character(summary(refined_manual_curation_subset$Manual_curation))))
  # cat("\n")
  # cat(sprintf(as.character(names(summary(refined_manual_curation_subset$Mechanistic_Class)))))
  # cat("\n")
  # cat(sprintf(as.character(summary(refined_manual_curation_subset$Mechanistic_Class))))
  # cat("\n")
  
  #### READ Table_S6 ----
  
  Table_S6<-readRDS(file=opt$Table_S6)
  
  
  cat("Table_S6_0\n")
  cat(str(Table_S6))
  cat("\n")
  cat(str(unique(Table_S6$VAR)))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S6$Manual_curation)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S6$Manual_curation))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S6$Mechanistic_Class)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S6$Mechanistic_Class))))
  cat("\n")
  cat(str(length(Table_S6$Candidate_effector[which(Table_S6$Candidate_effector == 'ABSENT')])))
  cat("\n")
  
  
  refined_manual_curation_subset$Manual_curation<-factor(refined_manual_curation_subset$Manual_curation,
                                                         levels=levels(Table_S6$Manual_curation),
                                                         ordered = T)
  
  refined_manual_curation_subset$Mechanistic_Class<-factor(refined_manual_curation_subset$Mechanistic_Class,
                                                         levels=levels(Table_S6$Mechanistic_Class),
                                                         ordered = T)
  
  
  cat("refined_manual_curation_subset_1\n")
  cat(str(refined_manual_curation_subset))
  cat("\n")
  cat(str(unique(refined_manual_curation_subset$VAR)))
  cat("\n")
  cat(sprintf(as.character(names(summary(refined_manual_curation_subset$Manual_curation)))))
  cat("\n")
  cat(sprintf(as.character(summary(refined_manual_curation_subset$Manual_curation))))
  cat("\n")
  cat(sprintf(as.character(names(summary(refined_manual_curation_subset$Mechanistic_Class)))))
  cat("\n")
  cat(sprintf(as.character(summary(refined_manual_curation_subset$Mechanistic_Class))))
  cat("\n")
  
  
  ###################### merge refined with old table ---------------------------------------
  
  
  indx.dep<-c(which(colnames(Table_S6) == 'Mechanistic_Class'),
              which(colnames(Table_S6) == 'Manual_curation'),which(colnames(Table_S6) == 'Candidate_effector'))
  
  
  Table_S6_subset<-unique(Table_S6[,-indx.dep])
  
  cat("Table_S6_subset_0\n")
  cat(str(Table_S6_subset))
  cat("\n")
  cat(str(unique(Table_S6_subset$VAR)))
  cat("\n")
  
  
  Table_S6<-merge(refined_manual_curation_subset,
                  Table_S6_subset,
                  by=c("VAR","rs"))
  
  cat("Table_S6_1\n")
  cat(str(Table_S6))
  cat("\n")
  cat(str(unique(Table_S6$VAR)))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S6$Manual_curation)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S6$Manual_curation))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S6$Mechanistic_Class)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S6$Mechanistic_Class))))
  cat("\n")
  cat(str(length(Table_S6$Candidate_effector[which(Table_S6$Candidate_effector == 'ABSENT')])))
  cat("\n")
  
  
  ##############
  
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
  cat(sprintf(as.character(names(summary(Table_S6$MPRA_CLASS)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S6$MPRA_CLASS))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S6$genIE_CLASS)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S6$genIE_CLASS))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S6$Mechanistic_Class)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S6$Mechanistic_Class))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S6$Manual_curation)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S6$Manual_curation))))
  cat("\n")
  
  Table_S6$MPRA_CLASS<-revalue(Table_S6$MPRA_CLASS,
                                       c('no MPRA hit' = 'MPRA negative',
                                         'MPRA hit' = 'MPRA positive'))
  Table_S6$genIE_CLASS<-revalue(Table_S6$genIE_CLASS,
                               c('no genIE hit' = 'genIE negative',
                                 'genIE hit' = 'genIE positive'))
  
  Table_S6$Mechanistic_Class<-revalue(Table_S6$Mechanistic_Class,
                               c('No effect' = 'No RNA effect',
                                 'Transcriptional R. + ATU' = 'DE + ATU',
                                 'Transcriptional R.' = 'DE'))
  
 
  
  
  Table_S6$Manual_curation<-revalue(Table_S6$Manual_curation,
                                c('R in non candidate' = 'other gene'))
  
  
  #### Create the category 4 ----
  
  
  Table_S6$integration_category<-NA
  
  Table_S6$integration_category[which(Table_S6$Mechanistic_Class == 'DE + ATU')]<-'DE/ATU'
  Table_S6$integration_category[which(Table_S6$Mechanistic_Class == 'DE')]<-'DE/ATU'
  Table_S6$integration_category[which(Table_S6$Mechanistic_Class == 'ATU')]<-'DE/ATU'
  Table_S6$integration_category[which(Table_S6$Mechanistic_Class == 'No RNA effect')]<-'No RNA effect'
  
  Table_S6$integration_category[!is.na(Table_S6$integration_category)]<-paste(Table_S6$integration_category[!is.na(Table_S6$integration_category)],Table_S6$MPRA_CLASS[!is.na(Table_S6$integration_category)], sep="|")
 
  Table_S6$integration_category<-factor(Table_S6$integration_category,
                                      levels=c('DE/ATU|MPRA positive','No RNA effect|MPRA positive',
                                               'DE/ATU|MPRA negative','No RNA effect|MPRA negative'),
                                      ordered=T)
  
  Table_S6$integration_category<-revalue(Table_S6$integration_category,
                                      c('DE/ATU|MPRA positive' = 'Double positives',
                                        'No RNA effect|MPRA positive' = 'MPRA+/RNA-',
                                        'DE/ATU|MPRA negative' = 'MPRA-/RNA+',
                                        'No RNA effect|MPRA negative' = 'Double negatives'))
  
  
  cat("Table_S6_2\n")
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
  cat(sprintf(as.character(names(summary(Table_S6$MPRA_CLASS)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S6$MPRA_CLASS))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S6$genIE_CLASS)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S6$genIE_CLASS))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S6$Mechanistic_Class)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S6$Mechanistic_Class))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S6$Manual_curation)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S6$Manual_curation))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_S6$integration_category)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_S6$integration_category))))
  cat("\n")
  
  Table_S6<-Table_S6[order(Table_S6$chr,Table_S6$pos),]
  
  cat("Table_S6_3\n")
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
  
  
  indx.dep<-c(which(colnames(Table_S6) =='chr'),which(colnames(Table_S6) =='pos'),which(colnames(Table_S6) =='ref'),which(colnames(Table_S6) =='alt'))
  
  Table_S6_subset<-unique(Table_S6[,-indx.dep])
  
  cat("Table_S6_subset_0\n")
  cat(str(Table_S6_subset))
  cat("\n")
  cat(str(unique(Table_S6_subset$VAR)))
  cat("\n")
 
 
  
  #### path for graphs ----
  
  path_provisional_Tables<-paste(out,'Provisional_Tables','/',sep='')
  
  if (file.exists(path_provisional_Tables)){
    
  }else{
    
    dir.create(file.path(path_provisional_Tables))
    
  }#path_provisional_Tables
  
  setwd(path_provisional_Tables)
  

  saveRDS(file="Table_S6_Provisional.rds", Table_S6_subset)
  
  
  #### check ----
  
  
  check<-Table_S6_subset[which(Table_S6_subset$rs%in%check_SNPS),]
  
  cat("check_0\n")
  cat(str(check))
  cat("\n")
  cat(str(unique(check$VAR)))
  cat("\n")
  cat(sprintf(as.character(check$rs)))
  cat("\n")
  cat(sprintf(as.character(check$Mechanistic_Class)))
  cat("\n")
  
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
    make_option(c("--check_SNPS"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--Table_S6"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--refined_manual_curation"), type="character", default=NULL, 
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