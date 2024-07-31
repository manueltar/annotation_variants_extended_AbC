
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
suppressMessages(library("tidyverse", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1//"))
suppressMessages(library("svglite", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("ggeasy", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("sandwich", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("digest", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("ggforce", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
library("desiR", lib.loc = "/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/")



opt = NULL

options(warn = 1)

Z_score_normalization_and_convergence = function(option_list)
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
  
  cat("OUT_\n")
  cat(sprintf(as.character(out)))
  cat("\n")
  
  #### READ and transform tracking_variants ----
  
  tracking_variants = unlist(strsplit(opt$tracking_variants, split=","))
  
  cat("tracking_variants_\n")
  cat(sprintf(as.character(tracking_variants)))
  cat("\n")
  
  
  #### Read AbC_score file ----
  

  AbC_score<-as.data.frame(fread(file=opt$AbC_score) , stringsAsFactors=F)
  
  cat("AbC_score_0\n")
  cat(str(AbC_score))
  cat("\n")
  cat(str(unique(AbC_score$VAR)))
  cat("\n")
  
  #### calculate mean and sd per lineage -----
  
  AbC_score$mean_AbC_score<-mean(AbC_score$ABC_score, na.rm =T)
  AbC_score$sd_AbC_score<-sd(AbC_score$ABC_score, na.rm =T)
  
  
  cat("AbC_score_POST_merge\n")
  cat(str(AbC_score))
  cat("\n")
  cat(str(unique(AbC_score$VAR)))
  cat("\n")
  
  
  AbC_score$AbC_score_Z_score<-(AbC_score$ABC_score-AbC_score$mean_AbC_score)/AbC_score$sd_AbC_score
  
  cat("AbC_score_POST_merge_Z_score\n")
  cat(str(AbC_score))
  cat("\n")
  cat(str(unique(AbC_score$VAR)))
  cat("\n")
  cat(sprintf(as.character(names(summary(AbC_score$AbC_score_Z_score)))))
  cat("\n")
  cat(sprintf(as.character(summary(AbC_score$AbC_score_Z_score))))
  cat("\n")
  
  #### select columns and change names to converge ----
  
  indx.int<-c(which(colnames(AbC_score) == "VAR"),which(colnames(AbC_score) == "ABC_score"),which(colnames(AbC_score) == "AbC_score_Z_score"))
  
  AbC_score_subset<-AbC_score[,indx.int]
  
  cat("AbC_score_subset_0\n")
  cat(str(AbC_score_subset))
  cat("\n")
  cat(str(unique(AbC_score_subset$VAR)))
  cat("\n")
  
  
  colnames(AbC_score_subset)[which(colnames(AbC_score_subset) == "ABC_score")]<-"value"
  colnames(AbC_score_subset)[which(colnames(AbC_score_subset) == "AbC_score_Z_score")]<-"value_Z_score"
  
  AbC_score_subset$variable<-"AbC_score"
  
  
  cat("AbC_score_subset_1\n")
  cat(str(AbC_score_subset))
  cat("\n")
  cat(str(unique(AbC_score_subset$VAR)))
  cat("\n")
  
  check<-AbC_score_subset[which(AbC_score_subset$VAR%in%tracking_variants),]
  
  
  cat("check_0\n")
  cat(str(check))
  cat("\n")
  
  # #### SAVE ----
  
  setwd(out)
  
  saveRDS(AbC_score_subset, file="Prepared_file_AbC_score.rds")
  
  write.table(check, file="check_AbC_score.tsv", sep="\t", quote = F, row.names = F)
  
  
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
    make_option(c("--AbC_score"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--tracking_variants"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
       make_option(c("--type"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
        make_option(c("--out"), type="character", default=NULL, 
                metavar="filename", 
                help="Path to tab-separated input file listing regions to analyze. Required.")
  )
  parser = OptionParser(usage = "140__Rscript_v106.R
                        --subset type
                        --TranscriptEXP FILE.txt
                        --cadd FILE.txt
                        --AbC_score FILE.txt
                        --type type
                        --out filename",
                        option_list = option_list)
  opt <<- parse_args(parser)
  
 
  Z_score_normalization_and_convergence(opt)
  
}


###########################################################################

system.time( main() )
