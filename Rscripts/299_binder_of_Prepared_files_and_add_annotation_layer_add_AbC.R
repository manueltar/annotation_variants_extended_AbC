
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

Data_wrangling = function(option_list)
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
  
 
  #### Read Master_file ----
  
  Master_file<-as.data.frame(readRDS(file=opt$Master_file) , stringsAsFactors=F)
  
  
  levels_order_variable<-levels(Master_file$variable)
  
  Master_file$variable<-as.character(Master_file$variable)
  
  cat("Master_file_0\n")
  cat(str(Master_file))
  cat("\n")
  cat(str(unique(Master_file$VAR)))
  cat("\n")
  cat(sprintf(as.character(names(summary(Master_file$variable)))))
  cat("\n")
  cat(sprintf(as.character(summary(Master_file$variable))))
  cat("\n")
  
  cat("levels_order_variable\n")
  cat(sprintf(as.character(levels_order_variable)))
  cat("\n")
  
  
  #### Read AbC_score file ----
  
  AbC_score<-readRDS(file=opt$AbC_score)
  
  cat("AbC_score_0\n")
  cat(str(AbC_score))
  cat("\n")
  cat(str(unique(AbC_score$VAR)))
  cat("\n")
  
  ind.int<-c(which(colnames(AbC_score) == "VAR"),which(colnames(AbC_score) == "value"),which(colnames(AbC_score) == "value_Z_score"),which(colnames(AbC_score) == "variable"))
  
  AbC_score_subset<-AbC_score[,ind.int]
  
  cat("AbC_score_subset_0\n")
  cat(str(AbC_score_subset))
  cat("\n")
  cat(str(unique(AbC_score_subset$VAR)))
  cat("\n")
  
 
  #### Rbind ----
  
  
  Master_file<-rbind(Master_file,AbC_score_subset)

  cat("Master_file_1\n")
  cat(str(Master_file))
  cat("\n")
  cat(str(unique(Master_file$VAR)))
  cat("\n")

  array_variables<-levels(as.factor(Master_file$variable))

  cat("array_variables_0\n")
  cat(str(array_variables))
  cat("\n")
  cat(sprintf(as.character(array_variables)))
  cat("\n")
  
  check_NA<-Master_file[is.na(Master_file$variable),]
  
  cat("check_NA_0\n")
  cat(str(check_NA))
  cat("\n")
  cat(str(unique(check_NA$VAR)))
  cat("\n")
  cat(str(unique(check_NA$variable)))
  cat("\n")
  cat(sprintf(as.character(names(summary(check_NA$variable)))))
  cat("\n")
  cat(sprintf(as.character(summary(check_NA$variable))))
  cat("\n")
  
  
  indx.to.add<-which(levels_order_variable == 'Gnocchi')
  
  cat("indx.to.add_0\n")
  cat(str(indx.to.add))
  cat("\n")
  
  indx.lower<-seq(1,indx.to.add,by=1)
  
  cat("indx.lower_0\n")
  cat(str(indx.lower))
  cat("\n")
  
  indx.higher<-seq(indx.to.add+1,length(levels_order_variable),by=1)
  
  cat("indx.higher_0\n")
  cat(str(indx.higher))
  cat("\n")
  
  
  new_levels_order_variable<-c(levels_order_variable[indx.lower],"AbC_score",levels_order_variable[indx.higher])
  
  cat("new_levels_order_variable\n")
  cat(sprintf(as.character(new_levels_order_variable)))
  cat("\n")

  Master_file$variable<-factor(Master_file$variable,
                               levels=new_levels_order_variable,
                               ordered=T)


  cat("Master_file_2\n")
  cat(str(Master_file))
  cat("\n")
  cat(str(unique(Master_file$VAR)))
  cat("\n")
  cat(str(unique(Master_file$variable)))
  cat("\n")
  cat(sprintf(as.character(names(summary(Master_file$variable)))))
  cat("\n")
  cat(sprintf(as.character(summary(Master_file$variable))))
  cat("\n")


  
  check_NA<-Master_file[is.na(Master_file$variable),]
  
  cat("check_NA_1\n")
  cat(str(check_NA))
  cat("\n")
  cat(str(unique(check_NA$VAR)))
  cat("\n")
  cat(str(unique(check_NA$variable)))
  cat("\n")
  cat(sprintf(as.character(names(summary(check_NA$variable)))))
  cat("\n")
  cat(sprintf(as.character(summary(check_NA$variable))))
  cat("\n")
  
 
  ############## SAVE -------------------------
  
  setwd(out)
  
  saveRDS(Master_file,file="Master_file_scores_added_AbC.rds")
  

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
    make_option(c("--Master_file"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--AbC_score"), type="character", default=NULL, 
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
                        --ncboost FILE.txt
                        --type type
                        --out filename",
                        option_list = option_list)
  opt <<- parse_args(parser)
  
  Data_wrangling(opt)
  
}


###########################################################################

system.time( main() )
