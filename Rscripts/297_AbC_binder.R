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
suppressMessages(library("tidyverse", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("svglite", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("ggeasy", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("sandwich", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("digest", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("reshape2", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("BiocGenerics", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("S4Vectors", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("IRanges", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("GenomeInfoDb", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("GenomicRanges", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("Biobase", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("AnnotationDbi", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("GenomicFeatures", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("OrganismDbi", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("GO.db", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("org.Hs.eg.db", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("TxDb.Hsapiens.UCSC.hg19.knownGene", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("Homo.sapiens", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("gwascat", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("rtracklayer", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("R.utils", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))



opt = NULL

filter_AbC = function(option_list)
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
  
  cat("OUT\n")
  cat(sprintf(as.character(out)))
  cat("\n")
  
  #### READ and transform AbC_cell_types ----
  
  AbC_cell_types = unlist(strsplit(opt$AbC_cell_types, split=","))
  
  cat("AbC_cell_types_\n")
  cat(sprintf(as.character(AbC_cell_types)))
  cat("\n")
  
  #### READ and transform Threshold_AbC ----
  
  Threshold_AbC = opt$Threshold_AbC
  
  cat("Threshold_AbC\n")
  cat(sprintf(as.character(Threshold_AbC)))
  cat("\n")
  
  #### READ and transform tracking_variants ----
  
  tracking_variants = unlist(strsplit(opt$tracking_variants, split=","))
  
  cat("tracking_variants_\n")
  cat(sprintf(as.character(tracking_variants)))
  cat("\n")
  
  
  #### Read AbC_scores ----
  
  
  AbC_scores<-as.data.frame(fread(file=opt$AbC_scores,sep="\t",header=T, stringsAsFactors = F))
  
  cat("AbC_scores_0\n")
  str(AbC_scores)
  cat("\n")
  
  
  AbC_scores_selected_cell_types<-AbC_scores[which(AbC_scores$CellType%in%AbC_cell_types),]
  
  cat("AbC_scores_selected_cell_types_0\n")
  str(AbC_scores_selected_cell_types)
  cat("\n")
  
  
  AbC_scores_selected_cell_types_Thresholded<-AbC_scores_selected_cell_types[which(AbC_scores_selected_cell_types$ABC.Score >= Threshold_AbC),]
  
  cat("AbC_scores_selected_cell_types_Thresholded_0\n")
  str(AbC_scores_selected_cell_types_Thresholded)
  cat("\n")
  
  
  ##### save -----
  
  
  setwd(out)
  
  saveRDS(AbC_scores_selected_cell_types_Thresholded, file='AbC_selected_thresholded.rds')
  
  write.table(AbC_scores_selected_cell_types_Thresholded, file='AbC_selected_thresholded.tsv', sep="\t",quote=F,row.names = F)
 
}

intersect_filtered_AbC = function(option_list)
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
  
  cat("OUT\n")
  cat(sprintf(as.character(out)))
  cat("\n")
  
  #### READ and transform tracking_variants ----
  
  tracking_variants = unlist(strsplit(opt$tracking_variants, split=","))
  
  cat("tracking_variants_\n")
  cat(sprintf(as.character(tracking_variants)))
  cat("\n")
  
  
  #### Read AbC_selected_thresholded ----
  
  setwd(out)
  
  AbC_selected_thresholded<-readRDS(file='AbC_selected_thresholded.rds')
  
  cat("AbC_selected_thresholded_0\n")
  str(AbC_selected_thresholded)
  cat("\n")
  
  gr_AbC_selected_thresholded <-GRanges(
    seqnames = as.character(AbC_selected_thresholded$chr),
    name2=AbC_selected_thresholded$TargetGene,
    name3=AbC_selected_thresholded$CellType,
    name4=AbC_selected_thresholded$ABC.Score,
    ranges=IRanges(
      start=as.numeric(AbC_selected_thresholded$start),
      end=as.numeric(AbC_selected_thresholded$end),
      names = AbC_selected_thresholded$name))
  
  
  cat("gr_AbC_selected_thresholded_0\n")
  str(gr_AbC_selected_thresholded)
  cat("\n")
  
  #### Read ALL_dB file ----
  
  ALL_dB<-as.data.frame(fread(file=opt$ALL_dB,sep="\t") , stringsAsFactors=F)
  
  cat("ALL_dB_0\n")
  cat(str(ALL_dB))
  cat("\n")
  cat(str(unique(ALL_dB$VAR)))
  cat("\n")
  
  
  indx.dep<-c(which(colnames(ALL_dB) == "maf_origin"))
  
  ALL_dB_subset<-unique(ALL_dB[,-indx.dep])
  
  cat("ALL_dB_subset\n")
  cat(str(ALL_dB_subset))
  cat("\n")
  cat(str(unique(ALL_dB_subset$VAR)))
  cat("\n")
  
  rm(ALL_dB)
  
  VAR_df<-unique(data.frame(chr=ALL_dB_subset$chr,
                            pos37=ALL_dB_subset$pos37,
                            ref=ALL_dB_subset$ref,
                            alt=ALL_dB_subset$alt,
                            VAR=ALL_dB_subset$VAR,
                            stringsAsFactors = F))
  
  Condition_DEBUG <- 1
  
  if(Condition_DEBUG == 1)
  {
    cat("VAR_df_\n")
    str(VAR_df)
    cat("\n")
    cat(str(unique(VAR_df$VAR)))
    cat("\n")
  }
  
  gr_VARS <-unique(GRanges(
    seqnames = as.character(VAR_df$chr),
    name2=rep("VAR",length(VAR_df$VAR)),
    ranges=IRanges(
      start=as.numeric(VAR_df$pos37),
      end=as.numeric(VAR_df$pos37),
      names = VAR_df$VAR)))
  
  
  
  if(Condition_DEBUG == 1)
  {
    cat("gr_VARS_\n")
    str(gr_VARS)
    cat("\n")
    cat(str(unique(gr_VARS$VAR))) #179324
    cat("\n")
  }
  
  #### Intersect TSS to genes with Regulatory features ----
  
  DEBUG <- 1
  
  m <- findOverlaps(gr_VARS,gr_AbC_selected_thresholded,
                    ignore.strand = TRUE)
  
  if(DEBUG == 1)
  {
    cat("m\n")
    cat(str(m))
    cat("\n")
  }
  
  subjectHits_AbC_selected_thresholded<-subjectHits(m)
  
  if(DEBUG == 1)
  {
    cat("subjectHits_AbC_selected_thresholded\n")
    cat(str(subjectHits_AbC_selected_thresholded))
    cat("\n")
  }
  
  queryHits_VARS<-queryHits(m)
  
  if(DEBUG == 1)
  {
    cat("queryHits_VARS\n")
    cat(str(queryHits_VARS))
    cat("\n")
  }
  
  VARS_df <- data.frame(chr=as.character(seqnames(gr_VARS)),
                                                  start=as.integer(start(gr_VARS)),
                                                  end=as.integer(end(gr_VARS)),
                                                  VAR=names(gr_VARS), stringsAsFactors = F)
  
  if(DEBUG == 1)
  {
    cat("VARS_df_0\n")
    cat(str(VARS_df))
    cat("\n")
  }
  
  VARS_df_hits<-VARS_df[queryHits_VARS,]
  
  if(DEBUG == 1)
  {
    cat("VARS_df_hits_0\n")
    cat(str(VARS_df_hits))
    cat("\n")
  }
 
  
  AbC_scores_df_VARS <- data.frame(chr=as.character(seqnames(gr_AbC_selected_thresholded)),
                                                      start=as.integer(start(gr_AbC_selected_thresholded)),
                                                      end=as.integer(end(gr_AbC_selected_thresholded)),
                                                      Symbol=as.character(gr_AbC_selected_thresholded$name2),
                                                      Cell_Type=as.character(gr_AbC_selected_thresholded$name3),
                                                      ABC_score=as.numeric(gr_AbC_selected_thresholded$name4),
                                                      ABC_name=names(gr_AbC_selected_thresholded),
                                                      stringsAsFactors = F)
  
  
  if(DEBUG == 1)
  {
    cat("AbC_scores_df_VARS_0\n")
    cat(str(AbC_scores_df_VARS))
    cat("\n")
  }
  
  AbC_scores_df_VARS_hits<-AbC_scores_df_VARS[subjectHits_AbC_selected_thresholded,]
  
  if(dim(AbC_scores_df_VARS_hits)[1] >0)
  {
    if(DEBUG == 1)
    {
      cat("AbC_scores_df_VARS_hits_0\n")
      cat(str(AbC_scores_df_VARS_hits))
      cat("\n")
    }
    
    AbC_scores_df_VARS_hits<-cbind(AbC_scores_df_VARS_hits,VARS_df_hits)
    
    if(DEBUG == 1)
    {
      cat("AbC_scores_df_VARS_hits_1\n")
      cat(str(AbC_scores_df_VARS_hits))
      cat("\n")
      cat(str(unique(AbC_scores_df_VARS_hits$VAR)))
      cat("\n")
    }
   
    check<-AbC_scores_df_VARS_hits[which(AbC_scores_df_VARS_hits$VAR%in%tracking_variants),]
    
    
    cat("check_0\n")
    cat(str(check))
    cat("\n")
    
    
    # #### SAVE ----
    
    setwd(out)
    
    write.table(AbC_scores_df_VARS_hits, file="AbC_GLOBAL.tsv", sep="\t", quote = F, row.names = F)
    
    write.table(check, file="check_AbC_GLOBAL.tsv", sep="\t", quote = F, row.names = F)
    
    
    
    
  }#dim(AbC_scores_df_VARS_hits)[1] >0
  
  
 
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
  traceback()
  options(show.error.locations = TRUE)
  
  option_list <- list(
    make_option(c("--AbC_scores"), type="character", default=NULL,
                metavar="FILE.txt",
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--ALL_dB"), type="character", default=NULL,
                metavar="FILE.txt",
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--tracking_variants"), type="character", default=NULL,
                metavar="FILE.txt",
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--AbC_cell_types"), type="character", default=NULL,
                metavar="FILE.txt",
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--Threshold_AbC"), type="numeric", default=NULL,
                metavar="FILE.txt",
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
  
 
 filter_AbC(opt)
 intersect_filtered_AbC(opt)
  
}




###########################################################################

system.time( main() )
