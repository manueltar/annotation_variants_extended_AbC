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
suppressMessages(library("cowplot", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))


opt = NULL

options(warn = 1)


Isolated_graphs_function = function(option_list)
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
  
  #### READ and transform Paper_Annotation_REP ----
  
  Paper_Annotation_REP = unlist(strsplit(opt$Paper_Annotation_REP, split=","))
  
  cat("GWAS_parameters_\n")
  cat(sprintf(as.character(Paper_Annotation_REP)))
  cat("\n")
  
  #### READ and transform GWAS_parameters ----
  
  GWAS_parameters = unlist(strsplit(opt$GWAS_parameters, split=","))
  
  cat("GWAS_parameters_\n")
  cat(sprintf(as.character(GWAS_parameters)))
  cat("\n")
  
  df_GWAS_parameters<-as.data.frame(cbind(rep("GWAS_parameters", length(GWAS_parameters)),GWAS_parameters), stringsAsFactors=F)
  colnames(df_GWAS_parameters)<-c("GROUP","variable")
  
  cat("df_GWAS_parameters_0\n")
  cat(str(df_GWAS_parameters))
  cat("\n")
  
  #### READ and transform Variant_based_scores ----
  
  Variant_based_scores = unlist(strsplit(opt$Variant_based_scores, split=","))
  
  cat("Variant_based_scores_\n")
  cat(sprintf(as.character(Variant_based_scores)))
  cat("\n")
  
  df_Variant_based_scores<-as.data.frame(cbind(rep("Variant_based_scores", length(Variant_based_scores)),Variant_based_scores), stringsAsFactors=F)
  colnames(df_Variant_based_scores)<-c("GROUP","variable")
  
  cat("df_Variant_based_scores_0\n")
  cat(str(df_Variant_based_scores))
  cat("\n")
  
  #### READ and transform Our_rankings ----
  
  Our_rankings = unlist(strsplit(opt$Our_rankings, split=","))
  
  cat("Our_rankings_\n")
  cat(sprintf(as.character(Our_rankings)))
  cat("\n")
  
  df_Our_rankings<-as.data.frame(cbind(rep("Our_rankings", length(Our_rankings)),Our_rankings), stringsAsFactors=F)
  colnames(df_Our_rankings)<-c("GROUP","variable")
  
  cat("df_Our_rankings_0\n")
  cat(str(df_Our_rankings))
  cat("\n")
  
  #### READ and transform Gene_based_features ----
  
  Gene_based_features = unlist(strsplit(opt$Gene_based_features, split=","))
  
  cat("Gene_based_features_\n")
  cat(sprintf(as.character(Gene_based_features)))
  cat("\n")
  
  df_Gene_based_features<-as.data.frame(cbind(rep("Gene_based_features", length(Gene_based_features)),Gene_based_features), stringsAsFactors=F)
  colnames(df_Gene_based_features)<-c("GROUP","variable")
  
  cat("df_Gene_based_features_0\n")
  cat(str(df_Gene_based_features))
  cat("\n")
  
  #### Merge ----
  
  df_subset_variables<-rbind(df_GWAS_parameters,
                             df_Variant_based_scores,
                             df_Our_rankings,
                             df_Gene_based_features)
  
  df_subset_variables$variable<-factor(df_subset_variables$variable,
                                       levels=rev(c(GWAS_parameters,Variant_based_scores,Our_rankings,Gene_based_features)),
                                       ordered=T)
  
  df_subset_variables$GROUP<-factor(df_subset_variables$GROUP,
                                    levels=c("GWAS_parameters","Variant_based_scores","Our_rankings","Gene_based_features"),
                                    ordered=T)
  
  cat("df_subset_variables_0\n")
  cat(str(df_subset_variables))
  cat("\n")
  cat(str(unique(df_subset_variables$GROUP)))
  cat("\n")
  cat(str(unique(df_subset_variables$variable)))
  cat("\n")
  
  
  # #####################################################################
  # quit(status = 1)
  
  #### Read Table_of_labels ----
  
  Table_of_labels<-droplevels(as.data.frame(readRDS(file=opt$Table_of_labels) , stringsAsFactors=F))
  
  cat("Table_of_labels_0\n")
  cat(str(Table_of_labels))
  cat("\n")
  cat(str(unique(Table_of_labels$VAR)))
  cat("\n")
  
  cat("Table_of_labels_0\n")
  cat(str(Table_of_labels))
  cat("\n")
  cat(str(unique(Table_of_labels$VAR)))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_of_labels$Fig1_Annot_Category)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_of_labels$Fig1_Annot_Category))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_of_labels$MPRA_CLASS)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_of_labels$MPRA_CLASS))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_of_labels$genIE_CLASS)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_of_labels$genIE_CLASS))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_of_labels$M_and_M)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_of_labels$M_and_M))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_of_labels$Multi_Lineage)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_of_labels$Multi_Lineage))))
  cat("\n")
  
  
  levels_MPRA_CLASS_selected<-levels(Table_of_labels$MPRA_CLASS)[c(1,2)]
  
  cat("levels_MPRA_CLASS_selected\n")
  cat(sprintf(as.character(levels_MPRA_CLASS_selected)))
  cat("\n")
  
  levels_M_and_M_selected<-levels(Table_of_labels$M_and_M)[c(1,13)]
  
  cat("levels_M_and_M_selected\n")
  cat(sprintf(as.character(levels_M_and_M_selected)))
  cat("\n")
  
  levels_Multi_Lineage_selected<-levels(Table_of_labels$Multi_Lineage)[c(1,2)]
  
  cat("levels_Multi_Lineage_selected\n")
  cat(sprintf(as.character(levels_Multi_Lineage_selected)))
  cat("\n")
  
  
  #### Categories colors ----
  
  DEF_colors<-readRDS(file=opt$Categories_colors)
  
  cat("DEF_colors_0\n")
  cat(str(DEF_colors))
  cat("\n")
  cat(sprintf(as.character(unique(DEF_colors$Annotation))))
  cat("\n")
  
  DEF_colors_sel_1<-DEF_colors[which(DEF_colors$Annotation == "Fig1_Annot_Category"),]
  
  cat("DEF_colors_sel_1_0\n")
  cat(str(DEF_colors_sel_1))
  cat("\n")
  cat(sprintf(as.character(unique(DEF_colors_sel_1$Annotation))))
  cat("\n")
  
  template_color<-DEF_colors[which(DEF_colors$Annotation == "Fig2_Annot_Category"),]
  
  cat("DEF_colors_sel_2_0\n")
  cat(str(template_color))
  cat("\n")
  cat(sprintf(as.character(unique(template_color$Annotation))))
  cat("\n")
  cat(sprintf(as.character(unique(template_color$colors))))
  cat("\n")
  
  DEF_colors_sel_2<-template_color
  
  DEF_colors_sel_2$Annotation[which(DEF_colors_sel_2$Annotation == "Fig2_Annot_Category")]<-"MPRA_CLASS"
  DEF_colors_sel_2$Category[which(DEF_colors_sel_2$Category == "NON_ACTIVE")]<-levels_MPRA_CLASS_selected[2]
  DEF_colors_sel_2$Category[which(DEF_colors_sel_2$Category == "ACTIVE")]<-levels_MPRA_CLASS_selected[1]
  
  DEF_colors_sel_2$colors[which(DEF_colors_sel_2$colors == "red")]<-"gray"
  
  cat("DEF_colors_sel_2_1\n")
  cat(str(DEF_colors_sel_2))
  cat("\n")
  cat(sprintf(as.character(unique(DEF_colors_sel_2$Annotation))))
  cat("\n")
  cat(sprintf(as.character(unique(DEF_colors_sel_2$colors))))
  cat("\n")
  
  
  
  DEF_colors_sel_3<-template_color
  
  DEF_colors_sel_3$Annotation[which(DEF_colors_sel_3$Annotation == "Fig2_Annot_Category")]<-"M_and_M"
  DEF_colors_sel_3$Category[which(DEF_colors_sel_3$Category == "NON_ACTIVE")]<-levels_M_and_M_selected[1]
  DEF_colors_sel_3$Category[which(DEF_colors_sel_3$Category == "ACTIVE")]<-levels_M_and_M_selected[2]
  
  DEF_colors_sel_3$colors[which(DEF_colors_sel_3$colors == "red")]<-"aquamarine4"
  DEF_colors_sel_3$colors[which(DEF_colors_sel_3$colors == "greenyellow")]<-"deeppink2"
  
  
  
  
  cat("DEF_colors_sel_3_1\n")
  cat(str(DEF_colors_sel_3))
  cat("\n")
  cat(sprintf(as.character(unique(DEF_colors_sel_3$Annotation))))
  cat("\n")
  cat(sprintf(as.character(unique(DEF_colors_sel_3$colors))))
  cat("\n")
  
  DEF_colors_sel_4<-template_color
  
  DEF_colors_sel_4$Annotation[which(DEF_colors_sel_4$Annotation == "Fig2_Annot_Category")]<-"Multi_Lineage"
  DEF_colors_sel_4$Category[which(DEF_colors_sel_4$Category == "NON_ACTIVE")]<-levels_Multi_Lineage_selected[1]
  DEF_colors_sel_4$Category[which(DEF_colors_sel_4$Category == "ACTIVE")]<-levels_Multi_Lineage_selected[2]
  
  
  # DEF_colors_sel_4$colors[which(DEF_colors_sel_4$colors == "red")]<-"gray"
  
  
  cat("DEF_colors_sel_4_1\n")
  cat(str(DEF_colors_sel_4))
  cat("\n")
  cat(sprintf(as.character(unique(DEF_colors_sel_4$Annotation))))
  cat("\n")
  cat(sprintf(as.character(unique(DEF_colors_sel_4$colors))))
  cat("\n")
  
  
  COLORS_DEF<-rbind(DEF_colors_sel_1,DEF_colors_sel_2,DEF_colors_sel_3,DEF_colors_sel_4)
  
  cat("COLORS_DEF_1\n")
  cat(str(COLORS_DEF))
  cat("\n")
  cat(sprintf(as.character(unique(COLORS_DEF$Annotation))))
  cat("\n")
  cat(sprintf(as.character(unique(COLORS_DEF$colors))))
  cat("\n")
  
  #### READ and transform tracking_variants ----
  
  tracking_variants = unlist(strsplit(opt$tracking_variants, split=","))
  
  cat("tracking_variants_\n")
  cat(sprintf(as.character(tracking_variants)))
  cat("\n")
  
  #### Read Master_file ----
  
  Master_file<-as.data.frame(readRDS(file=opt$Master_file) , stringsAsFactors=F)
  
  Master_file$variable<-factor(Master_file$variable,
                               levels=rev(levels(Master_file$variable)),
                               ordered=T)
  
  cat("Master_file_0\n")
  cat(str(Master_file))
  cat("\n")
  cat(str(unique(Master_file$VAR)))
  cat("\n")
  cat(sprintf(as.character(names(summary(Master_file$variable)))))
  cat("\n")
  cat(sprintf(as.character(summary(Master_file$variable))))
  cat("\n")
  
  #### Read Table_S2 ----
  
  Table_S2<-as.data.frame(readRDS(file=opt$Table_S2) , stringsAsFactors=F)
  
  cat("Table_S2_0\n")
  cat(str(Table_S2))
  cat("\n")
  cat(sprintf(as.character(unique(Table_S2$Annotation))))
  cat("\n")
  

  ############## Graph LOOP -----------------
  
  graph_path<-paste(out,'Isolated_violin_plots_Z_score','/',sep='')
  
  if (file.exists(graph_path)){
    
  }else{
    
    dir.create(file.path(graph_path))
  }
  
  setwd(graph_path)
  
  ####   LOOP Through category_vector ----
  
  
  category_vector<-Paper_Annotation_REP
  
  
  List_DEF<-list()
  
  CONDITION_DEBUG<-1
  
  for(i in 1:length(category_vector))
  {
    category_sel<-category_vector[i]
    
    cat("----------------------------------------------------------------------------------->\t")
    cat(sprintf(as.character(category_sel)))
    cat("\n")
    
    Table_S2_single_sel<-droplevels(Table_S2[which(Table_S2$Annotation ==  category_sel &
                                                         Table_S2$variable%in%df_subset_variables$variable),])
    
    if(CONDITION_DEBUG == 1)
    {
      cat("Table_S2_single_sel_0\n")
      cat(str(Table_S2_single_sel))
      cat("\n")
      cat(str(unique(Table_S2_single_sel$Annotation)))
      cat("\n")
    }
    
    
    categories_in_stats<-unique(as.character(c(Table_S2_single_sel$category2,Table_S2_single_sel$category1)))
    
    if(CONDITION_DEBUG == 1)
    {
      cat("categories_in_stats_0\n")
      cat(sprintf(as.character(categories_in_stats)))
      cat("\n")
    }
    
    ### Table_of_labels
    
    indx.category_sel<-which(colnames(Table_of_labels) == category_sel)
    
    if(CONDITION_DEBUG == 1)
    {
      cat("indx.category_sel\n")
      cat(str(indx.category_sel))
      cat("\n")
      
    }
    
    ind.int<-c(which(colnames(Table_of_labels) == "VAR"),indx.category_sel)
    
    
    
    Table_of_labels_subset<-unique(Table_of_labels[,ind.int])
    
    if(CONDITION_DEBUG == 1)
    {
      cat("Table_of_labels_subset_0\n")
      cat(str(Table_of_labels_subset))
      cat("\n")
      
    }
    
    indx.category_sel_subset<-which(colnames(Table_of_labels_subset) == category_sel)
    
    if(CONDITION_DEBUG == 1)
    {
      cat("indx.category_sel_subset_0\n")
      cat(str(indx.category_sel_subset))
      cat("\n")
      
    }
    
    Table_of_labels_subset_restricted<-droplevels(Table_of_labels_subset[which(Table_of_labels_subset[,indx.category_sel_subset]%in%categories_in_stats),])
    
    
    if(CONDITION_DEBUG == 1)
    {
      cat("Table_of_labels_subset_restricted_0\n")
      cat(str(Table_of_labels_subset_restricted))
      cat("\n")
      cat(str(unique(Table_of_labels_subset_restricted$VAR)))
      cat("\n")
      cat(sprintf(as.character(names(summary(Table_of_labels_subset_restricted[,indx.category_sel_subset])))))
      cat("\n")
      cat(sprintf(as.character(summary(Table_of_labels_subset_restricted[,indx.category_sel_subset]))))
      cat("\n")
    }
    
    indx.category_sel_subset<-which(colnames(Table_of_labels_subset_restricted) == category_sel)
    
    if(CONDITION_DEBUG == 1)
    {
      cat("indx.category_sel_subset_1\n")
      cat(str(indx.category_sel_subset))
      cat("\n")
      
    }
    
    if(category_sel == "Multi_Lineage")
    {
      Table_of_labels_subset_restricted[,indx.category_sel_subset]<-factor(Table_of_labels_subset_restricted[,indx.category_sel_subset],
                                                                           levels=rev(levels(Table_of_labels_subset_restricted[,indx.category_sel_subset])),
                                                                           ordered=T)
      
      
      
    }#category_sel == "Multi_Lineage")
    
    order_levels_category<-levels(Table_of_labels_subset_restricted[,indx.category_sel_subset])
    
    if(CONDITION_DEBUG == 1)
    {
      cat("order_levels_category_0\n")
      cat(str(order_levels_category))
      cat("\n")
    }
    
    
    
    #### DotPlot ----
    
    
    Table_S2_single_sel$variable<-factor(Table_S2_single_sel$variable,
                                           levels=rev(df_subset_variables$variable),
                                           ordered=T)
    
    if(CONDITION_DEBUG == 1)
    {
      cat("Table_S2_single_sel_1\n")
      cat(str(Table_S2_single_sel))
      cat("\n")
      cat(sprintf(as.character(unique(Table_S2_single_sel$variable))))
      cat("\n")
    }
    
    if(category_sel == "Fig1_Annot_Category")
    {
      
      Table_S2_single_sel<-Table_S2_single_sel[which(Table_S2_single_sel$category1 == "index_variants" |
                                                           Table_S2_single_sel$category2 == "index_variants"),]
      
    }
    
    if(CONDITION_DEBUG == 1)
    {
      cat("Table_S2_single_sel_2\n")
      cat(str(Table_S2_single_sel))
      cat("\n")
      cat(str(unique(Table_S2_single_sel$Annotation)))
      cat("\n")
    }
    
    indx.int<-c(which(colnames(Table_S2_single_sel) == "category2"),which(colnames(Table_S2_single_sel) == "category1"),which(colnames(Table_S2_single_sel) == "variable"),which(colnames(Table_S2_single_sel) == "MINUS_logpval"))
    
    Table_S2_single_sel_subset<-unique(Table_S2_single_sel[,indx.int])
    
    
    if(CONDITION_DEBUG == 1)
    {
      cat("Table_S2_single_sel_subset_0\n")
      cat(str(Table_S2_single_sel_subset))
      cat("\n")
    }
    
    
    
    Table_S2_single_sel_subset$x_lab<-NA
    
    Table_S2_single_sel_subset$x_lab[which(Table_S2_single_sel_subset$category2 != order_levels_category[length(order_levels_category)])]<-as.character(Table_S2_single_sel_subset$category2[which(Table_S2_single_sel_subset$category2 != order_levels_category[length(order_levels_category)])])
    Table_S2_single_sel_subset$x_lab[which(Table_S2_single_sel_subset$category1 != order_levels_category[length(order_levels_category)])]<-as.character(Table_S2_single_sel_subset$category1[which(Table_S2_single_sel_subset$category1 != order_levels_category[length(order_levels_category)])])
    
    if(CONDITION_DEBUG == 1)
    {
      cat("Table_S2_single_sel_subset_1\n")
      cat(str(Table_S2_single_sel_subset))
      cat("\n")
    }
    
    Table_S2_single_sel_subset$z_lab<-NA
    
    Table_S2_single_sel_subset$z_lab[which(Table_S2_single_sel_subset$category2 == order_levels_category[length(order_levels_category)])]<-as.character(Table_S2_single_sel_subset$category2[which(Table_S2_single_sel_subset$category2 == order_levels_category[length(order_levels_category)])])
    Table_S2_single_sel_subset$z_lab[which(Table_S2_single_sel_subset$category1 == order_levels_category[length(order_levels_category)])]<-as.character(Table_S2_single_sel_subset$category1[which(Table_S2_single_sel_subset$category1 == order_levels_category[length(order_levels_category)])])
    
    order_levels_category_RMV<-order_levels_category[-length(order_levels_category)]
    
    
    if(CONDITION_DEBUG == 1)
    {
      cat("order_levels_category_RMV_0\n")
      cat(str(order_levels_category_RMV))
      cat("\n")
    }
    
    Table_S2_single_sel_subset$x_lab<-factor(Table_S2_single_sel_subset$x_lab,
                                               levels=order_levels_category_RMV,
                                               ordered=T)
    
    if(CONDITION_DEBUG == 1)
    {
      cat("Table_S2_single_sel_subset_2\n")
      cat(str(Table_S2_single_sel_subset))
      cat("\n")
    }
    
    Table_S2_single_sel_subset<-Table_S2_single_sel_subset[order(Table_S2_single_sel_subset$variable,Table_S2_single_sel_subset$x_lab),]
    
    Table_S2_single_sel_subset$Significance<-NA
    
    
    Table_S2_single_sel_subset$Significance[which(Table_S2_single_sel_subset$MINUS_logpval >= 1.3)]<-"SIG"
    Table_S2_single_sel_subset$Significance[which(Table_S2_single_sel_subset$MINUS_logpval < 1.3)]<-"NO_SIG"
    
    
    
    # setwd(out)
    # 
    # write.table(Table_S2_single_sel_subset, file="test.tsv", sep="\t", quote=F,row.names=F)
    
    graph_path<-paste(out,'Isolated_violin_plots_Z_score','/',category_sel,'/',sep='')
    
    if (file.exists(graph_path)){
      
    }else{
      
      dir.create(file.path(graph_path))
    }
    
    setwd(graph_path)
    
    
    
    dotplot<-ggplot(data=Table_S2_single_sel_subset,
                    aes(y=variable,
                        x=z_lab,
                        color=Significance)) +
      geom_point(aes(size=MINUS_logpval), stroke=1)+
      scale_size(range = c(0,10), name='-log10pval') +
      scale_color_manual(values=c("gray","black"),name='Significant') + 
      scale_y_discrete(name=NULL, drop=F)+
      scale_x_discrete(name=NULL, drop=F)+
      facet_wrap('x_lab', nrow=1, scales='free_x')+
      theme_bw()+
      theme_classic()+
      theme(axis.title.y=element_text(size=16, family="sans"),
            axis.text.y=element_text(angle=0,size=16, color="black", family="sans"),
            axis.text.x=element_blank())+
      ggeasy::easy_center_title()
    
    
    
    svgname<-paste("dotplot_value_Z_score",".svg",sep='')
    makesvg = TRUE
    
    if (makesvg == TRUE)
    {
      ggsave(svgname, plot= dotplot,
             device="svg",
             height=10, width=12)
    }
    
    
    
    # quit(status = 1)
    
    
    #### Representation by subgroups of categories ----
    
    array_GROUP<-levels(df_subset_variables$GROUP)
    
    if(CONDITION_DEBUG == 1)
    {
      cat("array_GROUP\n")
      cat(str(array_GROUP))
      cat("\n")
      
    }
    
    
    list_array_GROUP<-list()
    
    for(k in 1:length(array_GROUP))
    {
      array_GROUP_sel<-array_GROUP[k]
      
      cat("------------>\t")
      cat(sprintf(as.character(array_GROUP_sel)))
      cat("\n")
      
      
      graph_path<-paste(out,'Isolated_violin_plots_Z_score','/',category_sel,'/',array_GROUP_sel,'/',sep='')
      
      if (file.exists(graph_path)){
        
      }else{
        
        dir.create(file.path(graph_path))
      }
      
      setwd(graph_path)
      
      #####
      
      df_subset_variables_sel<-df_subset_variables[which(df_subset_variables$GROUP == array_GROUP_sel),]
      
      cat("df_subset_variables_sel\n")
      cat(str(df_subset_variables_sel))
      cat("\n")
      cat(sprintf(as.character(unique(df_subset_variables_sel$variable))))
      cat("\n")
      
      
      Master_file_sel<-droplevels(Master_file[which(Master_file$variable%in%df_subset_variables_sel$variable),])
      
      if(CONDITION_DEBUG == 1)
      {
        cat("Master_file_sel_0\n")
        cat(str(Master_file_sel))
        cat("\n")
        cat(str(unique(Master_file_sel$VAR)))
        cat("\n")
        cat(sprintf(as.character(unique(Master_file_sel$variable))))
        cat("\n")
        cat(sprintf(as.character(names(summary(Master_file_sel$value_Z_score)))))
        cat("\n")
        cat(sprintf(as.character(summary(Master_file_sel$value_Z_score))))
        cat("\n")
      }
      
      ### Add labels
      
      Master_file_sel<-droplevels(merge(Master_file_sel,
                                        Table_of_labels_subset_restricted,
                                        by="VAR"))
      
      if(CONDITION_DEBUG == 1)
      {
        cat("Master_file_sel_1\n")
        cat(str(Master_file_sel))
        cat("\n")
        cat(str(unique(Master_file_sel$VAR)))
        cat("\n")
        cat(sprintf(as.character(names(summary(Master_file_sel$value_Z_score)))))
        cat("\n")
        cat(sprintf(as.character(summary(Master_file_sel$value_Z_score))))
        cat("\n")
      }
      
      Master_file_sel$variable<-factor(Master_file_sel$variable,
                                       levels=rev(df_subset_variables_sel$variable),
                                       ordered=T)
      
      if(CONDITION_DEBUG == 1)
      {
        cat("Master_file_sel_2\n")
        cat(str(Master_file_sel))
        cat("\n")
        cat(str(unique(Master_file_sel$VAR)))
        cat("\n")
        cat(sprintf(as.character(names(summary(Master_file_sel$variable)))))
        cat("\n")
        cat(sprintf(as.character(summary(Master_file_sel$variable))))
        cat("\n")
        
        
       }
      
      
      ind.cat<-c(which(colnames(Master_file_sel) == category_sel))
      
      if(CONDITION_DEBUG == 1)
      {
        cat("ind.cat\n")
        cat(str(ind.cat))
        cat("\n")
      }
      
      
      check_NA<-Master_file_sel[is.na(Master_file_sel$value),]
      
      if(CONDITION_DEBUG == 1)
      {
        cat("check_NA_0\n")
        cat(str(check_NA))
        cat("\n")
        cat(str(unique(check_NA$VAR)))
        cat("\n")
        cat(sprintf(as.character(names(summary(check_NA$value_Z_score)))))
        cat("\n")
        cat(sprintf(as.character(summary(check_NA$value_Z_score))))
        cat("\n")
      }
      
      
      for(l in 1:length(levels(Master_file_sel$variable)))
      {
        variable_sel<-levels(Master_file_sel$variable)[l]
        
        
        cat("----->\t")
        cat(sprintf(as.character(variable_sel)))
        cat("\n")
        
        
        Master_file_sel_FINAL<-droplevels(Master_file_sel[which(Master_file_sel$variable%in%variable_sel),])
        
        if(CONDITION_DEBUG == 1)
        {
          cat("Master_file_sel_FINAL_0\n")
          cat(str(Master_file_sel_FINAL))
          cat("\n")
          cat(str(unique(Master_file_sel_FINAL$VAR)))
          cat("\n")
          cat(sprintf(as.character(names(summary(Master_file_sel_FINAL$variable)))))
          cat("\n")
          cat(sprintf(as.character(summary(Master_file_sel_FINAL$variable))))
          cat("\n")
        }
        
        
        ####  Z score Ranks ----
        
        summary_variable<-summary(Master_file_sel_FINAL$value_Z_score[!is.na(Master_file_sel_FINAL$value_Z_score)])
        max_variable<-max(summary_variable)
        min_variable<-min(summary_variable)
        
        step<-(max_variable-min_variable)/5
        
        if(step == 0)
        {
          
          step<--1
        }
        
        
        
        
        if(CONDITION_DEBUG == 1)
        {
          cat("summary_variable_0\n")
          cat(sprintf(as.character(names(summary_variable))))
          cat("\n")
          cat(sprintf(as.character(summary_variable)))
          cat("\n")
        }
        
        breaks.Rank<-unique(sort(c(0,seq(from= min_variable, to= max_variable+step,by=step))))
        labels.Rank<-as.character(round(breaks.Rank,2))
        
        if(CONDITION_DEBUG == 1)
        {
          cat("labels.Rank_0\n")
          cat(sprintf(as.character(labels.Rank)))
          cat("\n")
        }
        
        COLORS_DEF_sel<-COLORS_DEF[which(COLORS_DEF$Annotation == category_sel),]
        
        if(CONDITION_DEBUG == 1)
        {
          cat("COLORS_DEF_sel_0\n")
          cat(str(COLORS_DEF_sel))
          cat("\n")
        }
        
        if(category_sel == 'Multi_Lineage')
        {
          value_colors<-c('greenyellow','red')
          
        }else{
          
          value_colors<-COLORS_DEF_sel$colors
        }
        
        ########################## Violin plot ------------------------
        
        # geom_violin(scale = "width", adjust = 1, trim = TRUE)+
          
          
        violin_plot <- ggplot(data=Master_file_sel_FINAL, aes(x=Master_file_sel_FINAL[,ind.cat], 
                                                        y=value_Z_score,
                                                        fill = Master_file_sel_FINAL[,ind.cat])) +
          geom_violin(scale = "width", adjust = 1, trim = FALSE, linetype = "solid", size=0.25, draw_quantiles = c(0.25, 0.75))+
          scale_y_continuous(name="Z-Score normalised value",
                             breaks=breaks.Rank,labels=labels.Rank, 
                             limits=c(breaks.Rank[1],breaks.Rank[length(breaks.Rank)]))+
          scale_fill_manual(values = value_colors,
                            drop=F)
        
        
        violin_plot <- violin_plot +
          facet_grid(. ~ variable, scales='free_x', space='free_x',
                     drop=T) +
          theme_cowplot(font_size = 12)+
          scale_x_discrete(name=NULL, breaks=NULL, drop=T)+
          stat_summary(fun = median, fun.min = median, fun.max = median,
                       geom = "crossbar", width = 0.25)+
          theme( strip.background = element_blank(),
                 strip.placement = "inside",
                 strip.text = element_text(size=14, color="black", family="sans", face='bold'),
                 panel.spacing = unit(0.2, "lines"),
                 panel.border = element_rect(colour = "black", fill=NA),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank())+
          theme_classic()+
          theme(axis.title.y=element_text(size=8, color="black", family="sans"),
                axis.title.x=element_blank(),
                axis.text.y=element_text(size=6, color="black", family="sans"),
                axis.text.x=element_blank(),
                axis.line.x = element_line(size = 0.2),
                axis.ticks.x = element_blank(),
                axis.ticks.y = element_line(size = 0.2),
                axis.line.y = element_line(size = 0.2))+
          theme(legend.position="right",
                legend.key.size = unit(1.5, 'cm'), #change legend key size
                legend.key.height = unit(1.5, 'cm'), #change legend key height
                legend.key.width = unit(1, 'cm'), #change legend key width
                legend.title = element_blank(), #change legend title font size
                legend.text = element_text(size=12, color="black", family="sans"))
        
        cat("violin_plot_0\n")
        
        
        setwd(graph_path)
        
        svgname<-paste("NEW_violin_plot_value_Z_score","_",variable_sel,".svg",sep='')
        makesvg = TRUE
        
        if (makesvg == TRUE)
        {
          ggsave(svgname, plot= violin_plot,
                 device="svg",
                 height=13, width=13)
        }
        
        cat("END\n")
      }# l in 1:length(levels(Master_file_sel$variable))
    }#k in 1:length(array_GROUP)
  }#i category_sel
  
}

Selected_graphs_function = function(option_list)
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
  
  #### READ and transform Paper_Annotation_REP ----
  
  Paper_Annotation_REP = unlist(strsplit(opt$Paper_Annotation_REP, split=","))
  
  cat("GWAS_parameters_\n")
  cat(sprintf(as.character(Paper_Annotation_REP)))
  cat("\n")
  
  #### READ and transform GWAS_parameters ----
  
  GWAS_parameters = unlist(strsplit(opt$GWAS_parameters, split=","))
  
  cat("GWAS_parameters_\n")
  cat(sprintf(as.character(GWAS_parameters)))
  cat("\n")
  
  df_GWAS_parameters<-as.data.frame(cbind(rep("GWAS_parameters", length(GWAS_parameters)),GWAS_parameters), stringsAsFactors=F)
  colnames(df_GWAS_parameters)<-c("GROUP","variable")
  
  cat("df_GWAS_parameters_0\n")
  cat(str(df_GWAS_parameters))
  cat("\n")
  
  #### READ and transform Variant_based_scores ----
  
  Variant_based_scores = unlist(strsplit(opt$Variant_based_scores, split=","))
  
  cat("Variant_based_scores_\n")
  cat(sprintf(as.character(Variant_based_scores)))
  cat("\n")
  
  df_Variant_based_scores<-as.data.frame(cbind(rep("Variant_based_scores", length(Variant_based_scores)),Variant_based_scores), stringsAsFactors=F)
  colnames(df_Variant_based_scores)<-c("GROUP","variable")
  
  cat("df_Variant_based_scores_0\n")
  cat(str(df_Variant_based_scores))
  cat("\n")
  
  #### READ and transform Our_rankings ----
  
  Our_rankings = unlist(strsplit(opt$Our_rankings, split=","))
  
  cat("Our_rankings_\n")
  cat(sprintf(as.character(Our_rankings)))
  cat("\n")
  
  df_Our_rankings<-as.data.frame(cbind(rep("Our_rankings", length(Our_rankings)),Our_rankings), stringsAsFactors=F)
  colnames(df_Our_rankings)<-c("GROUP","variable")
  
  cat("df_Our_rankings_0\n")
  cat(str(df_Our_rankings))
  cat("\n")
  
  #### READ and transform Gene_based_features ----
  
  Gene_based_features = unlist(strsplit(opt$Gene_based_features, split=","))
  
  cat("Gene_based_features_\n")
  cat(sprintf(as.character(Gene_based_features)))
  cat("\n")
  
  df_Gene_based_features<-as.data.frame(cbind(rep("Gene_based_features", length(Gene_based_features)),Gene_based_features), stringsAsFactors=F)
  colnames(df_Gene_based_features)<-c("GROUP","variable")
  
  cat("df_Gene_based_features_0\n")
  cat(str(df_Gene_based_features))
  cat("\n")
  
  #### Merge ----
  
  df_subset_variables<-rbind(df_GWAS_parameters,
                             df_Variant_based_scores,
                             df_Our_rankings,
                             df_Gene_based_features)
  
  df_subset_variables$variable<-factor(df_subset_variables$variable,
                                       levels=rev(c(GWAS_parameters,Variant_based_scores,Our_rankings,Gene_based_features)),
                                       ordered=T)
  
  df_subset_variables$GROUP<-factor(df_subset_variables$GROUP,
                                    levels=c("GWAS_parameters","Variant_based_scores","Our_rankings","Gene_based_features"),
                                    ordered=T)
  
  cat("df_subset_variables_0\n")
  cat(str(df_subset_variables))
  cat("\n")
  cat(str(unique(df_subset_variables$GROUP)))
  cat("\n")
  cat(str(unique(df_subset_variables$variable)))
  cat("\n")
  
  
  # #####################################################################
  # quit(status = 1)
  
  #### Read Table_of_labels ----
  
  Table_of_labels<-droplevels(as.data.frame(readRDS(file=opt$Table_of_labels) , stringsAsFactors=F))
  
  cat("Table_of_labels_0\n")
  cat(str(Table_of_labels))
  cat("\n")
  cat(str(unique(Table_of_labels$VAR)))
  cat("\n")
  
  cat("Table_of_labels_0\n")
  cat(str(Table_of_labels))
  cat("\n")
  cat(str(unique(Table_of_labels$VAR)))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_of_labels$Fig1_Annot_Category)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_of_labels$Fig1_Annot_Category))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_of_labels$MPRA_CLASS)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_of_labels$MPRA_CLASS))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_of_labels$genIE_CLASS)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_of_labels$genIE_CLASS))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_of_labels$M_and_M)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_of_labels$M_and_M))))
  cat("\n")
  cat(sprintf(as.character(names(summary(Table_of_labels$Multi_Lineage)))))
  cat("\n")
  cat(sprintf(as.character(summary(Table_of_labels$Multi_Lineage))))
  cat("\n")
  
  
  levels_MPRA_CLASS_selected<-levels(Table_of_labels$MPRA_CLASS)[c(1,2)]
  
  cat("levels_MPRA_CLASS_selected\n")
  cat(sprintf(as.character(levels_MPRA_CLASS_selected)))
  cat("\n")
  
  levels_M_and_M_selected<-levels(Table_of_labels$M_and_M)[c(1,13)]
  
  cat("levels_M_and_M_selected\n")
  cat(sprintf(as.character(levels_M_and_M_selected)))
  cat("\n")
  
  levels_Multi_Lineage_selected<-levels(Table_of_labels$Multi_Lineage)[c(1,2)]
  
  cat("levels_Multi_Lineage_selected\n")
  cat(sprintf(as.character(levels_Multi_Lineage_selected)))
  cat("\n")
  
  
  #### Categories colors ----
  
  DEF_colors<-readRDS(file=opt$Categories_colors)
  
  cat("DEF_colors_0\n")
  cat(str(DEF_colors))
  cat("\n")
  cat(sprintf(as.character(unique(DEF_colors$Annotation))))
  cat("\n")
  
  DEF_colors_sel_1<-DEF_colors[which(DEF_colors$Annotation == "Fig1_Annot_Category"),]
  
  cat("DEF_colors_sel_1_0\n")
  cat(str(DEF_colors_sel_1))
  cat("\n")
  cat(sprintf(as.character(unique(DEF_colors_sel_1$Annotation))))
  cat("\n")
  
  template_color<-DEF_colors[which(DEF_colors$Annotation == "Fig2_Annot_Category"),]
  
  cat("DEF_colors_sel_2_0\n")
  cat(str(template_color))
  cat("\n")
  cat(sprintf(as.character(unique(template_color$Annotation))))
  cat("\n")
  cat(sprintf(as.character(unique(template_color$colors))))
  cat("\n")
  
  DEF_colors_sel_2<-template_color
  
  DEF_colors_sel_2$Annotation[which(DEF_colors_sel_2$Annotation == "Fig2_Annot_Category")]<-"MPRA_CLASS"
  DEF_colors_sel_2$Category[which(DEF_colors_sel_2$Category == "NON_ACTIVE")]<-levels_MPRA_CLASS_selected[2]
  DEF_colors_sel_2$Category[which(DEF_colors_sel_2$Category == "ACTIVE")]<-levels_MPRA_CLASS_selected[1]
  
  DEF_colors_sel_2$colors[which(DEF_colors_sel_2$colors == "red")]<-"gray"
  
  cat("DEF_colors_sel_2_1\n")
  cat(str(DEF_colors_sel_2))
  cat("\n")
  cat(sprintf(as.character(unique(DEF_colors_sel_2$Annotation))))
  cat("\n")
  cat(sprintf(as.character(unique(DEF_colors_sel_2$colors))))
  cat("\n")
  
  
  
  DEF_colors_sel_3<-template_color
  
  DEF_colors_sel_3$Annotation[which(DEF_colors_sel_3$Annotation == "Fig2_Annot_Category")]<-"M_and_M"
  DEF_colors_sel_3$Category[which(DEF_colors_sel_3$Category == "NON_ACTIVE")]<-levels_M_and_M_selected[1]
  DEF_colors_sel_3$Category[which(DEF_colors_sel_3$Category == "ACTIVE")]<-levels_M_and_M_selected[2]
  
  DEF_colors_sel_3$colors[which(DEF_colors_sel_3$colors == "red")]<-"aquamarine4"
  DEF_colors_sel_3$colors[which(DEF_colors_sel_3$colors == "greenyellow")]<-"deeppink2"
  
  
  
  
  cat("DEF_colors_sel_3_1\n")
  cat(str(DEF_colors_sel_3))
  cat("\n")
  cat(sprintf(as.character(unique(DEF_colors_sel_3$Annotation))))
  cat("\n")
  cat(sprintf(as.character(unique(DEF_colors_sel_3$colors))))
  cat("\n")
  
  DEF_colors_sel_4<-template_color
  
  DEF_colors_sel_4$Annotation[which(DEF_colors_sel_4$Annotation == "Fig2_Annot_Category")]<-"Multi_Lineage"
  DEF_colors_sel_4$Category[which(DEF_colors_sel_4$Category == "NON_ACTIVE")]<-levels_Multi_Lineage_selected[1]
  DEF_colors_sel_4$Category[which(DEF_colors_sel_4$Category == "ACTIVE")]<-levels_Multi_Lineage_selected[2]
  
  
  # DEF_colors_sel_4$colors[which(DEF_colors_sel_4$colors == "red")]<-"gray"
  
  
  cat("DEF_colors_sel_4_1\n")
  cat(str(DEF_colors_sel_4))
  cat("\n")
  cat(sprintf(as.character(unique(DEF_colors_sel_4$Annotation))))
  cat("\n")
  cat(sprintf(as.character(unique(DEF_colors_sel_4$colors))))
  cat("\n")
  
  
  COLORS_DEF<-rbind(DEF_colors_sel_1,DEF_colors_sel_2,DEF_colors_sel_3,DEF_colors_sel_4)
  
  cat("COLORS_DEF_1\n")
  cat(str(COLORS_DEF))
  cat("\n")
  cat(sprintf(as.character(unique(COLORS_DEF$Annotation))))
  cat("\n")
  cat(sprintf(as.character(unique(COLORS_DEF$colors))))
  cat("\n")
  
  #### READ and transform tracking_variants ----
  
  tracking_variants = unlist(strsplit(opt$tracking_variants, split=","))
  
  cat("tracking_variants_\n")
  cat(sprintf(as.character(tracking_variants)))
  cat("\n")
  
  #### Read Master_file ----
  
  Master_file<-as.data.frame(readRDS(file=opt$Master_file) , stringsAsFactors=F)
  
  Master_file$variable<-factor(Master_file$variable,
                               levels=rev(levels(Master_file$variable)),
                               ordered=T)
  
  cat("Master_file_0\n")
  cat(str(Master_file))
  cat("\n")
  cat(str(unique(Master_file$VAR)))
  cat("\n")
  cat(sprintf(as.character(names(summary(Master_file$variable)))))
  cat("\n")
  cat(sprintf(as.character(summary(Master_file$variable))))
  cat("\n")
  
  #### Read Table_S2 ----
  
  Table_S2<-as.data.frame(readRDS(file=opt$Table_S2) , stringsAsFactors=F)
  
  cat("Table_S2_0\n")
  cat(str(Table_S2))
  cat("\n")
  cat(sprintf(as.character(unique(Table_S2$Annotation))))
  cat("\n")
  
  
  ############## Graph LOOP -----------------
  
  graph_path<-paste(out,'Isolated_violin_plots_Z_score','/',sep='')
  
  if (file.exists(graph_path)){
    
  }else{
    
    dir.create(file.path(graph_path))
  }
  
  setwd(graph_path)
  
  ####   LOOP Through category_vector ----
  
  
  category_vector<-Paper_Annotation_REP
  
  
  List_DEF<-list()
  
  CONDITION_DEBUG<-1
  
  for(i in 1:length(category_vector))
  {
    category_sel<-category_vector[i]
    
    cat("----------------------------------------------------------------------------------->\t")
    cat(sprintf(as.character(category_sel)))
    cat("\n")
    
    Table_S2_single_sel<-droplevels(Table_S2[which(Table_S2$Annotation ==  category_sel &
                                                         Table_S2$variable%in%df_subset_variables$variable),])
    
    if(CONDITION_DEBUG == 1)
    {
      cat("Table_S2_single_sel_0\n")
      cat(str(Table_S2_single_sel))
      cat("\n")
      cat(str(unique(Table_S2_single_sel$Annotation)))
      cat("\n")
    }
    
    
    categories_in_stats<-unique(as.character(c(Table_S2_single_sel$category2,Table_S2_single_sel$category1)))
    
    if(CONDITION_DEBUG == 1)
    {
      cat("categories_in_stats_0\n")
      cat(sprintf(as.character(categories_in_stats)))
      cat("\n")
    }
    
    ### Table_of_labels
    
    indx.category_sel<-which(colnames(Table_of_labels) == category_sel)
    
    if(CONDITION_DEBUG == 1)
    {
      cat("indx.category_sel\n")
      cat(str(indx.category_sel))
      cat("\n")
      
    }
    
    ind.int<-c(which(colnames(Table_of_labels) == "VAR"),indx.category_sel)
    
    
    
    Table_of_labels_subset<-unique(Table_of_labels[,ind.int])
    
    if(CONDITION_DEBUG == 1)
    {
      cat("Table_of_labels_subset_0\n")
      cat(str(Table_of_labels_subset))
      cat("\n")
      
    }
    
    indx.category_sel_subset<-which(colnames(Table_of_labels_subset) == category_sel)
    
    if(CONDITION_DEBUG == 1)
    {
      cat("indx.category_sel_subset_0\n")
      cat(str(indx.category_sel_subset))
      cat("\n")
      
    }
    
    Table_of_labels_subset_restricted<-droplevels(Table_of_labels_subset[which(Table_of_labels_subset[,indx.category_sel_subset]%in%categories_in_stats),])
    
    
    if(CONDITION_DEBUG == 1)
    {
      cat("Table_of_labels_subset_restricted_0\n")
      cat(str(Table_of_labels_subset_restricted))
      cat("\n")
      cat(str(unique(Table_of_labels_subset_restricted$VAR)))
      cat("\n")
      cat(sprintf(as.character(names(summary(Table_of_labels_subset_restricted[,indx.category_sel_subset])))))
      cat("\n")
      cat(sprintf(as.character(summary(Table_of_labels_subset_restricted[,indx.category_sel_subset]))))
      cat("\n")
    }
    
    indx.category_sel_subset<-which(colnames(Table_of_labels_subset_restricted) == category_sel)
    
    if(CONDITION_DEBUG == 1)
    {
      cat("indx.category_sel_subset_1\n")
      cat(str(indx.category_sel_subset))
      cat("\n")
      
    }
    
    if(category_sel == "Multi_Lineage")
    {
      Table_of_labels_subset_restricted[,indx.category_sel_subset]<-factor(Table_of_labels_subset_restricted[,indx.category_sel_subset],
                                                                           levels=rev(levels(Table_of_labels_subset_restricted[,indx.category_sel_subset])),
                                                                           ordered=T)
      
      
      
    }#category_sel == "Multi_Lineage")
    
    order_levels_category<-levels(Table_of_labels_subset_restricted[,indx.category_sel_subset])
    
    if(CONDITION_DEBUG == 1)
    {
      cat("order_levels_category_0\n")
      cat(str(order_levels_category))
      cat("\n")
    }
    
    
    
    #### Representation by subgroups of categories ----
    
    array_GROUP<-levels(df_subset_variables$GROUP)
    
    if(CONDITION_DEBUG == 1)
    {
      cat("array_GROUP\n")
      cat(str(array_GROUP))
      cat("\n")
      
    }
    
    
    list_array_GROUP<-list()
    
    for(k in 1:length(array_GROUP))
    {
      array_GROUP_sel<-array_GROUP[k]
      
      cat("------------>\t")
      cat(sprintf(as.character(array_GROUP_sel)))
      cat("\n")
      
      
      graph_path<-paste(out,'Isolated_violin_plots_Z_score','/',category_sel,'/',array_GROUP_sel,'/',sep='')
      
      if (file.exists(graph_path)){
        
      }else{
        
        dir.create(file.path(graph_path))
      }
      
      setwd(graph_path)
      
      #####
      
      df_subset_variables_sel<-df_subset_variables[which(df_subset_variables$GROUP == array_GROUP_sel),]
      
      cat("df_subset_variables_sel\n")
      cat(str(df_subset_variables_sel))
      cat("\n")
      cat(sprintf(as.character(unique(df_subset_variables_sel$variable))))
      cat("\n")
      
      
      Master_file_sel<-droplevels(Master_file[which(Master_file$variable%in%df_subset_variables_sel$variable),])
      
      if(CONDITION_DEBUG == 1)
      {
        cat("Master_file_sel_0\n")
        cat(str(Master_file_sel))
        cat("\n")
        cat(str(unique(Master_file_sel$VAR)))
        cat("\n")
        cat(sprintf(as.character(unique(Master_file_sel$variable))))
        cat("\n")
        cat(sprintf(as.character(names(summary(Master_file_sel$value_Z_score)))))
        cat("\n")
        cat(sprintf(as.character(summary(Master_file_sel$value_Z_score))))
        cat("\n")
      }
      
      ### Add labels
      
      Master_file_sel<-droplevels(merge(Master_file_sel,
                                        Table_of_labels_subset_restricted,
                                        by="VAR"))
      
      if(CONDITION_DEBUG == 1)
      {
        cat("Master_file_sel_1\n")
        cat(str(Master_file_sel))
        cat("\n")
        cat(str(unique(Master_file_sel$VAR)))
        cat("\n")
        cat(sprintf(as.character(names(summary(Master_file_sel$value_Z_score)))))
        cat("\n")
        cat(sprintf(as.character(summary(Master_file_sel$value_Z_score))))
        cat("\n")
      }
      
      Master_file_sel$variable<-factor(Master_file_sel$variable,
                                       levels=rev(df_subset_variables_sel$variable),
                                       ordered=T)
      
      if(CONDITION_DEBUG == 1)
      {
        cat("Master_file_sel_2\n")
        cat(str(Master_file_sel))
        cat("\n")
        cat(str(unique(Master_file_sel$VAR)))
        cat("\n")
        cat(sprintf(as.character(names(summary(Master_file_sel$variable)))))
        cat("\n")
        cat(sprintf(as.character(summary(Master_file_sel$variable))))
        cat("\n")
        
        
      }
      
      
      ind.cat<-c(which(colnames(Master_file_sel) == category_sel))
      
      if(CONDITION_DEBUG == 1)
      {
        cat("ind.cat\n")
        cat(str(ind.cat))
        cat("\n")
      }
      
      
      check_NA<-Master_file_sel[is.na(Master_file_sel$value),]
      
      if(CONDITION_DEBUG == 1)
      {
        cat("check_NA_0\n")
        cat(str(check_NA))
        cat("\n")
        cat(str(unique(check_NA$VAR)))
        cat("\n")
        cat(sprintf(as.character(names(summary(check_NA$value_Z_score)))))
        cat("\n")
        cat(sprintf(as.character(summary(check_NA$value_Z_score))))
        cat("\n")
      }
      
      
      for(l in 1:length(levels(Master_file_sel$variable)))
      {
        variable_sel<-levels(Master_file_sel$variable)[l]
        
        
        cat("----->\t")
        cat(sprintf(as.character(variable_sel)))
        cat("\n")
        
        
        Master_file_sel_FINAL<-droplevels(Master_file_sel[which(Master_file_sel$variable%in%variable_sel),])
        
        if(CONDITION_DEBUG == 1)
        {
          cat("Master_file_sel_FINAL_0\n")
          cat(str(Master_file_sel_FINAL))
          cat("\n")
          cat(str(unique(Master_file_sel_FINAL$VAR)))
          cat("\n")
          cat(sprintf(as.character(names(summary(Master_file_sel_FINAL$variable)))))
          cat("\n")
          cat(sprintf(as.character(summary(Master_file_sel_FINAL$variable))))
          cat("\n")
        }
        
        
        ####  Z score Ranks ----
        
        summary_variable<-summary(Master_file_sel_FINAL$value_Z_score[!is.na(Master_file_sel_FINAL$value_Z_score)])
        max_variable<-max(summary_variable)
        min_variable<-min(summary_variable)
        
        step<-(max_variable-min_variable)/5
        
        if(step == 0)
        {
          
          step<--1
        }
        
        
        
        
        if(CONDITION_DEBUG == 1)
        {
          cat("summary_variable_0\n")
          cat(sprintf(as.character(names(summary_variable))))
          cat("\n")
          cat(sprintf(as.character(summary_variable)))
          cat("\n")
        }
        
        breaks.Rank<-unique(sort(c(0,seq(from= min_variable, to= max_variable+step,by=step))))
        labels.Rank<-as.character(round(breaks.Rank,2))
        
        if(CONDITION_DEBUG == 1)
        {
          cat("labels.Rank_0\n")
          cat(sprintf(as.character(labels.Rank)))
          cat("\n")
        }
        
        
        if(variable_sel == 'Absolute_effect_size'){
          
          breaks.Rank<-seq(0,60,by=10)
          labels.Rank<-as.character(round(breaks.Rank,0))
          
          if(CONDITION_DEBUG == 1)
          {
            cat("labels.Rank_CHANGED_Absolute_effect_size\n")
            cat(sprintf(as.character(labels.Rank)))
            cat("\n")
          }
          
        }else{
          
          
          if(variable_sel == 'credset_size'){
            
            breaks.Rank<-seq(-1.5,1.5,by=0.5)
            labels.Rank<-as.character(round(breaks.Rank,1))
            
            if(CONDITION_DEBUG == 1)
            {
              cat("labels.Rank_CHANGED_credset_size\n")
              cat(sprintf(as.character(labels.Rank)))
              cat("\n")
            }
          }else{
            
            if(variable_sel == 'multi_lineage_ATAC'){
              
              breaks.Rank<-seq(-0.5,4,by=0.5)
              labels.Rank<-as.character(round(breaks.Rank,1))
              
              if(CONDITION_DEBUG == 1)
              {
                cat("labels.Rank_CHANGED_multi_lineage_ATAC\n")
                cat(sprintf(as.character(labels.Rank)))
                cat("\n")
              }
            }else{
              
              
            }#variable_sel == 'multi_lineage_ATAC'
            
          }#variable_sel == 'credset_size'
          
        }#variable_sel == 'Absolute_effect_size'
        
        COLORS_DEF_sel<-COLORS_DEF[which(COLORS_DEF$Annotation == category_sel),]
        
        if(CONDITION_DEBUG == 1)
        {
          cat("COLORS_DEF_sel_0\n")
          cat(str(COLORS_DEF_sel))
          cat("\n")
        }
        
        if(category_sel == 'Multi_Lineage')
        {
          value_colors<-c('greenyellow','red')
          
        }else{
          
          value_colors<-COLORS_DEF_sel$colors
        }
        
        ########################## Violin plot ------------------------
        
        violin_plot <- ggplot(data=Master_file_sel_FINAL, aes(x=Master_file_sel_FINAL[,ind.cat], 
                                                              y=value_Z_score,
                                                              fill = Master_file_sel_FINAL[,ind.cat])) +
          geom_violin(scale = "width", adjust = 1, trim = FALSE, linetype = "solid", size=0.25, draw_quantiles = c(0.25, 0.75))+
          scale_y_continuous(name="Z-Score normalised value",
                             breaks=breaks.Rank,labels=labels.Rank, 
                             limits=c(breaks.Rank[1],breaks.Rank[length(breaks.Rank)]))+
          scale_fill_manual(values = value_colors,
                            drop=F)
        
        
        violin_plot <- violin_plot +
          facet_grid(. ~ variable, scales='free_x', space='free_x',
                     drop=T) +
          theme_cowplot(font_size = 2)+
          scale_x_discrete(name=NULL, breaks=NULL, drop=T)+
          stat_summary(fun = median, fun.min = median, fun.max = median,
                       geom = "crossbar", width = 0.25)+
          theme( strip.background = element_blank(),
                 strip.placement = "inside",
                 strip.text = element_blank(),
                 panel.spacing = unit(0.2, "lines"),
                 panel.border = element_rect(colour = "black", fill=NA),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank())+
          theme(plot.title=element_blank(),
                axis.title.y=element_blank(),
                axis.title.x=element_blank(),
                axis.text.y=element_text(size=6, color="black", family="sans"),
                axis.text.x=element_text(size=6, color="black", family="sans"),
                axis.line.x = element_line(size = 0.2),
                axis.line.y = element_line(size = 0.2),
                axis.ticks.x = element_line(size = 0.2),
                axis.ticks.y = element_line(size = 0.2))+
          theme(legend.key.size = unit(0.25, 'cm'), #change legend key size
                legend.key.height = unit(0.25, 'cm'), #change legend key height
                legend.key.width = unit(0.25, 'cm'), #change legend key width
                legend.title = element_text(size=8, family="sans"), #change legend title font size
                legend.text = element_text(size=6, family="sans"),
                legend.position="hidden")+ #change legend text font size
          ggeasy::easy_center_title()
        
        cat("violin_plot_0\n")
        
        

        setwd(graph_path)
        
        svgname<-paste("Paper_NEW_violin_plot_value_Z_score","_",variable_sel,".svg",sep='')
        makesvg = TRUE
        
        if (makesvg == TRUE)
        {
          ggsave(svgname, plot= violin_plot,
                 device="svg",
                 height=1.4, width=0.9)
        }
        
        cat("END\n")
      }# l in 1:length(levels(Master_file_sel$variable))
    }#k in 1:length(array_GROUP)
  }#i category_sel
  
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
    make_option(c("--Table_of_labels"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--Table_S1"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--Paper_Annotation_REP"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--Master_file"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--Table_S2"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--GWAS_parameters"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--Variant_based_scores"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--Our_rankings"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--Gene_based_features"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--Categories_colors"), type="character", default=NULL, 
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
                        --ncboost FILE.txt
                        --type type
                        --out filename",
                        option_list = option_list)
  opt <<- parse_args(parser)
  
  # Isolated_graphs_function(opt)
  Selected_graphs_function(opt)
  
}


###########################################################################

system.time( main() )
