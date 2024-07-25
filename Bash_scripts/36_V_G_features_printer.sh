#!/bin/bash

MASTER_ROUTE=$1
analysis=$2


Rscripts_path=$(echo "/home/manuel.tardaguila/Scripts/R/")
module load R/4.1.0


bashrc_file=$(echo "/home/manuel.tardaguila/.bashrc")

source $bashrc_file
eval "$(conda shell.bash hook)"


output_dir=$(echo "$MASTER_ROUTE""$analysis""/")

#rm -rf $output_dir
#mkdir -p $output_dir

Log_files=$(echo "$output_dir""/""Log_files/")

rm -rf $Log_files
mkdir -p $Log_files

  
#### V_and_G_features_violin_plots #############################


type=$(echo "V_and_G_features_violin_plots""_""$analysis")
outfile_V_and_G_features_violin_plots=$(echo "$Log_files""outfile_1_""$type"".log")
touch $outfile_V_and_G_features_violin_plots
echo -n "" > $outfile_V_and_G_features_violin_plots
name_V_and_G_features_violin_plots=$(echo "$type""_job")
seff_name=$(echo "seff""_""$type")

Rscript_V_and_G_features_violin_plots=$(echo "$Rscripts_path""39_Variant_and_gene_based_features_Z_score_violin_plots_v4.R")


Master_file=$(echo "/group/soranzo/manuel.tardaguila/Paper_bits/Master_file_scores.rds")
Master_file=$(echo "/group/soranzo/manuel.tardaguila/Paper_bits/AbC_Engreitz/AbC_paper/Master_file_scores_added_AbC.rds")


Table_S2=$(echo "/group/soranzo/manuel.tardaguila/Paper_bits/FIX_TABLES/Provisional_Tables/Table_S2_Provisional.rds")
Table_S1=$(echo "/group/soranzo/manuel.tardaguila/Paper_bits/FIX_TABLES/Provisional_Tables/Table_S1_Provisional.rds")
Table_S6=$(echo "/group/soranzo/manuel.tardaguila/Paper_bits/FIX_TABLES/Provisional_Tables/Table_S6_Provisional.rds")
Categories_colors=$(echo "/group/soranzo/manuel.tardaguila/Paper_bits/ER_Labelling_Categories_colors.rds")


GWAS_parameters=$(echo "PP,Absolute_effect_size,credset_size")
#Variant_based_scores=$(echo "CADD_raw,Gnocchi,NCBoost,SpliceAI_DG,SpliceAI_DL,SpliceAI_AG,SpliceAI_AL")
Variant_based_scores=$(echo "CADD_raw,Gnocchi,AbC_score,NCBoost")
Our_rankings=$(echo "Rank_ATAC_erythroid_lineage,Rank_ATAC_mega_lineage,Rank_ATAC_gran_mono_lineage,Rank_ATAC_lymph_lineage,multi_lineage_ATAC,Rank_PCHiC,Rank_chromstates")
Gene_based_features=$(echo "COGS,oe_lof,Rank_GENE_EXP")

tracking_variants=$(echo "chr1_202129205_G_A,chr12_111844956_C_T,chr18_60880701_T_C,chr18_60920854_C_T,chr3_128317978_C_T,chr3_128322617_G_A,chr3_184091102_T_G,chr17_38764524_T_A,chr3_71355240_G_C,chr16_86016328_C_T")



myjobid_V_and_G_features_violin_plots=$(sbatch --job-name=$name_V_and_G_features_violin_plots --output=$outfile_V_and_G_features_violin_plots --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=2 --mem-per-cpu=1024M --parsable --wrap="Rscript $Rscript_V_and_G_features_violin_plots --Master_file $Master_file --Table_S1 $Table_S1 --Table_S6 $Table_S6 --Table_S2 $Table_S2 --Categories_colors $Categories_colors --GWAS_parameters $GWAS_parameters --Variant_based_scores $Variant_based_scores --Our_rankings $Our_rankings --Gene_based_features $Gene_based_features --tracking_variants $tracking_variants  --type $type --out $output_dir")
myjobid_seff_V_and_G_features_violin_plots=$(sbatch --dependency=afterany:$myjobid_V_and_G_features_violin_plots --open-mode=append --output=$outfile_V_and_G_features_violin_plots --job-name=$seff_name --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=128M --parsable --wrap="seff $myjobid_V_and_G_features_violin_plots >> $outfile_V_and_G_features_violin_plots")


#### Isolated_V_and_G_features_violin_plots #############################


type=$(echo "Isolated_V_and_G_features_violin_plots""_""$analysis")
outfile_Isolated_V_and_G_features_violin_plots=$(echo "$Log_files""outfile_2_""$type"".log")
touch $outfile_Isolated_V_and_G_features_violin_plots
echo -n "" > $outfile_Isolated_V_and_G_features_violin_plots
name_Isolated_V_and_G_features_violin_plots=$(echo "$type""_job")
seff_name=$(echo "seff""_""$type")

Rscript_Isolated_V_and_G_features_violin_plots=$(echo "$Rscripts_path""44_Variant_and_gene_based_features_Z_score_violin_plots_ISOLATED_v2.R")


#Master_file=$(echo "/group/soranzo/manuel.tardaguila/Paper_bits/Master_file_scores.rds")
Master_file=$(echo "/group/soranzo/manuel.tardaguila/Paper_bits/AbC_Engreitz/AbC_paper/Master_file_scores_added_AbC.rds")
Table_S2=$(echo "/group/soranzo/manuel.tardaguila/Paper_bits/FIX_TABLES/Provisional_Tables/Table_S2_Provisional.rds")


Table_S1=$(echo "/group/soranzo/manuel.tardaguila/Paper_bits/Table_S1.rds")
Categories_colors=$(echo "/group/soranzo/manuel.tardaguila/Paper_bits/ER_Labelling_Categories_colors.rds")


GWAS_parameters=$(echo "PP,Absolute_effect_size,credset_size")
#Variant_based_scores=$(echo "CADD_raw,Gnocchi,NCBoost,SpliceAI_DG,SpliceAI_DL,SpliceAI_AG,SpliceAI_AL")
Variant_based_scores=$(echo "CADD_raw,Gnocchi,AbC_score,NCBoost")
Our_rankings=$(echo "Rank_ATAC_erythroid_lineage,Rank_ATAC_mega_lineage,Rank_ATAC_gran_mono_lineage,Rank_ATAC_lymph_lineage,multi_lineage_ATAC,Rank_PCHiC,Rank_chromstates")
Gene_based_features=$(echo "COGS,oe_lof,Rank_GENE_EXP")
Paper_Annotation_REP=$(echo 'Multi_Lineage')
tracking_variants=$(echo "chr1_202129205_G_A,chr12_111844956_C_T,chr18_60880701_T_C,chr18_60920854_C_T,chr3_128317978_C_T,chr3_128322617_G_A,chr3_184091102_T_G,chr17_38764524_T_A,chr3_71355240_G_C,chr16_86016328_C_T")
Table_of_labels=$(echo "/group/soranzo/manuel.tardaguila/Paper_bits/Table_of_labels.rds")


myjobid_Isolated_V_and_G_features_violin_plots=$(sbatch --job-name=$name_Isolated_V_and_G_features_violin_plots --output=$outfile_Isolated_V_and_G_features_violin_plots --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=2 --mem-per-cpu=1024M --parsable --wrap="Rscript $Rscript_Isolated_V_and_G_features_violin_plots --Master_file $Master_file --Table_S2 $Table_S2 --Table_S1 $Table_S1 --Table_of_labels $Table_of_labels --Categories_colors $Categories_colors --GWAS_parameters $GWAS_parameters --Variant_based_scores $Variant_based_scores --Our_rankings $Our_rankings --Gene_based_features $Gene_based_features --tracking_variants $tracking_variants --Paper_Annotation_REP $Paper_Annotation_REP  --type $type --out $output_dir")
myjobid_seff_Isolated_V_and_G_features_violin_plots=$(sbatch --dependency=afterany:$myjobid_Isolated_V_and_G_features_violin_plots --open-mode=append --output=$outfile_Isolated_V_and_G_features_violin_plots --job-name=$seff_name --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=128M --parsable --wrap="seff $myjobid_Isolated_V_and_G_features_violin_plots >> $outfile_Isolated_V_and_G_features_violin_plots")
