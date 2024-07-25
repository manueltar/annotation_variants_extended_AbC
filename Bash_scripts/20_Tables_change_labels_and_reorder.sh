#!/bin/bash

analysis=$1
MASTER_ROUTE=$2

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

#### Table_S8_Multiome_DE_and_DA ############################# Table_S8_Multiome_DE_and_DA


type=$(echo "Table_S8_Multiome_DE_and_DA""_""$analysis")
outfile_Table_S8_Multiome_DE_and_DA=$(echo "$Log_files""outfile_0_""$type"".log")
touch $outfile_Table_S8_Multiome_DE_and_DA
echo -n "" > $outfile_Table_S8_Multiome_DE_and_DA
name_Table_S8_Multiome_DE_and_DA=$(echo "$type""_job")
seff_name=$(echo "seff""_""$type")

Rscript_Table_S8_Multiome_DE_and_DA=$(echo "$Rscripts_path""262_Table_S8_Multiome_DE_and_DA_builder.R")

Multiome_DE=$(echo "/group/soranzo/manuel.tardaguila/SC_RNA_seq/k562_multiome/NEW_object_output/Per_cluster_DESeq2_time_covariate_model/DE_results.rds")
Multiome_DA=$(echo "/group/soranzo/manuel.tardaguila/SC_RNA_seq/k562_multiome/NEW_object_output/Per_cluster_DESeq2_time_covariate_model_ATAC/DA_results.rds")

myjobid_Table_S8_Multiome_DE_and_DA=$(sbatch --job-name=$name_Table_S8_Multiome_DE_and_DA --output=$outfile_Table_S8_Multiome_DE_and_DA --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=4 --mem-per-cpu=4096M --parsable --wrap="Rscript $Rscript_Table_S8_Multiome_DE_and_DA --Multiome_DA $Multiome_DA  --Multiome_DE $Multiome_DE --type $type --out $output_dir")
myjobid_seff_Table_S8_Multiome_DE_and_DA=$(sbatch --dependency=afterany:$myjobid_Table_S8_Multiome_DE_and_DA --open-mode=append --output=$outfile_Table_S8_Multiome_DE_and_DA --job-name=$seff_name --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=128M --parsable --wrap="seff $myjobid_Table_S8_Multiome_DE_and_DA >> $outfile_Table_S8_Multiome_DE_and_DA")



#### Table_S6_M_M ############################# Table_S6_M_M


type=$(echo "Table_S6_M_M""_""$analysis")
outfile_Table_S6_M_M=$(echo "$Log_files""outfile_5_""$type"".log")
touch $outfile_Table_S6_M_M
echo -n "" > $outfile_Table_S6_M_M
name_Table_S6_M_M=$(echo "$type""_job")
seff_name=$(echo "seff""_""$type")

Rscript_Table_S6_M_M=$(echo "$Rscripts_path""66_Table_S6_Manual_curation_v2.R")

#Table_S6=$(echo "/group/soranzo/manuel.tardaguila/Paper_bits/Reclassif_5/Table_S6_corrected.rds")
#Table_S6=$(echo "/group/soranzo/manuel.tardaguila/Paper_bits/Reclassif_5/Table_S6_corrected_reverted.rds")
Table_S6=$(echo "/group/soranzo/manuel.tardaguila/Paper_bits/Reclassif_5/Table_S6_corrected_reverted_reverted_ATU_of_reg_proxies.rds")
refined_manual_curation=$(echo "/group/soranzo/manuel.tardaguila/Paper_bits/FIX_TABLES/SCP_Table_S6_Manual_curation_with_TF_class_NEW_CANDIDATE_GENES.txt")


check_SNPS=$(echo "rs148548662,rs192828153,rs543594419,rs35301716,rs142122062,rs150640087,rs187715179,rs200489612,rs114050631,rs112233623")
check_SNPS=$(echo "rs200489612,rs114050631,rs112233623")

myjobid_Table_S6_M_M=$(sbatch --job-name=$name_Table_S6_M_M --output=$outfile_Table_S6_M_M --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=1024M --parsable --wrap="Rscript $Rscript_Table_S6_M_M --Table_S6 $Table_S6 --refined_manual_curation $refined_manual_curation --check_SNPS $check_SNPS --type $type --out $output_dir")
myjobid_seff_Table_S6_M_M=$(sbatch --dependency=afterany:$myjobid_Table_S6_M_M --open-mode=append --output=$outfile_Table_S6_M_M --job-name=$seff_name --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=128M --parsable --wrap="seff $myjobid_Table_S6_M_M >> $outfile_Table_S6_M_M")


#### Table_S1_Variant_Prioritisation ############################# Table_S1_Variant_Prioritisation


type=$(echo "Table_S1_Variant_Prioritisation""_""$analysis")
outfile_Table_S1_Variant_Prioritisation=$(echo "$Log_files""outfile_1_""$type"".log")
touch $outfile_Table_S1_Variant_Prioritisation
echo -n "" > $outfile_Table_S1_Variant_Prioritisation
name_Table_S1_Variant_Prioritisation=$(echo "$type""_job")
seff_name=$(echo "seff""_""$type")

Rscript_Table_S1_Variant_Prioritisation=$(echo "$Rscripts_path""69_Table_S1_Variant_Prioritisation_v2.R")

Table_S1=$(echo "/group/soranzo/manuel.tardaguila/Paper_bits/Original_Paper_tables/Table_S1_Variant_Prioritisation.tsv")
Table_S6=$(echo "/group/soranzo/manuel.tardaguila/Paper_bits/FIX_TABLES/Provisional_Tables/Table_S6_Provisional.rds")



myjobid_Table_S1_Variant_Prioritisation=$(sbatch --dependency=afterany:$myjobid_Table_S6_M_M --job-name=$name_Table_S1_Variant_Prioritisation --output=$outfile_Table_S1_Variant_Prioritisation --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=4 --mem-per-cpu=4096M --parsable --wrap="Rscript $Rscript_Table_S1_Variant_Prioritisation --Table_S1 $Table_S1 --Table_S6 $Table_S6 --type $type --out $output_dir")
myjobid_seff_Table_S1_Variant_Prioritisation=$(sbatch --dependency=afterany:$myjobid_Table_S1_Variant_Prioritisation --open-mode=append --output=$outfile_Table_S1_Variant_Prioritisation --job-name=$seff_name --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=128M --parsable --wrap="seff $myjobid_Table_S1_Variant_Prioritisation >> $outfile_Table_S1_Variant_Prioritisation")




#### Print_Table_S2 #############################


type=$(echo "Print_Table_S2""_""$analysis")
outfile_Print_Table_S2=$(echo "$Log_files""outfile_2_""$type"".log")
touch $outfile_Print_Table_S2
echo -n "" > $outfile_Print_Table_S2
name_Print_Table_S2=$(echo "$type""_job")
seff_name=$(echo "seff""_""$type")

Rscript_Print_Table_S2=$(echo "$Rscripts_path""118_STATS_on_Z_score_and_Table_S2_v2_added_4_categories_and_AbC.R")

Master_file=$(echo "/group/soranzo/manuel.tardaguila/Paper_bits/Master_file_scores.rds")
Master_file=$(echo "/group/soranzo/manuel.tardaguila/Paper_bits/AbC_Engreitz/AbC_paper/Master_file_scores_added_AbC.rds")
Table_S1=$(echo "/group/soranzo/manuel.tardaguila/Paper_bits/FIX_TABLES/Provisional_Tables/Table_S1_Provisional.rds")
Table_S6=$(echo "/group/soranzo/manuel.tardaguila/Paper_bits/FIX_TABLES/Provisional_Tables/Table_S6_Provisional.rds")
GWAS_parameters=$(echo "PP,Absolute_effect_size,credset_size,MAF")
Variant_based_scores=$(echo "CADD_raw,Gnocchi,AbC_score,NCBoost,SpliceAI_DG,SpliceAI_DL,SpliceAI_AG,SpliceAI_AL")
#Variant_based_scores=$(echo "CADD_raw,Gnocchi,NCBoost")
Our_rankings=$(echo "Rank_ATAC_erythroid_lineage,Rank_ATAC_mega_lineage,Rank_ATAC_gran_mono_lineage,Rank_ATAC_lymph_lineage,multi_lineage_ATAC,Rank_PCHiC,Rank_chromstates")
Gene_based_features=$(echo "COGS,oe_lof,Rank_GENE_EXP")

# --dependency=afterany:$myjobid_Table_S1_Variant_Prioritisation:$myjobid_Table_S6_M_M

myjobid_Print_Table_S2=$(sbatch --dependency=afterany:$myjobid_Table_S1_Variant_Prioritisation:$myjobid_Table_S6_M_M --job-name=$name_Print_Table_S2 --output=$outfile_Print_Table_S2 --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=4 --mem-per-cpu=1024M --parsable --wrap="Rscript $Rscript_Print_Table_S2 --Master_file $Master_file --Table_S1 $Table_S1 --Table_S6 $Table_S6  --GWAS_parameters $GWAS_parameters --Variant_based_scores $Variant_based_scores --Our_rankings $Our_rankings --Gene_based_features $Gene_based_features --type $type --out $output_dir")
myjobid_seff_Print_Table_S2=$(sbatch --dependency=afterany:$myjobid_Print_Table_S2 --open-mode=append --output=$outfile_Print_Table_S2 --job-name=$seff_name --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=128M --parsable --wrap="seff $myjobid_Print_Table_S2 >> $outfile_Print_Table_S2")



#### Table_S3_Blood_indices_lineages_and_cell_types ############################# Table_S3_Blood_indices_lineages_and_cell_types


type=$(echo "Table_S3_Blood_indices_lineages_and_cell_types""_""$analysis")
outfile_Table_S3_Blood_indices_lineages_and_cell_types=$(echo "$Log_files""outfile_3_""$type"".log")
touch $outfile_Table_S3_Blood_indices_lineages_and_cell_types
echo -n "" > $outfile_Table_S3_Blood_indices_lineages_and_cell_types
name_Table_S3_Blood_indices_lineages_and_cell_types=$(echo "$type""_job")
seff_name=$(echo "seff""_""$type")

Rscript_Table_S3_Blood_indices_lineages_and_cell_types=$(echo "$Rscripts_path""70_Table_S3_Blood_indices_lineages_and_cell_types.R")

Table_S3=$(echo "/group/soranzo/manuel.tardaguila/Paper_bits/Original_Paper_tables/Table_S3_Blood_indices_lineages_and_cell_types_v2.tsv")

myjobid_Table_S3_Blood_indices_lineages_and_cell_types=$(sbatch --job-name=$name_Table_S3_Blood_indices_lineages_and_cell_types --output=$outfile_Table_S3_Blood_indices_lineages_and_cell_types --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=1024M --parsable --wrap="Rscript $Rscript_Table_S3_Blood_indices_lineages_and_cell_types --Table_S3 $Table_S3 --type $type --out $output_dir")
myjobid_seff_Table_S3_Blood_indices_lineages_and_cell_types=$(sbatch --dependency=afterany:$myjobid_Table_S3_Blood_indices_lineages_and_cell_types --open-mode=append --output=$outfile_Table_S3_Blood_indices_lineages_and_cell_types --job-name=$seff_name --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=128M --parsable --wrap="seff $myjobid_Table_S3_Blood_indices_lineages_and_cell_types >> $outfile_Table_S3_Blood_indices_lineages_and_cell_types")

#### Table_S5_MPRA_results ############################# Table_S5_MPRA_results


type=$(echo "Table_S5_MPRA_results""_""$analysis")
outfile_Table_S5_MPRA_results=$(echo "$Log_files""outfile_4_""$type"".log")
touch $outfile_Table_S5_MPRA_results
echo -n "" > $outfile_Table_S5_MPRA_results
name_Table_S5_MPRA_results=$(echo "$type""_job")
seff_name=$(echo "seff""_""$type")

Rscript_Table_S5_MPRA_results=$(echo "$Rscripts_path""68_Table_S5_MPRA_results.R")

Table_S5=$(echo "/group/soranzo/manuel.tardaguila/Paper_bits/Reclassif_5/Table_S5_corrected.rds")

myjobid_Table_S5_MPRA_results=$(sbatch --job-name=$name_Table_S5_MPRA_results --output=$outfile_Table_S5_MPRA_results --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=1024M --parsable --wrap="Rscript $Rscript_Table_S5_MPRA_results --Table_S5 $Table_S5 --type $type --out $output_dir")
myjobid_seff_Table_S5_MPRA_results=$(sbatch --dependency=afterany:$myjobid_Table_S5_MPRA_results --open-mode=append --output=$outfile_Table_S5_MPRA_results --job-name=$seff_name --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=128M --parsable --wrap="seff $myjobid_Table_S5_MPRA_results >> $outfile_Table_S5_MPRA_results")





#### Table_S7_DE_ATU ############################# Table_S7_DE_ATU


type=$(echo "Table_S7_DE_ATU""_""$analysis")
outfile_Table_S7_DE_ATU=$(echo "$Log_files""outfile_6_""$type"".log")
touch $outfile_Table_S7_DE_ATU
echo -n "" > $outfile_Table_S7_DE_ATU
name_Table_S7_DE_ATU=$(echo "$type""_job")
seff_name=$(echo "seff""_""$type")

Rscript_Table_S7_DE_ATU=$(echo "$Rscripts_path""67_Table_S7_DE_ATU_v2.R")

Table_S7=$(echo "/group/soranzo/manuel.tardaguila/Paper_bits/Table_S7.rds")
Table_S6=$(echo "/group/soranzo/manuel.tardaguila/Paper_bits/Reclassif_5/Table_S6_corrected.rds")
VAR_to_gene_body_correspondence=$(echo "/group/soranzo/manuel.tardaguila/Paper_bits/Redo_Fig2/Table_of_correspondences_upsetr_plus_Gene_body_CLASS.rds")

myjobid_Table_S7_DE_ATU=$(sbatch --job-name=$name_Table_S7_DE_ATU --output=$outfile_Table_S7_DE_ATU --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=1024M --parsable --wrap="Rscript $Rscript_Table_S7_DE_ATU --Table_S7 $Table_S7 --Table_S6 $Table_S6 --VAR_to_gene_body_correspondence $VAR_to_gene_body_correspondence --type $type --out $output_dir")
myjobid_seff_Table_S7_DE_ATU=$(sbatch --dependency=afterany:$myjobid_Table_S7_DE_ATU --open-mode=append --output=$outfile_Table_S7_DE_ATU --job-name=$seff_name --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=128M --parsable --wrap="seff $myjobid_Table_S7_DE_ATU >> $outfile_Table_S7_DE_ATU")

#### Print All in the new order ###################################


type=$(echo "Reassignation""_""$analysis")
outfile_Reassignation=$(echo "$Log_files""outfile_7_""$type"".log")
touch $outfile_Reassignation
echo -n "" > $outfile_Reassignation
name_Reassignation=$(echo "$type""_job")
seff_name=$(echo "seff""_""$type")

Rscript_Reassignation=$(echo "$Rscripts_path""71_Reassignation.R")

Table_S1=$(echo "/group/soranzo/manuel.tardaguila/Paper_bits/FIX_TABLES/Provisional_Tables/Table_S1_Provisional.rds")
Table_S2=$(echo "/group/soranzo/manuel.tardaguila/Paper_bits/FIX_TABLES/Provisional_Tables/Table_S2_Provisional.rds")
Table_S3=$(echo "/group/soranzo/manuel.tardaguila/Paper_bits/FIX_TABLES/Provisional_Tables/Table_S3_Provisional.rds")
Table_S5=$(echo "/group/soranzo/manuel.tardaguila/Paper_bits/FIX_TABLES/Provisional_Tables/Table_S5_Provisional.rds")
Table_S6=$(echo "/group/soranzo/manuel.tardaguila/Paper_bits/FIX_TABLES/Provisional_Tables/Table_S6_Provisional.rds")
Table_S7=$(echo "/group/soranzo/manuel.tardaguila/Paper_bits/FIX_TABLES/Provisional_Tables/Table_S7_Provisional.rds")
Table_S8=$(echo "/group/soranzo/manuel.tardaguila/Paper_bits/FIX_TABLES/Provisional_Tables/Table_S8_SIG_Provisional.rds")

check_SNPS=$(echo "rs200489612,rs114050631,rs112233623")

# --dependency=afterany:$myjobid_Table_S8_Multiome_DE_and_DA:$myjobid_Table_S1_Variant_Prioritisation:$myjobid_Print_Table_S2:$myjobid_Table_S3_Blood_indices_lineages_and_cell_types:$myjobid_Table_S5_MPRA_results:$myjobid_Table_S6_M_M:$myjobid_Table_S7_DE_ATU

myjobid_Reassignation=$(sbatch --dependency=afterany:$myjobid_Table_S8_Multiome_DE_and_DA:$myjobid_Table_S1_Variant_Prioritisation:$myjobid_Print_Table_S2:$myjobid_Table_S3_Blood_indices_lineages_and_cell_types:$myjobid_Table_S5_MPRA_results:$myjobid_Table_S6_M_M:$myjobid_Table_S7_DE_ATU --job-name=$name_Reassignation --output=$outfile_Reassignation --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=2 --mem-per-cpu=1024M --parsable --wrap="Rscript $Rscript_Reassignation --Table_S1 $Table_S1 --Table_S2 $Table_S2 --Table_S3 $Table_S3 --Table_S5 $Table_S5 --Table_S6 $Table_S6 --Table_S7 $Table_S7 --Table_S8 $Table_S8 --check_SNPS $check_SNPS --type $type --out $output_dir")
myjobid_seff_Reassignation=$(sbatch --dependency=afterany:$myjobid_Reassignation --open-mode=append --output=$outfile_Reassignation --job-name=$seff_name --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=128M --parsable --wrap="seff $myjobid_Reassignation >> $outfile_Reassignation")

