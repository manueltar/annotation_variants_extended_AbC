#!/bin/bash>

MASTER_ROUTE=$1
output_dir=$MASTER_ROUTE

MASTER_ROUTE=$1
analysis=$2


Rscripts_path=$(echo "/home/manuel.tardaguila/Scripts/R/")
module load R/4.1.0


bashrc_file=$(echo "/home/manuel.tardaguila/.bashrc")

source $bashrc_file
eval "$(conda shell.bash hook)"


output_dir=$(echo "$MASTER_ROUTE""/""$analysis""/")

#rm -rf $output_dir
#mkdir -p $output_dir



Log_files=$(echo "$output_dir""Log_files""/")

rm -rf $Log_files
mkdir -p $Log_files

#######################################################################binder_of_scores_AbC ###################################


type=$(echo "binder_of_scores_AbC""_""$analysis")
outfile_binder_of_scores_AbC=$(echo "$Log_files""outfile_1_""$type"".log")
touch $outfile_binder_of_scores_AbC
echo -n "" > $outfile_binder_of_scores_AbC
name_binder_of_scores_AbC=$(echo "$type""_job")
seff_name=$(echo "seff""_""$type")

Rscript_binder_of_scores_AbC=$(echo "$Rscripts_path""297_AbC_binder.R")


ALL_dB=$(echo '/group/soranzo/manuel.tardaguila/ALL_dB/ALL_db.tsv')
tracking_variants=$(echo "chr1_202129205_G_A,chr12_111844956_C_T,chr18_60880701_T_C,chr18_60920854_C_T,chr3_128317978_C_T,chr3_128322617_G_A,chr3_184091102_T_G,chr17_38764524_T_A,chr3_71355240_G_C,chr16_86016328_C_T")
AbC_scores=$(echo "/group/soranzo/manuel.tardaguila/Paper_bits/AbC_Engreitz/AllPredictions.AvgHiC.ABC0.015.minus150.ForABCPaperV3.txt.gz")
AbC_cell_types=$(echo 'erythroblast-Corces2016,CD8-positive_alpha-beta_T_cell-Corces2016,CD4-positive_helper_T_cell-Corces2016,CD14-positive_monocyte_treated_with_LPS_d6-Novakovic2016,CD14-positive_monocyte_treated_with_LPS_4h-Novakovic2016,CD14-positive_monocyte_treated_with_RPMI_4h-Novakovic2016,CD14-positive_monocyte_treated_with_LPS_1h-Novakovic2016,CD14-positive_monocyte_treated_with_BG_4h-Novakovic2016,CD14-positive_monocyte_treated_with_RPMI_d1-Novakovic2016,CD14-positive_monocyte_treated_with_BG_d1-Novakovic2016,natural_killer_cell-Corces2016,CD4-positive_helper_T_cell-ENCODE,CD56-positive_natural_killer_cells-Roadmap,CD14-positive_monocyte_treated_with_BG_1h-Novakovic2016,CD19-positive_B_cell-Roadmap,CD3-positive_T_cell-Roadmap,CD14-positive_monocyte-Novakovic2016,CD14-positive_monocyte_treated_with_RPMI_1h-Novakovic2016,B_cell-ENCODE,CD14-positive_monocyte-ENCODE,CD8-positive_alpha-beta_T_cell-ENCODE,T-cell-ENCODE,CD14-positive_monocyte_treated_with_LPS_d1-Novakovic2016,dendritic_cell_treated_with_Lipopolysaccharide_100_ng-mL_for_6_hour-Garber2017,CD14-positive_monocytes-Roadmap,dendritic_cell_treated_with_Lipopolysaccharide_100_ng-mL_for_4_hour-Garber2017,CD34-positive_mobilized-Roadmap,dendritic_cell_treated_with_Lipopolysaccharide_100_ng-mL_for_30_minute-Garber2017,dendritic_cell_treated_with_Lipopolysaccharide_100_ng-mL_for_1_hour-Garber2017,dendritic_cell_treated_with_Lipopolysaccharide_0_ng-mL_for_0_hour-Garber2017,megakaryocyte-erythroid_progenitor-Corces2016,dendritic_cell_treated_with_Lipopolysaccharide_100_ng-mL_for_2_hour-Garber2017,CD14-positive_monocyte_treated_with_RPMI_d6-Novakovic2016,CD14-positive_monocyte_treated_with_BG_d6-Novakovic2016')
Threshold_AbC=$(echo "0.1")


myjobid_binder_of_scores_AbC=$(sbatch --job-name=$name_binder_of_scores_AbC --output=$outfile_binder_of_scores_AbC --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=4 --mem-per-cpu=4096M --parsable --wrap="Rscript $Rscript_binder_of_scores_AbC --ALL_dB $ALL_dB --tracking_variants $tracking_variants --AbC_scores $AbC_scores --AbC_cell_types $AbC_cell_types --Threshold_AbC $Threshold_AbC --type $type --out $output_dir")
myjobid_seff_binder_of_scores_AbC=$(sbatch --dependency=afterany:$myjobid_binder_of_scores_AbC --open-mode=append --output=$outfile_binder_of_scores_AbC --job-name=$seff_name --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=128M --parsable --wrap="seff $myjobid_binder_of_scores_AbC >> $outfile_binder_of_scores_AbC")


#### z_score_normalize_AbC_scores ###################################


type=$(echo "z_score_normalize_AbC_scores""_""$analysis")
outfile_z_score_normalize_AbC_scores=$(echo "$Log_files""outfile_2_""$type"".log")
touch $outfile_z_score_normalize_AbC_scores
echo -n "" > $outfile_z_score_normalize_AbC_scores
name_z_score_normalize_AbC_scores=$(echo "$type""_job")
seff_name=$(echo "seff""_""$type")

Rscript_z_score_normalize_AbC_scores=$(echo "$Rscripts_path""298_AbC_score.R")



tracking_variants=$(echo "chr1_202129205_G_A,chr12_111844956_C_T,chr18_60880701_T_C,chr18_60920854_C_T,chr3_128317978_C_T,chr3_128322617_G_A,chr3_184091102_T_G,chr17_38764524_T_A,chr3_71355240_G_C,chr16_86016328_C_T")
AbC_score=$(echo "$output_dir""AbC_GLOBAL.tsv")

# --dependency=afterany:$myjobid_binder_of_scores_AbC

myjobid_z_score_normalize_AbC_scores=$(sbatch --dependency=afterany:$myjobid_binder_of_scores_AbC --job-name=$name_z_score_normalize_AbC_scores --output=$outfile_z_score_normalize_AbC_scores --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=2 --mem-per-cpu=1024M --parsable --wrap="Rscript $Rscript_z_score_normalize_AbC_scores --tracking_variants $tracking_variants --AbC_score $AbC_score --type $type --out $output_dir")
myjobid_seff_z_score_normalize_AbC_scores=$(sbatch --dependency=afterany:$myjobid_z_score_normalize_AbC_scores --open-mode=append --output=$outfile_z_score_normalize_AbC_scores --job-name=$seff_name --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=128M --parsable --wrap="seff $myjobid_z_score_normalize_AbC_scores >> $outfile_z_score_normalize_AbC_scores")

#### Add_AbC_to_master_file ###################################


type=$(echo "Add_AbC_to_master_file""_""$analysis")
outfile_Add_AbC_to_master_file=$(echo "$Log_files""outfile_3_""$type"".log")
touch $outfile_Add_AbC_to_master_file
echo -n "" > $outfile_Add_AbC_to_master_file
name_Add_AbC_to_master_file=$(echo "$type""_job")
seff_name=$(echo "seff""_""$type")

Rscript_Add_AbC_to_master_file=$(echo "$Rscripts_path""299_binder_of_Prepared_files_and_add_annotation_layer_add_AbC.R")


Master_file=$(echo "/group/soranzo/manuel.tardaguila/Paper_bits/Master_file_scores.rds")
AbC_score=$(echo "$output_dir""Prepared_file_AbC_score.rds")

# --dependency=afterany:$myjobid_z_score_normalize_AbC_scores

myjobid_Add_AbC_to_master_file=$(sbatch --dependency=afterany:$myjobid_z_score_normalize_AbC_scores --job-name=$name_Add_AbC_to_master_file --output=$outfile_Add_AbC_to_master_file --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=2 --mem-per-cpu=1024M --parsable --wrap="Rscript $Rscript_Add_AbC_to_master_file --Master_file $Master_file --AbC_score $AbC_score --type $type --out $output_dir")
myjobid_seff_Add_AbC_to_master_file=$(sbatch --dependency=afterany:$myjobid_Add_AbC_to_master_file --open-mode=append --output=$outfile_Add_AbC_to_master_file --job-name=$seff_name --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=128M --parsable --wrap="seff $myjobid_Add_AbC_to_master_file >> $outfile_Add_AbC_to_master_file")



