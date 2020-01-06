#!/bin/sh

#SBATCH -t 12:00:00 
#SBATCH -o ../results/out_files/Job%A_%a
#SBATCH --mem=20000

mkdir -p ../results/$COUNTRY/N_POP_$N_POP/N_$N/results/param_set_$SLURM_ARRAY_TASK_ID

Rscript main.R $COUNTRY $INI_YEAR $END_YEAR $N $SLURM_ARRAY_TASK_ID $N_POP
