############################################################
# country and years on which to run the simulations
# currently : ES (Spain) or FR (France)
COUNTRY=IR
INI_YEAR=1910
END_YEAR=2017

# number of parameters' combinations we want to explore.
N_COMBI=1000

# number of simulated populations at each combination of parameters
N_POP=10

# size of the initial population
N=600

# Organisation of results by :
#      - Country
#           - Number of simulated populations
#                - Size of the initial populaiton

mkdir -p ../results/out_files

mkdir -p ../results/$COUNTRY/N_POP_${N_POP}/N_$N

export COUNTRY
export INI_YEAR
export END_YEAR
export N
export N_POP

# create and save sample of parameters used by jobs of the array
Rscript get_paramSet.R $COUNTRY $N_POP $N $N_COMBI

# run the job array
sbatch -J="${COUNTRY}_N${N}_N_POP${N_POP}" --array=1-$N_COMBI --tasks-per-node=$N_POP -n $N_POP send_main.sh


############################################################
############################################################
############################################################
