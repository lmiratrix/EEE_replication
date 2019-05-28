export R_LIBS_USER=$HOME/apps/R:$R_LIBS_USER

for INDEX_VAR in $(seq 1001 1500); do

  #print out indexes
  echo "${INDEX_VAR}"

  #give indexes to R so it can find them.
  export INDEX_VAR 

  #Run R script, and produce output files
  sbatch -o logs/sbout_p${INDEX_VAR}.stdout.txt \
        -e logs/sbout_p${INDEX_VAR}.stderr.txt \
        --job-name=runScr_p${INDEX_VAR} \
        sbatch_runScript.txt
  
  sleep 1 # pause to be kind to the scheduler


done
