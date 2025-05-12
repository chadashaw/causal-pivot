#!/usr/bin/env bash

# get full path to this script so we can call R script
ROOT_DIR=$(
    cd "$(dirname "${BASH_SOURCE[0]}")"
    pwd -P
)

set -e -x -o pipefail

batch() {
  local n=$1
  local batch_size=$2
  local batches=()

  # Calculate the number of full batches
  local num_batches=$((n / batch_size))
  local remainder=$((n % batch_size))

  # Generate full batches
  for ((i = 0; i < num_batches; i++)); do
    batches+=($batch_size)
  done

  # Add the remainder batch if there is any
  if ((remainder > 0)); then
    batches+=($remainder)
  fi

  # Output the result
  echo "${batches[@]}"
}

model_name=$1
n=$2
outlbl=$3
local=$4

batch_size=256
batches=$(batch $n $batch_size)

# gamma_vals=$(seq '0' '0.1' '1')
# eta_vals='0 0.1'
# gamma_vals='0 0.2 0.35 0.45 0.5 0.55 0.65 0.8 1'
# gamma_vals='0.05 0.1 0.15 0.25 0.3'
# gamma_vals='0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0'
gamma_vals='0.00 0.25 0.50 0.75 1.00'
# gamma_vals='0.55 0.65 0.8 1'
# gamma_vals='0 0.5 1 1.5 2'
# eta_vals='0 0.1'
eta_vals='0'

if [ $local -eq 1 ]; then
  outdir="$ROOT_DIR/.data/results/$outlbl/"
  mkdir -p $outdir
  for test_gamma in $gamma_vals; do
    for test_eta in $eta_vals; do
      for n_batch in $batches; do
        echo "Running local simulations; <gamma=$test_gamma> <eta=$test_eta>"
        time ( \
          cd $ROOT_DIR && \
          Rscript --vanilla --slave lrt.sim.R \
          $model_name $test_gamma $test_eta $n_batch $outdir/$(uuidgen).rds \
        )
      done
    done
  done
else
  outdir=/lrt_results/$outlbl
  dx mkdir -p $outdir

  declare -A dxjobs
  
  for test_gamma in $gamma_vals; do
    for test_eta in $eta_vals; do
      for n_batch in $batches; do
        job_id=$(dx run -y --brief --priority high \
          sim_runner --destination $outdir \
          -imodel_name=$model_name \
          -igamma=$test_gamma \
          -ieta=$test_eta \
          -in_sims=$n_batch)
    
        echo "job $job_id started for gamma=$test_gamma eta=$test_eta n=$n_batch"
        dxjobs["$job_id"]="kicked"
      done
    done
  done
   
  completed_states=("done" "failed" "terminated")
  
  while true; do
    sleep 15
  
    for job_id in "${!dxjobs[@]}"; do
      state=$(dx describe "$job_id" --json | jq -r '.state')
      if [[ ${completed_states[@]} =~ $state ]]; then
        dxjobs["$job_id"]=$state
        unset dxjobs["$job_id"]
        echo "$job_id: $state"
      fi
    done
  
    if [ "${#dxjobs[@]}" -eq 0 ]; then
      break
    fi
  done
  
  mkdir -p ./lrt_results/$outlbl
  
  from="$outdir/*"
  dx download -r $from -o ./lrt_results/$outlbl/
fi

