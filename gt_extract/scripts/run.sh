#!/usr/bin/env bash

# This script is version 2 of the TCG exome extraction procedure.
# It is meant to be run inside a RAP cloud_workstation w/ the necessary
# dependencies installed.
# This script will resume failed runs if given a single "run_id" input parameter
# Inputs:
#   - cohort name - relative path (from the project root) of a CohortBrowser dataset
#   - bed file path - relative path (from the project root) of a genomic regions bedfile
# Process (v2):
#   - download cohort participant IDs (eids)
#   - loop over chromosome numbers (1-22,X,Y)
#     - download BGEN files for chromosome (.bgen, .bgen.bgi, .sample)
#     - extract variants from cohort samples and genomic regions - vcf.gz
#     - upload intermediate
#   - download intermediate chromosome files (if not present)
#   - concat chromsomes to VCF
#   - annotate VCF and upload

set -e
set -x
set -o pipefail

# make a tmpdir w/ trap that will delete on exit
TMPDIR="$(mktemp -d)"
trap 'rm -rf -- "$TMPDIR"' EXIT

SCRIPT_DIR=$(
    cd "$(dirname "${BASH_SOURCE[0]}")"
    pwd -P
)

new_run=0
if [ "$#" -eq "1" ]; then
    # a single parameter indicates we're resuming a prior run
    run_id=$1
elif [ "$#" -eq "2" ]; then
    # two parameters indicates we're starting a new run
    new_run=1
    run_id=$(python -c "from names_generator import generate_name; print(generate_name());")
    cohort_record_name=$1
    bedfile_path=$2
else
    echo >&2 "Start New Job: $0 cohort_record_name bedfile_path\nResume Job: $0 run_id"
    exit 1
fi

# we source common.sh to define some constants and functions that are used in
# both this script and run_batch.sh
source $SCRIPT_DIR/common.sh $run_id

# set up and move to our local workspace
echo >&2 "<run_id=$run_id>"
RUN_DIR=~/runs/$run_id
if [ $new_run -eq 0 ]; then
    rm -rf $RUN_DIR
fi
dx mkdir -p /runs/$run_id/
mkdir -p $RUN_DIR
cd $RUN_DIR

# define important filename variables
eid_list=$RUN_DIR/eids.txt
merged_lbl=$RUN_DIR/merged
error_list=$RUN_DIR/error-list.txt
runlog=$RUN_DIR/run.log

if [ $new_run -eq 1 ]; then
    # init new run by pulling cohort IDs, downloading bedfile and uploading to dx

    echo >&2 "Downloading cohort EIDs"
    dx extract_dataset "${cohort_record_name}" --fields="participant.eid" --output="${eid_list}"
    sed -i '1d' "${eid_list}"

    echo >&2 "Downloading bedfile"
    dx download --no-progress $bedfile_path -o $RUN_DIR/
    bed="$RUN_DIR/$(basename $bedfile_path)"

    echo >&2 "Uploading to init files to dx"
    upload_result $eid_list
    upload_result $bed
else
    # resume prior run by downloading run files to our local workspace
    echo >&2 "Downloading files for: $run_id"
    dx download -rf /runs/$run_id/* -o $RUN_DIR/
    bed=$(ls $RUN_DIR/*.bed)
    find $RUN_DIR -name '*.vcf.csi' -exec touch {} \; # to avoid annoying "index older than vcf" warnings from bcftools
fi

# set prefix for remote BGEN files
WES_BGEN_DIR="/Bulk/Exome sequences/Population level exome OQFE variants, BGEN format - final release/"
# create output directories
BGEN_INPUTS_DIR="$RUN_DIR/bgen_inputs"
mkdir -p "$BGEN_INPUTS_DIR"
VCF_OUTPUTS_DIR="$RUN_DIR/vcf_outputs"
mkdir -p "$VCF_OUTPUTS_DIR"
PGEN_OUTPUTS_DIR="$RUN_DIR/pgen_outputs"
mkdir -p "$PGEN_OUTPUTS_DIR"
# clear "pmerge-list" list file
pmerge_list_file="$PGEN_OUTPUTS_DIR/pmerge.list"
if [ -f "${pmerge_list_file}" ]; then rm "${pmerge_list_file}"; fi

# convert eid list into FID IID list for plink2
samples_to_keep="$TMPDIR/samples.txt"
paste "${eid_list}" "${eid_list}" > "${samples_to_keep}"
# remove "chr" prefix - if any - from BED entries
regions_to_extract=$TMPDIR/regions.bed""
cat "${bed}" | sed 's/^chr//' > "${regions_to_extract}"

# get 90% of free memory to give to plink2
free_mem=$(free | grep 'Mem:' | sed 's/\s\+/,/g' | cut -d',' -f7);
use_mem=$(echo "result = $free_mem / 1000.0 * 0.9; scale=0; result / 1" | bc)

# loop over chromosomes and extract samples/regions
for chrom in {1..22} X Y; do
  # remote chrom BGEN prefix
  dx_bgen_prefix="ukb23159_c${chrom}_b0_v1"
  # local label for files
  bgen_lbl="$BGEN_INPUTS_DIR/chr${chrom}"
  # skip if we already have output
  out_lbl="$PGEN_OUTPUTS_DIR/chr${chrom}"
  if [[ ! -f "${out_lbl}.pgen" ]]; then
    >&2 echo "Downloading BGEN files for chromosome ${chrom}"
    # loop over BGEN file suffixes
    for bgen_suffix in "bgen" "bgen.bgi" "sample"; do
      # skip if file exists
      if [ -f "${bgen_lbl}.${bgen_suffix}" ]; then
        continue
      fi
      # download file
      dx download --no-progress --lightweight \
        "$WES_BGEN_DIR/${dx_bgen_prefix}.${bgen_suffix}" -o "${bgen_lbl}.${bgen_suffix}"
    done

    >&2 echo "Extracting variants from chromosome ${chrom}"
    # extract variants
    set +e
    plink2 \
      --memory $use_mem \
      --split-par b38 \
      --bgen "${bgen_lbl}.bgen" ref-first \
      --sample "${bgen_lbl}.sample" \
      --keep "${samples_to_keep}" \
      --extract bed0 "${regions_to_extract}" \
      --make-pgen \
      --out "${out_lbl}"
    exit_status=$?
    set -e

    if [ $exit_status -ne 0 ]; then
      >&2 echo "Variant extraction failed for ${chrom} - exit_status: $exit_status"
      continue
    fi

    >&2 echo "Uploading extracted markers from chromosome ${chrom}"
    dst_dir=$(basename $PGEN_OUTPUTS_DIR)
    upload_result "${out_lbl}.pgen" "${dst_dir}"
    upload_result "${out_lbl}.pvar" "${dst_dir}"
    upload_result "${out_lbl}.psam" "${dst_dir}"
  fi
done

markers_lbl="$PGEN_OUTPUTS_DIR/markers"
if [ ! -f "${markers_lbl}.pgen" ]; then
  >&2 echo "Concatenating chromosomes"
  for f in $(ls -1 "$PGEN_OUTPUTS_DIR" | grep "\.pgen"); do
    lbl="$PGEN_OUTPUTS_DIR/${f%.pgen}"
    echo "${lbl}.pgen ${lbl}.pvar ${lbl}.psam" >> "${pmerge_list_file}"
  done
  plink2 \
    --pmerge-list "${pmerge_list_file}" \
    --make-pgen \
    --out "${markers_lbl}"
  dst_dir=$(basename $PGEN_OUTPUTS_DIR)
  upload_result "${markers_lbl}.pgen" "${dst_dir}"
  upload_result "${markers_lbl}.pvar" "${dst_dir}"
  upload_result "${markers_lbl}.psam" "${dst_dir}"
fi

marker_ids_out="$RUN_DIR/marker.ids"
if [ ! -f "${marker_ids_out}" ]; then
  >&2 echo "Exporting variant IDs"
  tmp=$(mktemp -p $TMPDIR)
  cat "${markers_lbl}.pvar" | grep -v '^#' | cut -d$'\t' -f 3 > "${tmp}"
  mv "${tmp}" "${marker_ids_out}"
  upload_result "${marker_ids_out}"
fi

sids_out="$RUN_DIR/sample.ids"
if [ ! -f "${sids_out}" ]; then
  >&2 echo "Exporting sample IDs"
  tmp=$(mktemp -p $TMPDIR)
  cat "${markers_lbl}.psam" | grep -v '^#' | awk '{ print $1"_"$2 }' > "${tmp}"
  mv "${tmp}" "${sids_out}"
  upload_result "${sids_out}"
fi

mm_out="$RUN_DIR/geno.mm"
if [ ! -f "${mm_out}" ]; then
  >&2 echo "Converting markers PGEN to Matrix Market format"
  tmp=$(mktemp -p $TMPDIR)
  $SCRIPT_DIR/pgen_to_mmT.py "${markers_lbl}" > "${tmp}"
  mv "${tmp}" "${mm_out}"
  upload_result "${mm_out}"
fi

ann_out="$RUN_DIR/markers.annotations.tsv.gz"
if [ ! -f "${ann_out}" ]; then
  >&2 echo "Annotating markers"
  tmp=$(mktemp -p $TMPDIR)
  bash -ex $SCRIPT_DIR/annotate_markers.sh "${markers_lbl}" | gzip > "${tmp}"
  mv "${tmp}" "${ann_out}"
  upload_result "${ann_out}"
fi
