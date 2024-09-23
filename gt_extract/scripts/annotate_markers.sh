#!/usr/bin/env bash

set -e
set -x
set -o pipefail

if [ $# -eq 1 ]; then
  input_lbl=$1
  use_local_modules=0
elif [ $# -eq 2 ]; then
  input_lbl=$1
  use_local_modules=$2
else
    >&2 echo "Usage: $0 markers_lbl [use_local_modules {0|1}]"
    exit 1
fi

input_dir=$(dirname $input_lbl)

# make a tmpdir w/ trap that will delete on exit
TMPDIR="$(mktemp -d)"
trap 'rm -rf -- "$TMPDIR"' EXIT

oc_dir=$(dirname $(oc config md))
>&2 echo "[DEBUG] ${oc_dir}"

if [[ "${use_local_modules}" -eq 0 ]]; then
  if [[ ! -f "${oc_dir}/cravat-modules.tar.gz" ]]; then
    >&2 echo "Downloading and installing OpenCRAVAT modules (this may take several minutes)"
    dx download cravat-modules.tar.gz -o "${oc_dir}/"
  fi
  if [[ ! -f "${oc_dir}/modules.installed" ]]; then
    tar -xvf "${oc_dir}/cravat-modules.tar.gz" -C "${oc_dir}"
    >&2 echo "OpenCRAVAT modules installed. Updating..."
    oc module update -y
    touch "${oc_dir}/modules.installed"
  fi
fi

VCF_PVAR_LBL="$TMPDIR/markers"
MARKERS_VCF="$VCF_PVAR_LBL.vcf"
>&2 plink2 \
  --pfile "${input_lbl}" \
  --make-just-pvar cols=vcfheader,qual,filter,info \
  --out "$VCF_PVAR_LBL"

mv "$VCF_PVAR_LBL.pvar" "$MARKERS_VCF"

cd $TMPDIR
>&2 echo "Annotating $input_file..."
>&2 oc run "$MARKERS_VCF" \
  --cleanrun \
  -l hg38 \
  -a $(ls "${oc_dir}/modules/annotators/") \
  -n annotations \
  -t tsv

sed -i '/^#/d' annotations.variant.tsv
cat annotations.variant.tsv

cd ~-
