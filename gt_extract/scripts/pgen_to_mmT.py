#!/usr/bin/env python3

"""
Converts plink2 .pgen to transposed Matrix Media format in coordinate form.
Coordinates are encoded as (sample_index, variant_index, value).
But PGEN is variant-major, so the coordinates are printed "out of order".
MM doesn't care about the order of values though, so we good.
"""

import sys
import tempfile
import numpy as np
from tqdm import tqdm
from pgenlib import PgenReader, PvarReader

input_lbl = sys.argv[1]


def yield_gts(pvar, pgen, nsamp, nvars):
    """Iterate over all variants and yield genotype integer arrays."""
    for i in range(nvars):
        geno_int_out = np.empty(nsamp, np.int8)
        pgen.read(i, geno_int_out)
        yield geno_int_out


def parse(input_lbl, fout):
    """Read plink2 PGEN and PVAR files and write coordinates to fout.
    Returns sample count, variant count and number non-zero vals.
    """
    pgen_path = f"{input_lbl}.pgen"
    pvar_path = f"{input_lbl}.pvar"

    nvals = 0

    with PvarReader(pvar_path.encode('utf-8')) as pvar:
        with PgenReader(pgen_path.encode('utf-8')) as pgen:
            nsamp = pgen.get_raw_sample_ct()
            nvars = pvar.get_variant_ct()
            print(nsamp, nvars, file=sys.stderr)
            with tqdm(total=nvars, file=sys.stderr) as progress_bar:
                for j, geno_int in enumerate(
                    yield_gts(pvar, pgen, nsamp, nvars)
                ):
                    for i in np.where((geno_int > 0))[0]:
                        nvals += 1
                        geno = geno_int[i]
                        fout.write(' '.join([str(i+1), str(j+1), str(geno)]))
                        fout.write('\n')
                    progress_bar.update(1)
    fout.flush()
    return nsamp, nvars, nvals


with tempfile.NamedTemporaryFile(delete=False) as temp_file:
    """First pass over file writes coordinates to tmpfile and calcs metadata"""
    temp_path = temp_file.name
    print(f"Parsing non-zero allele counts from {input_lbl} -> {temp_path}",
          file=sys.stderr)
    with open(temp_path, 'w+') as fout:
        nsamp, nvars, nvals = parse(input_lbl, fout)

    """Second pass prints MM transposed format to stdout"""
    print("Printing Matrix Market file", file=sys.stderr)
    print('%%MatrixMarket matrix coordinate integer general')
    print(' '.join([str(nsamp), str(nvars), str(nvals)]))
    with tqdm(total=nvals, file=sys.stderr) as progress_bar:
        with open(temp_path, 'r') as fin:
            for i, line in enumerate(fin):
                print(line, end='')
                progress_bar.update(1)
