#!/usr/bin/env bash

set -e -x -o pipefail

input_file=$1

sed -i 's/\\//g' $input_file
sed -i 's/\[//g' $input_file
sed -i 's/\]//g' $input_file
sed -i 's/Omega/omega/g' $input_file
sed -i 's/Alpha/alpha/g' $input_file
sed -i 's/Beta/beta/g' $input_file
sed -i 's/Gamma/gamma/g' $input_file
sed -i 's/Eta/eta/g' $input_file
sed -i 's/Power/raise.power/g' $input_file
sed -i 's/E/exp(1)/g' $input_file

