#!/usr/bin/env bash

set -e -x -o pipefail

# fancy stuff to get absolute path to this script and it's directory
SCRIPT_PATH="${BASH_SOURCE}"
while [ -L "${SCRIPT_PATH}" ]; do
    SCRIPT_DIR="$(cd -P "$(dirname "${SCRIPT_PATH}")" >/dev/null 2>&1 && pwd)"
    SCRIPT_PATH="$(readlink "${SCRIPT_PATH}")"
    [[ ${SCRIPT_PATH} != /* ]] && SCRIPT_PATH="${SCRIPT_DIR}/${SCRIPT_PATH}"
done
SCRIPT_PATH="$(readlink -f "${SCRIPT_PATH}")"
SCRIPT_DIR="$(cd -P "$(dirname -- "${SCRIPT_PATH}")" >/dev/null 2>&1 && pwd)"

# our source files should be relative to this script
cd $SCRIPT_DIR

APP_DIR=$SCRIPT_DIR/tcg_cloud_workstation
APP_RESOURCES_DIR=$APP_DIR/resources
APP_USER_DIR=$APP_RESOURCES_DIR/home/dnanexus

# make a tmpdir w/ trap that will delete on exit
TMPDIR="$(mktemp -d)"
trap 'rm -rf -- "$TMPDIR"' EXIT

# create a folder structure that mimics what we need in the cloud_workstation image
RESOURCES_TMPDIR=$TMPDIR/resources
APP_USER_TMPDIR=$RESOURCES_TMPDIR/home/dnanexus
# APP_BIN
# echo >&2 "Using tmp resources directory: $RESOURCES_TMPDIR"
mkdir -p $APP_USER_TMPDIR

# copy root nextflow files to tmp dnanexus user directory
# for root_nextflow_file in $SCRIPT_DIR/../nf/*.nf; do
#     cp -a $root_nextflow_file $APP_USER_TMPDIR/$(basename $root_nextflow_file)
# done

# copy modules directory
# cp -a $SCRIPT_DIR/nf/modules $APP_USER_TMPDIR/modules

# copy binaries
mkdir -p $APP_USER_TMPDIR/bin
cp -a $SCRIPT_DIR/bin/plink2-intel-avx2 $APP_USER_TMPDIR/bin/plink2
cp -a $SCRIPT_DIR/bin/jq-linux-amd64 $APP_USER_TMPDIR/bin/jq

# copy nvim config
mkdir -p $APP_USER_TMPDIR/.config/
cp -rL ~/.config/nvim $APP_USER_TMPDIR/.config/
rm -rf $APP_USER_TMPDIR/.config/nvim/.git*

# make a tarball of relative paths to our dnanexus user resources
# we do this so that we can list the contents and delete the byproducts after the build
# we *could* copy real files instead of using links, but since the `dx build` process
# automagically hardcopies any softlinks that point OUTSIDE of the application resources directory
# it's technically faster this way
# NOTE: it's very important for this to use "./*" so that we don't include the current directory in the tarball
# if we DID then the subsequent steps to delete artifacts would delete the entire dnanexus home directory
cd $APP_USER_TMPDIR
tar -caf $TMPDIR/app_user_resources.tar ./bin
tar --append -af $TMPDIR/app_user_resources.tar ./.config

# extract our tarball of links to the application dnanexus user directory
tar -xvf $TMPDIR/app_user_resources.tar -C $APP_USER_DIR

# use dx to build the application and capture the resulting applet ID
APPLET_ID=$(dx build -f --brief $APP_DIR | jq '.id' | sed 's/"//g')

# delete artifacts from the appplication dnanexus user directory
# we use `sort -r` to delete nested files
cd $APP_USER_DIR
tar -tf $TMPDIR/app_user_resources.tar | sort -r | xargs -d'\n' rm -rf

# echo the applet ID so we can know what to run
echo $APPLET_ID
