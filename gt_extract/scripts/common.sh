#!/usr/bin/env bash

WES_VCF_FIELD_ID=23141

RUN_ID=$1
WORKDIR=$PWD

function upload_result {
    local to_upload=$1
    local dst_dir=$2
    # echo dx upload -rp --brief --no-progress $to_upload --destination /runs/$RUN_ID/$dst_dir/
    dx upload -rp --brief --no-progress $to_upload --destination /runs/$RUN_ID/$dst_dir/ >/dev/null
}

function log_error {
    local eid=$1
    local exit_code=$2
    local logs=$3

    echo >&2 $logs
    echo $logs >>$runlog

    if [ "$exit_code" -ne "0" ]; then
        echo >&2 "ERROR DETECTED: <eid=$eid>"
        echo $eid >>$error_list
        return 1
    fi

    return 0
}
