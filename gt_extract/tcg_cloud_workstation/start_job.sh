#!/usr/bin/env bash

if [ "$#" -lt 3 ]; then
    echo "Usage: $0 job_name max_session_length [allow_ssh_cidr_24 (default: 0)]"
    exit 1
fi

name=$1
max_session_length=$2
allow_ssh_cidr_24=$3

cidr_24=""
if [ "$allow_ssh_cidr_24" -gt 0 ]; then
    my_ip=$(curl 2>/dev/null https://api.ipify.org\?format\=json | jq -r '.ip')
    cidr_24="${my_ip%.*}.0/24"
fi

cmd_str="dx run tcg_cloud_workstation -y --ssh --tag tcg_cloud_workstation --name $name -imax_session_length=$max_session_length"
if [ ! -z "$cidr_24" ]; then
    cmd_str="${cmd_str} --allow-ssh $cidr_24"
fi

eval $cmd_str
