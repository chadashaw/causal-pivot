source ~/environment
source ~/.bashrc

DIR=$(pwd)

export DX_RUN_DETACH=1

if [[ -z $BYOBU_BACKEND ]]; then
    if [[ -f /run/shm/dx_job_monitor_started ]]; then
        byobu
    else
        touch /run/shm/dx_job_monitor_started
        byobu new-session -n "${DX_JOB_ID:-unknown_dx_job}" "tail -n +1 -f -q dx_stdout dx_stderr" \; new-window -n DNAnexus 'bash --login'
    fi
else
    /etc/update-motd.d/dx-motd
fi

eval "$(register-python-argcomplete dx | sed 's/-o default//')"

if [[ -z $DX_SNAPSHOT_FILE ]]; then
    DX_SNAPSHOT_FILE=$(cat job_input.json | jq -r '.snapshot."$dnanexus_link"')
    if [[ $DX_SNAPSHOT_FILE != null ]]; then
        dx-check-snapshot-version $DX_SNAPSHOT_FILE 1>/dev/null || true
    fi
    export DX_SNAPSHOT_FILE
fi

# some silly hack see: https://documentation.dnanexus.com/developer/cloud-workstation
unset DX_WORKSPACE_ID
dx cd $DX_PROJECT_CONTEXT_ID:
DX_PROJECT_CONTEXT_NAME=$(dx find projects | grep $DX_PROJECT_CONTEXT_ID | cut -d' ' -f3)

# we use this hacky `.setupcomplete` file to indicate that these setup steps have already been run
if [ ! -f "/home/$USER/.setupcomplete" ]; then
    # make local `bin` directory
    if [ ! -d /home/$USER/bin ]; then
        mkdir -p /home/$USER/bin
    fi

    sudo apt-get update
    sudo apt-get install openjdk-17-jre-headless -y --fix-missing

    # install nextflow if not installed
    if [[ -z $(command -v nextflow) ]]; then
        TMPDIR="$(mktemp -d)"
        trap 'rm -rf -- "$TMPDIR"' EXIT
        cd $TMPDIR
        wget -qO- https://get.nextflow.io | bash
        cp -a nextflow /home/$USER/bin
        chmod +x /home/$USER/bin/nextflow
        cd $DIR
    fi

    # download our analysis scripts from our dx project
    # dx download -rf /scripts -o /home/$USER/
    # chmod u+x /home/$USER/scripts/*

    # create hacky signal file
    echo 1 >/home/$USER/.setupcomplete
fi
