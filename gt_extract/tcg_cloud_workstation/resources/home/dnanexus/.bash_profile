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

export PATH=$PATH:/home/$USER/bin
export PATH=$PATH:/home/$USER/.local/bin # just to avoid pip3 path warnings

# we use this hacky `.setupcomplete` file to indicate that these setup steps have already been run
if [ ! -f "/home/$USER/.setupcomplete" ]; then
    # make local `bin` directory
    if [ ! -d /home/$USER/bin ]; then
        mkdir -p /home/$USER/bin
    fi

    # install python3.9
    # Note: this is due to what seems like a bug in open-cravat in pre-3.9 python version

    sudo apt-get update && sudo apt-get install python3.9 -y

    # Add and update Python 3.9 as python/python3 alternatives if not already added
    sudo update-alternatives --install /usr/bin/python python $(which python3.9) 100
    sudo update-alternatives --set python /usr/bin/python3.9

    sudo update-alternatives --install /usr/bin/python3 python3 $(which python3.9) 100
    sudo update-alternatives --set python3 /usr/bin/python3.9

    # update `pip` and install pip dependencies
    python -m pip install --upgrade pip
    python -m pip install dxpy names_generator numpy tqdm pgenlib

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

    # install Open CRAVAT if not installed
    if [[ -z $(command -v oc) ]]; then
        python -m pip install open-cravat
        mkdir -p /home/$USER/cravat/modules
        oc config md /home/$USER/cravat/modules
        oc module install-base
        # dx download -o /home/$USER/ cravat-modules.tar.gz
        # tar -xvf cravat-modules.tar.gz -C /home/$USER/cravat/
    fi

    # download our analysis scripts from our dx project
    dx download -rf /scripts -o /home/$USER/
    chmod u+x /home/$USER/scripts/*

    # create hacky signal file
    echo 1 >/home/$USER/.setupcomplete
fi
