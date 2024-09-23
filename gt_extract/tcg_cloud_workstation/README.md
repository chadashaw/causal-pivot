# TCG Cloud Workstation Applet

## Purpose of Applet

This applet, which is a modified version of the
[Cloud Workstation](https://documentation.dnanexus.com/developer/cloud-workstation)
app supplied by dnanexus. It creates a temporary Ubuntu linux environment with SSH access
and pre-installed binaries for working with the DNAnexus platform

- see the [original README](./original.Readme.md).

This applet is primarly used to run our exome pre-processing workflow

- see [the main script](../dx-applets/analysis_scripts/run.sh).

To run the workflow:

- start and SSH into the workstation
- download the appropriate bedfile
- choose a cohort
- execute `~/scripts/run.sh name_of_cohort path/to/regions.bed`

This applet can also be used to perform exploratory analysis on Bulk data and other files in the project
without having to download those files off the platform. This saves egress fees, but also provides
a more powerful and stable platform if powerful local resources aren't available.

## Modifications

The [dx app config][./dxapp.json] file was modified to:

- beef up the workstation instance
- add some additional pre-installed binaries (most importantly bcftools)
- change the name of the applet

The [DNAnexus Bulk Download Assistant][./resources/usr/bin/dxda] binary was downloaded from
[the GitHub repo](https://github.com/dnanexus/dxda) to improve bulk file download speeds.

The [.bash_profile](./resources/home/dnanexus/.bash_profile) was modified to:

- setup the `dx` project context
- setup `python3` and `pip`
- install `nextflow`
- install `Open CRAVAT`
- download scripts from the RAP project

## Starting

We wrote a convent [startup](./start_job.sh) script to start and SSH into a cloud workstation:

Usage:

```bash
start_job.sh job_name max_session_length [optional VPN quickfix flag 0 (disabled) or 1(enabled)]
```

The `job_name` parameter will be used as the name of the DNAnexus job and can be used to monitor
the execution status of the workstation. The `max_session_length` parameter can be used to set a
maximum lifetime of the job in human readable format. The optional flag at the end, can be used to
specify whether the script should add a 24 bit CIDR mask on your public IP as the allowable addresses
that can SSH into the instances. This is to help from VPN connections that switch IP addresses frequently
to obscure connections.

See the [original README][./original.Readme.md] for more info.

## Adding the Open CRAVAT annotator modules tar to DNAnexus

The analysis scripts downloaded by the tcg_cloud_workstation depend on a tarball of Open CRAVAT annotation modules
in the root of the project directory. Use the following command to archive and compress your local
annotation modules (**note:** in this example my cravat module directory is `~/.local/cravat`):

`tar -chavf cravat-modules.tar.gz ./modules -C ~/.local/cravat`

You can view and change your Open CRAVAT module directory by using the `oc config md` command.

Then to upload to the project just run:

`dx upload cravat-modules.tar.gz`

If you already have an existing `cravat-modules.tar.gz` in the root project directory,
you'll need to first delete it using the UI or move it using:

`dx mv /cravat-modules.tar.gz /trash`
