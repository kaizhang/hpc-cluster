Build images
------------

To execute the entire build workflow, run:

```
./buildClient run --config recipe/wangcluster_config.yaml
```

The steps of workflow can be visualized by:

```
./buildClient view workflow.html
```

To build/rebuild a specific image, run:

```
./buildClient delete Build_compute
./buildClient run --config recipe/wangcluster_config.yaml --select Build_compute
```

Add new machines
----------------

First add the machine specifications to the `machines.txt` file. The file is TAB-delimited.

To add machines, run `addMachines --machines machines.txt`.
This will add new machines to the server but will not overwrite the existing ones.

To overwrite the existing machines, use `addMachines --machines machines.txt --overwrite`.

You can also add specific machines using the `--include` argument.

The machine information will also be appended to the file `/etc/slurm/slurm.conf`. But you need to manually assign the computers to different partitions and delete redundant records.

How to perform update
---------------------

1. First update the frontend and reboot: `yum -y update && reboot`.

2. Rebuild the bootstraping kernel: ``wwbootstrap `uname -r` ``.

3. Rebuild the images

4. Readd the machines using `--overwrite`.

5. Restart the compute nodes.

Configuration files
-------------------

* /etc/group
* /etc/passwd
* /etc/shadow
* /etc/auto.home
* /etc/auto.master
* /etc/auto.share
* /etc/autofs.conf
* /etc/slurm/slurm.conf
