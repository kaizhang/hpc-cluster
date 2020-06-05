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
