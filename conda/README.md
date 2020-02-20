# Anaconda Environment Setup

### Caveat
This is a preliminary set of requirements files for *Conda*
environments. Since some of the packages used by *msticpy*
are not available as Conda recipes, we have divided the package
requirements into conda-installable and pip-installable
requirements files.

Using pip installs into a Conda environment will invalidate
some of Conda's dependency tracking so *please* do this in
a dedicated Conda environment - as shown below.


## Installation Steps
Packages available from Conda or Conda forge are installable
using the `conda/conda-reqs.txt`. No version constraints are
included in these package specs. Conda will automatically install
the latest compatible versions of these packages. If you have
an up-to-date version of Anaconda these versions should be
compatible with *msticpy*.

### Create your Conda environment
Python 3.7.6 was the latest Python 3.7 release at the time of
writing. Some wheels are not yet available for Windows for
Python 3.8, so you may have problems installing some of the
*msticpy* dependencies on Windows. *msticpy* should, however,
work fine with Python 3.8.

```shell
conda create --name my_env_name python=3.7.6
conda activate my_env_name
```

## Append the Conda-Forge channel
Some of the Conda packages are only available from
Conda-Forge.

```shell
conda config --add channels conda-forge
```

## Install the Conda packages

```shell
conda install --file {path}/conda-reqs.txt
```

## Install pip packages

```shell
conda install pip
pip install -r {path}/conda-reqs-pip.txt
```

## Install dev packages (optional)

Installation of these packages is only needed if you are doing
development work on msticpy.

```shell
conda install --file {path}/conda-reqs-dev.txt
pip install -r {path}/conda-reqs-dev-pip.txt
```