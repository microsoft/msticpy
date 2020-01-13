Installing
==========


Python 3.6 or Later
-------------------

*msticpy* requires Python 3.6 or later.
If you are running in hosted environment such as Azure Notebooks,
Python is already installed. Please ensure that the Python 3.6 (or later)
kernel is selected for your notebooks.

If you are running the notebooks locally, you will need to install Python 3.6
or later. The Ananconda distribution is a good starting point since it comes
with many of packages required by *msticpy* pre-installed.

Creating a virtual environment
------------------------------

*msticpy* has a significant number of dependencies. To avoid conflicts
with packages in your existing Python environment you may want to
create a Python virtual environment
or a conda environment and install the package there.


For standard python use the ``virtualenv`` command (several alternatives
to virtualenv are available). For Conda use the conda ``env`` command.
In both cases be sure to activate the environment before running jupyter
using ``activate {my_env_name}``.

Installation
------------

Run the following command to install *msticpy*.


``pip install msticpy``

or for the latest dev build

``pip install git+https://github.com/microsoft/msticpy``