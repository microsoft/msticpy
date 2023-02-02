Jupyter, msticpy and Microsoft Sentinel
=======================================

You can run notebooks from Microsoft Sentinel in the
Azure Machine Learning studio hosted environment.

If you have a local installation of Python 3.8 or later, you can also
download the notebooks and run these locally. Our recommendation
is to use the `Anaconda <https://www.anaconda.com/distribution/>`__
distribution since it contains the Jupyter packages and many others
needed for the Microsoft Sentinel notebooks.

Running notebooks in Azure Machine Learning (AML)
-------------------------------------------------

Please follow the guidance given in this document to get started
`Tutorial: Get started with Jupyter notebooks and MSTICPy in Microsoft Sentinel
<https://docs.microsoft.com/azure/sentinel/notebook-get-started>`__.


Running notebooks locally
-------------------------

The easiest way of getting to the Microsoft Sentinel template
and sample notebooks is to use Git to clone the `Azure-Sentinel-Notebooks
repository <https://github.com/Azure/Azure-Sentinel-Notebooks>`__

.. code:: bash

   cd <root-path-to-create-repo-folder>
   git clone https://github.com/Azure/Azure-Sentinel-Notebooks.git

You can also download the repo as a ZIP file and extract the contents
into a folder of your choice.

View the notebooks on GitHub or in NBViewer
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can also view the notebooks without executing them natively
in GitHub just by clicking on them. You can also use **nbviewer**.
Copy the URL from a notebook in the repo, then go to
`nbviewer.org <https://nbviewer.org>`__ and paste the notebook URL
into the text box.


Notebook Setup
--------------

When it comes to running one of the notebooks in against real data, you
will need some preparatory steps.

Prerequisites
~~~~~~~~~~~~~

Permissions - in order to read any data, you will need to have at least LogAnalytics
Reader role for your account.

Configuring your Python Environment for the First Time
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You will need to carry out this procedure every time you start working
in a fresh Python environment.

1. Ensure that you have a version of Python 3.8 or later.

2. Install msticpy.

For more details see :doc:`Installing <../getting_started/Installing>`

Creating a ``msticpyconfig.yaml`` configuration file
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To use Microsoft Sentinel you need at least to configure the Sentinel
Workspace details in this file.

See the section `Authenticating to MS Sentinel`_ below.
This is covered in more detail in:

- :doc:`MSTICPy Config <../getting_started/SettingsEditor>`
- :doc:`Settings Editor <../getting_started/msticpyconfig>`


Querying Data
-------------

Import and initialize MSTICPy
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: python

   import msticpy as mp
   mp.init_notebook()


Creating a Query Provider
~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: python

   qry_prov = mp.QueryProvider("MSSentinel")

A range of built-in queries come with the Sentinel query provider.
You can view these with the ``list_queries`` function or interactively
browse the queries and help with the query browser.

.. code:: python

   qry_prov.list_queries()

   # or
   qry_prov.browse()


Authenticating to MS Sentinel
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This assumes that you have configured at least one Sentinel Workspace
in your ``msticpyconfig.yaml``. The contents should look something
like this.

.. code:: yaml

   AzureSentinel:
      Workspaces:
         Default:
            WorkspaceId: "52b1ab41-869e-4138-9e40-2a4457f09bf3"
            TenantId: "72f988bf-86f1-41af-91ab-2d7cd011db49"
            SubscriptionId: "cd928da3-dcde-42a3-aad7-d2a1268c2f48"
            ResourceGroup: MyResourceGroup
            WorkspaceName: Workspace1

At minimum you must have WorkspaceId and TenantId configured.

You can authenticate to Sentinel using the query provider ``connect``
function.

.. code:: python

   qry_prov.connect(mp.WorkspaceConfig())

No parameters are needed for WorkspaceConfig if you have a Workspace
entry in your msticpyconfig name "Default". If you do not, use
the workspace parameter to pick a named workspace.

.. code:: yaml

   AzureSentinel:
      Workspaces:
         MyHuntingWorkspace:
            WorkspaceId: "52b1ab41-869e-4138-9e40-2a4457f09bf3"
            TenantId: "72f988bf-86f1-41af-91ab-2d7cd011db49"
            ...

.. code:: python

   qry_prov.connect(mp.WorkspaceConfig(workspace="MyHuntingWorkspace"))

.. note:: From version 2.0 you can also use a shortcut parameter to
   connect to specify the workspace directly.
   ``qry_prov.connect(workspace="MyHuntingWorkspace")``
   to use the Default workspace, use "Default" as the workspace name.


Query Help
~~~~~~~~~~

Most queries require additional parameters (you can check which
parameters are needed by using the help in the query browser or
calling the query with a "?" parameter).

.. code:: python

   qry_prov.Linux.list_logons("?")

Time range
~~~~~~~~~~

The queries use a built-in time range as their default time boundary.
You can change this by opening and modifying the ``query_time`` country_flag_emoji

.. code:: python

   qry_prov.query_time

Run a query
~~~~~~~~~~~

.. code:: python

   results_df = qry_prov.WindowsSecurity.list_host_logons(host_name="MyHost")
   results_df.head()

This will run the query with start and end times defined by the settings in
``qry_prov.query_time``

Run an ad hoc query
~~~~~~~~~~~~~~~~~~~

Use the ``exec_query`` function to run arbitrary KQL queries.

.. note:: The ``query_time`` settings have no impact on ad hoc queries.
   You must supply required ``where`` clauses to restrict the time
   range for your query.

.. code:: python

   results_df = qry_prov.exec_query("SecurityAlert | where TimeGenerated > ago(1day)")
   results_df.head()


Example Notebooks
-----------------


-  Microsoft Sentinel Jupyter notebooks can be found
   `here <https://github.com/Azure/Azure-Sentinel-Notebooks>`__ on GitHub.

Some examples:

- `Getting started <https://github.com/Azure/Azure-Sentinel-Notebooks/blob/master/A%20Getting%20Started%20Guide%20For%20Azure%20Sentinel%20ML%20Notebooks.ipynb>`__
- `MSTICPy CyberSec Features tour <https://github.com/Azure/Azure-Sentinel-Notebooks/blob/master/A%20Tour%20of%20Cybersec%20notebook%20features.ipynb>`__
- `Account Explorer <https://github.com/Azure/Azure-Sentinel-Notebooks/blob/master/Entity%20Explorer%20-%20Account.ipynb>`__
- `Domain and URL Explorer <https://github.com/Azure/Azure-Sentinel-Notebooks/blob/master/Entity%20Explorer%20-%20Domain%20and%20URL.ipynb>`__
- `IP Explorer <https://github.com/Azure/Azure-Sentinel-Notebooks/blob/master/Entity%20Explorer%20-%20IP%20Address.ipynb>`__
- `Linux Host Explorer <https://github.com/Azure/Azure-Sentinel-Notebooks/blob/master/Entity%20Explorer%20-%20Linux%20Host.ipynb>`__
- `Windows Host Explorer <https://github.com/Azure/Azure-Sentinel-Notebooks/blob/master/Entity%20Explorer%20-%20Windows%20Host.ipynb>`__

Other sample notebooks with saved data are in the `Sample-Notebooks <https://github.com/Azure/Azure-Sentinel-Notebooks/tree/master/tutorials-and-examples>`__
folder.