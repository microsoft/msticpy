MSTIC Jupyter and Python Security Tools
=======================================

The **msticpy** package was developed to support Jupyter
Notebook authoring for `Azure Sentinel <https://azure.microsoft.com/en-us/services/azure-sentinel/>`__.
Many of the components can also be used independently of Azure Sentinel
when using Jupyter for threat hunting and security investigation.

Most of the content of the package began life as inline code in a
notebook. However, having a lot of code in your notebooks creates
a few problems:

* notebooks dominated by big code blocks make it difficult to see
  the results and text of your notebook - which is really what you
  want your audience to see
* it makes the notebook especially intimidating for non-programmers
* it makes code re-use very hard.

The philosophy behind this package is to create a repository for
re-usable pieces of functionality that make it both quicker to author
notebooks, and make resultant notebooks easier to read.

What is/who are MSTIC?
^^^^^^^^^^^^^^^^^^^^^^

MSTIC stands for the Microsoft Threat Intelligence Center - mainly
staffed by security engineers and analysts working on:

* authoring security detections for several Microsoft platforms
* threat identification and investigation

Contents
========

.. toctree::
   :maxdepth: 2

   GettingStarted
   DataAcquisition
   DataEnrichment
   DataAnalysis
   Visualization
   msticpyAPI
   notebooksamples.rst
   contributing.rst
   license.rst


Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`
