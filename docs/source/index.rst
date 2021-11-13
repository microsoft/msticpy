MSTIC Jupyter and Python Security Tools
=======================================


**msticpy** is a library for InfoSec investigation and hunting
in Jupyter Notebooks. It includes functionality to:

* query log data from multiple sources
* enrich the data with Threat Intelligence, geolocations and Azure
  resource data
* extract Indicators of Activity (IoA) from logs and unpack encoded data
* perform sophisticated analysis such as Anomalous session detection and
  time series decomposition
* visualize data using interactive timelines, process trees and
  multi-dimensional Morph Charts

It also includes some time-saving notebook tools such as widgets to
set query time boundaries, select and display items from lists, and
configure the notebook environment.

.. figure:: visualization/_static/Timeline-08.png
   :alt: Timeline with reference marker

Background - Why we created *msticpy*
-------------------------------------

The package was originally built for authoring notebooks in
`Azure Sentinel <https://azure.microsoft.com/en-us/services/azure-sentinel/>`__.
While Azure Sentinel is still a big focus of our work, we are
have extended the data query/acquisition components to pull log data from
other sources (e.g. Splunk, Microsoft 365 Defender and Microsoft Graph,
SumoLogic, and
are actively working on support for data from other SIEM platforms).
Most of the components can also be used with data from any source. Pandas
dataframes are used as the ubiquitous input and output format of almost
all components.

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

MSTIC == Microsoft Threat Intelligence Center. We are mostly security
analysts and  engineers working on:

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
   notebooksamples
   blog_articles
   Releases
   contributing
   license


Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`
