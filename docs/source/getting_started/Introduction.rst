Introduction
============

*msticpy* is a set of Python tools intended to be used for security
investigations and hunting.
Many of the tools originated as code Jupyter notebooks written to
solve a problem as part of a security investigation. Some
of tools them are only useful in notebooks (e.g. much of the nbtools
sub-package) but many others can be used from any Python program.

The package addresses three central needs for security investigators
and hunters:

-  Acquiring and enriching data
-  Analyzing data
-  Visualizing data

It is organized into three main sub-packages:

-  **sectools** - Python security tools to help with data enrichment,
   analysis or investigation.
-  **nbtools** - Jupyter-specific UI tools such as widgets, plotting and
   other data display.
-  **data** - data layer and pre-defined queries for Azure Sentinel, MDATP and
   other data sources.

Use Cases and Environments
--------------------------

Although *msticpy* was originally developed for use with Azure Sentinel,
much of the package is agnostic to the data source. Data query components
are also supplied for Microsoft Defender Advanced Threat Protection and
Microsoft Security Graph.

The tool APIs typically accept a pandas DataFrame as input and, where
appropriate, return output as a DataFrame.
