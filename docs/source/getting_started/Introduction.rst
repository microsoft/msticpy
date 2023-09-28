Introduction
============

*msticpy* is a set of Python tools intended to be used for security
investigations and hunting.
Many of the tools originated as code Jupyter notebooks written to
solve a problem as part of a security investigation. Some
of the tools are only useful in notebooks (e.g. the widgets and
visualizations) but many others can be used from the Python commandline
or imported into your code.

The package addresses three central needs for security investigators
and hunters:

-  Acquiring and enriching data
-  Analyzing data
-  Visualizing data


Use Cases and Environments
--------------------------

Although *msticpy* was originally developed for use with Azure Sentinel,
much of the package is agnostic to the data source. Data query components
for Splunk, Microsoft 365 Defender Advanced, Microsoft Graph and others
are also included.

The tool APIs typically accept a pandas DataFrame as input and, where
appropriate, return output as a DataFrame.
