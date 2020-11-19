# MSTIC Jupyter and Python Security Tools

Microsoft Threat Intelligence Python Security Tools.

**msticpy** is a library for InfoSec investigation and hunting
in Jupyter Notebooks. It includes functionality to:
- query log data from multiple sources
- enrich the data with Threat Intelligence, geolocations and Azure
  resource data
- extract Indicators of Activity (IoA) from logs and unpack encoded data
- perform sophisticated analysis such as anomalous session detection and
  time series decomposition
- visualize data using interactive timelines, process trees and
  multi-dimensional Morph Charts

It also includes some time-saving notebook tools such as widgets to
set query time boundaries, select and display items from lists, and
configure the notebook environment.

<img src="https://github.com/microsoft/msticpy/blob/master/docs/source/visualization/_static/Timeline-08.png"
alt="Timeline" title="Msticpy Timeline Control" height="300" />

The **msticpy** package was initially developed to support
[Jupyter Notebooks](https://jupyter-notebook-beginner-guide.readthedocs.io/en/latest/)
authoring for
[Azure Sentinel](https://azure.microsoft.com/en-us/services/azure-sentinel/).
While Azure Sentinel is still a big focus of our work, we are
extending the data query/acquisition components to pull log data from
other sources (currently Microsoft Defender and Microsoft Graph but we
are actively working on support for data from other SIEM platforms).
Most of the components can also be used with data from any source. Pandas
DataFrames are used as the ubiquitous input and output format of almost
all components.

The package addresses three central needs for security investigators
and hunters:

-  Acquiring and enriching data
-  Analyzing data
-  Visualizing data

We welcome feedback, bug reports, suggestions for new features and contributions.

## Installing

`pip install msticpy`

or for the latest dev build

`pip install git+https://github.com/microsoft/msticpy`

## Documentation

Full documentation is at [ReadTheDocs](https://msticpy.readthedocs.io/en/latest/)

Sample notebooks for many of the modules are in the
[docs/notebooks](https://github.com/microsoft/msticpy/blob/master/docs/notebooks)
folder and accompanying notebooks.

You can also browse through the sample notebooks referenced at the end of this document
to see some of the functionality used in context. You can play with some of the package
functions in this interactive demo on mybinder.org.

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/Azure/Azure-Sentinel-Notebooks/master?filepath=%2Fnbdemo%2Fmsticpy%20demo.ipynb)

---

## Log Data Acquisition

- QueryProvider - extensible query library targeting Azure Sentinel, OData
  sources and other. Built-in parameterized queries allow complex queries to be run
  from a single function call. Add your own queries using a simple YAML
  schema.
- security_alert and security_event - encapsulation classes for alerts and events.
- entity_schema - definitions for multiple entities (Host, Account, File, IPAddress,
  etc.)

[Data Queries Notebook](https://github.com/microsoft/msticpy/blob/master/docs/notebooks/Data_Queries.ipynb)

## Data Enrichment

### tiproviders

The TILookup class can lookup IoCs across multiple TI providers. built-in
providers include AlienVault OTX, IBM XForce, VirusTotal and Azure Sentinel.

The input can be a single IoC observable or a pandas DataFrame containing
multiple observables. Depending on the provider, you may require an account
and an API key. Some providers also enforce throttling (especially for free
tiers), which might affect performing bulk lookups.

[TIProviders](https://msticpy.readthedocs.io/en/latest/data_acquisition/TIProviders.html)
and
[TILookup Usage Notebook](https://github.com/microsoft/msticpy/blob/master/docs/notebooks/TIProviders.ipynb)

### GeoLocation Data
The GeoIP lookup classes allow you to match the geo-locations of IP addresses
using either:
- GeoLiteLookup - Maxmind Geolite (see <https://www.maxmind.com>)
- IPStackLookup  - IPStack (see <https://ipstack.com>)

<img src="https://github.com/microsoft/msticpy/blob/master/docs/source/visualization/_static/FoliumMap-01.png"
  alt="Folium map"
  title="Plotting Geo IP Location" height="200" />

[GeoIP Lookup](https://msticpy.readthedocs.io/en/latest/data_acquisition/GeoIPLookups.html)
and
[GeoIP Notebook](https://github.com/microsoft/msticpy/blob/master/docs/notebooks/GeoIPLookups.ipynb)

### Azure Data
This package contains functionality for enriching data regarding Azure host
details with additional host details exposed via the Azure API.

[Azure Data](https://msticpy.readthedocs.io/en/latest/data_acquisition/AzureData.html)


## Security Analysis

This subpackage contains several modules helpful for working on security investigations and hunting:

### Anomalous Sequence Detection

Detect unusual sequences of events in your Office, Active Directory or other log data.
You can extract sessions (e.g. activity initiated by the same account) and identify and
visualize unusual sequences of activity. For example, detecting an attacker setting
a mail forwarding rule on someone's mailbox.

[Anomalous Sessions](https://msticpy.readthedocs.io/en/latest/data_analysis/AnomalousSequence.html)
and
[Anomalous Sequence Notebook](https://github.com/microsoft/msticpy/blob/master/docs/notebooks/AnomalousSequence.ipynb)

### Time Series

Time series analysis allows you to identify unusual patterns in your log data
taking into account normal seasonal variations (e.g. the regular ebb and flow of
events over hours of the day, days of the week, etc.). Using both analysis and
visualization highlights unusual traffic flows or event activity for any data
set.

<img src="https://github.com/microsoft/msticpy/blob/master/docs/source/visualization/_static/TimeSeriesAnomalieswithRangeTool.png"
alt="Time Series anomalies" title="Time Series anomalies" height="300" />

[Time Series](https://msticpy.readthedocs.io/en/latest/visualization/TimeSeriesAnomalies.html)


### base64unpack

Base64 and archive (gz, zip, tar) extractor. It will try to identify any base64 encoded
strings and try decode them. If the result looks like one of the supported archive types it
will unpack the contents. The results of each decode/unpack are rechecked for further
base64 content and up to a specified depth.

[Base64 Decoding](https://msticpy.readthedocs.io/en/latest/data_analysis/Base64Unpack.html)
[Base64Unpack Notebook](https://github.com/microsoft/msticpy/blob/master/docs/notebooks/Base64Unpack.ipynb)

### iocextract

Uses regular expressions to look for Indicator of Compromise (IoC) patterns - IP Addresses, URLs,
DNS domains, Hashes, file paths.
Input can be a single string or a pandas dataframe.

[IoC Extraction](https://msticpy.readthedocs.io/en/latest/data_analysis/IoCExtract.html)
[IoCExtract Notebook](https://github.com/microsoft/msticpy/blob/master/docs/notebooks/IoCExtract.ipynb)

### eventcluster (experimental)

This module is intended to be used to summarize large numbers of
events into clusters of different patterns. High volume repeating
events can often make it difficult to see unique and interesting items.

<img src="https://github.com/microsoft/msticpy/blob/master/docs/source/data_analysis/_static/EventClustering_2a.png"
  alt="Clustering"
  title="Clustering based on command-line variability" height="400" />

This is an unsupervised learning module implemented using SciKit Learn DBScan.

[Event Clustering](https://msticpy.readthedocs.io/en/latest/data_analysis/EventClustering.html)
[Event Clustering Notebook](https://github.com/microsoft/msticpy/blob/master/docs/notebooks/EventClustering.ipynb)

## Visualization

### Timelines

Display any log events on an interactive timeline. Using the
[Bokeh Visualization Library](https://bokeh.org/) the timeline control enables
you to visualize one or more event streams, interactively zoom into specific time
slots and view event details for plotted events.

<img src="https://github.com/microsoft/msticpy/blob/master/docs/source/visualization/_static/TimeLine-01.png"
alt="Timeline" title="Msticpy Timeline Control" height="300" />

[Timeline](https://msticpy.readthedocs.io/en/latest/visualization/EventTimeline.html)
[Timeline Notebook](https://github.com/microsoft/msticpy/blob/master/docs/notebooks/EventTimeline.ipynb)

### Process Trees

The process tree functionality has two main components:

- Process Tree creation - taking a process creation log from a host and building
  the parent-child relationships between processes in the data set.
- Process Tree visualization - this takes the processed output displays an interactive process tree using Bokeh plots.

There are a set of utility functions to extract individual and partial trees from the processed data set.

<img src="https://github.com/microsoft/msticpy/blob/master/docs/source/visualization/_static/process_tree3.png"
alt="Process Tree"
title="Interactive Process Tree" height="400" />

[Process Tree](https://msticpy.readthedocs.io/en/latest/visualization/ProcessTree.html)
[Process Tree Notebook](https://github.com/microsoft/msticpy/blob/master/docs/notebooks/ProcessTree.ipynb)

## Other Tools

### auditdextract

Module to load and decode Linux audit logs. It collapses messages sharing the same
message ID into single events, decodes hex-encoded data fields and performs some
event-specific formatting and normalization (e.g. for process start events it will
re-assemble the process command line arguments into a single string).

This is still a work-in-progress.

### syslog_utils

Module to support an investigation of a Linux host with only syslog logging enabled.
This includes functions for collating host data, clustering logon events and detecting
user sessions containing suspicious activity.

### cmd_line

A module to support he detection of known malicious command line activity or suspicious
patterns of command line activity.

### Notebook widgets

These are built from the [Jupyter ipywidgets](https://ipywidgets.readthedocs.io/) collection
and group common functionality useful in InfoSec tasks such as list pickers,
query time boundary settings and event display into an easy-to-use format.

<img src="https://github.com/microsoft/msticpy/blob/master/docs/source/visualization/_static/Widgets1.png"
  alt="Time span Widget"
  title="Query time setter" height="100" />


<img src="https://github.com/microsoft/msticpy/blob/master/docs/source/visualization/_static/Widgets4.png"
  alt="Alert browser"
  title="Alert browser" height="300" />


---

## Clone the notebooks in this repo to Azure Notebooks

Requires sign-in to Azure Notebooks
<a href="https://notebooks.azure.com/import/gh/Microsoft/msticpy">
<img src="https://notebooks.azure.com/launch.png" />
</a>

## More Notebooks

- [Account Explorer](https://github.com/Azure/Azure-Sentinel-Notebooks/blob/master/Entity%20Explorer%20-%20Account.ipynb)
- [Domain and URL Explorer](https://github.com/Azure/Azure-Sentinel-Notebooks/blob/master/Entity%20Explorer%20-%20Domain%20and%20URL.ipynb)
- [IP Explorer](https://github.com/Azure/Azure-Sentinel-Notebooks/blob/master/Entity%20Explorer%20-%20IP%20Address.ipynb)
- [Linux Host Explorer](https://github.com/Azure/Azure-Sentinel-Notebooks/blob/master/Entity%20Explorer%20-%20Linux%20Host.ipynb)
- [Windows Host Explorer](https://github.com/Azure/Azure-Sentinel-Notebooks/blob/master/Entity%20Explorer%20-%20Windows%20Host.ipynb)

View directly on GitHub or copy and paste the link into [nbviewer.org](https://nbviewer.jupyter.org/)

## Notebook examples with saved data

See the following notebooks for more examples of the use of this package in practice:

- Windows Alert Investigation in
  [github](https://github.com/Azure/Azure-Sentinel-Notebooks/blob/master/Sample-Notebooks/Example%20-%20Guided%20Investigation%20-%20Process-Alerts.ipynb)
  or
  [NbViewer](https://nbviewer.jupyter.org/github/Azure/Azure-Sentinel-Notebooks/blob/master/Sample-Notebooks/Example%20-%20Guided%20Investigation%20-%20Process-Alerts.ipynb)
- Office 365 Exploration in
  [github](https://github.com/Azure/Azure-Sentinel-Notebooks/blob/master/Sample-Notebooks/Example%20-%20Guided%20Hunting%20-%20Office365-Exploring.ipynb)
  or [NbViewer](https://nbviewer.jupyter.org/github/Azure/Azure-Sentinel-Notebooks/blob/master/Sample-Notebooks/Example%20-%20Guided%20Hunting%20-%20Office365-Exploring.ipynb)
- Cross-Network Hunting in
  [github](https://github.com/Azure/Azure-Sentinel-Notebooks/blob/master/Sample-Notebooks/Example%20-%20Step-by-Step%20Linux-Windows-Office%20Investigation.ipynb)or
  [NbViewer](https://nbviewer.jupyter.org/github/Azure/Azure-Sentinel-Notebooks/blob/master/Sample-Notebooks/Example%20-%20Step-by-Step%20Linux-Windows-Office%20Investigation.ipynb)


## Supported Platforms and Packages

- msticpy is OS-independent
- Requires [Python 3.6 or later](https://www.python.org/dev/peps/pep-0494/)
- See [requirements.txt](requirements.txt) for more details and version requirements.

---

## Contributing

For (brief) developer guidelines, see this wiki article
[Contributor Guidelines](https://github.com/microsoft/msticpy/wiki/Contributor-guidelines)

This project welcomes contributions and suggestions.  Most contributions require you to agree to a
Contributor License Agreement (CLA) declaring that you have the right to, and actually do, grant us
the rights to use your contribution. For details, visit <https://cla.microsoft.com>.

When you submit a pull request, a CLA-bot will automatically determine whether you need to provide
a CLA and decorate the PR appropriately (e.g., label, comment). Simply follow the instructions
provided by the bot. You will only need to do this once across all repos using our CLA.

This project has adopted the [Microsoft Open Source Code of Conduct](https://opensource.microsoft.com/codeofconduct/).
For more information see the [Code of Conduct FAQ](https://opensource.microsoft.com/codeofconduct/faq/) or
contact [opencode@microsoft.com](mailto:opencode@microsoft.com) with any additional questions or comments.
