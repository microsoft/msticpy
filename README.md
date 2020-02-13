# MSTIC Jupyter and Python Security Tools

Microsoft Threat Intelligence Python Security Tools.

The **msticpy** package was initially developed to support [Jupyter Notebook](https://jupyter-notebook.readthedocs.io/en/stable/examples/Notebook/What%20is%20the%20Jupyter%20Notebook.html)
authoring for [Azure Sentinel](https://azure.microsoft.com/en-us/services/azure-sentinel/).
Many of the included tools can be used in other security scenarios for threat hunting
and threat investigation.

<img src="https://github.com/microsoft/msticpy/blob/master/docs/source/visualization/_static/Timeline-08.png"
alt="Timeline" title="Msticpy Timeline Control" width="300" height="200" />

There are three main sub-packages:

- **sectools** - Python security tools to help with data enrichment,
  analysis or investigation.
- **nbtools** - Jupyter-specific UI tools such as widgets, plotting and
  other data display.
- **data** - data layer and pre-defined queries for Azure Sentinel, MDATP and
   other data sources.

We welcome feedback, bug reports, suggestions for new features and contributions.

## Installing

`pip install msticpy`

or for the latest dev build

`pip install git+https://github.com/microsoft/msticpy`

## Documentation

Full documentation is at [ReadTheDocs](https://msticpy.readthedocs.io/en/latest/)

Sample notebooks for many of the modules are in the [docs/notebooks](https://github.com/microsoft/msticpy/blob/master/docs/notebooks) folder and accompanying notebooks.

You can also browse through the sample notebooks referenced at the end of this document
(especially the *Windows Alert Investigation* notebook) to see some of the functionality used in context.

---

## Security Tools Sub-package - `sectools`

This subpackage contains several modules helpful for working on security investigations and hunting:

### base64unpack

Base64 and archive (gz, zip, tar) extractor. Input can either be a single string
or a specified column of a pandas dataframe. It will try to identify any base64 encoded
strings and decode them. If the result looks like one of the supported archive types it
will unpack the contents. The results of each decode/unpack are rechecked for further
base64 content and will recurse down up to 20 levels (default can be overridden).
Output is to a decoded string (for single string input) or a DataFrame (for dataframe input).

[Base64Unpack Notebook](https://github.com/microsoft/msticpy/blob/master/docs/notebooks/Base64Unpack.ipynb)

### iocextract

Uses a set of built-in regular expressions to look for Indicator of Compromise (IoC) patterns.
Input can be a single string or a pandas dataframe with one or more columns specified as input.

The following types are built-in:

- IPv4 and IPv6
- URL
- DNS domain
- Hashes (MD5, SHA1, SHA256)
- Windows file paths
- Linux file paths (this is kind of noisy because a legal Linux file path can have almost any character)

You can modify or add to the regular expressions used at runtime.

Output is a dictionary of matches (for single string input) or a DataFrame (for dataframe input).

[IoCExtract Notebook](https://github.com/microsoft/msticpy/blob/master/docs/notebooks/IoCExtract.ipynb)

### tiproviders

The TILookup class can lookup IoCs across multiple TI providers. built-in
providers include AlienVault OTX, IBM XForce, VirusTotal and Azure Sentinel.

The input can be a single IoC observable or a pandas DataFrame containing
multiple observables. Depending on the provider, you may require an account
and an API key. Some providers also enforce throttling (especially for free
tiers), which might affect performing bulk lookups.

For more details see `TIProviders` and
[TILookup Usage Notebook](https://github.com/microsoft/msticpy/blob/master/docs/notebooks/TIProviders.ipynb)

### vtlookup

Wrapper class around [Virus Total API](https://www.virustotal.com/en/documentation/public-api/).
Input can be a single IoC observable or a pandas DataFrame containing multiple observables.
Processing requires a Virus Total account and API key and processing performance is limited to
the number of requests per minute for the account type that you have.
Support IoC Types:

- Filehash
- URL
- DNS Domain
- IPv4 Address

[VTLookup Notebook](https://github.com/microsoft/msticpy/blob/master/docs/notebooks/VirusTotalLookup.ipynb)

### geoip

Geographic location lookup for IP addresses.

<img src="https://github.com/microsoft/msticpy/blob/PandasMagicExtensions/docs/source/visualization/_static/FoliumMap-01.png"
  alt="Folium map"
  title="Plotting Geo IP Location" width="150" height="100" />

This module has two classes for different services:

- GeoLiteLookup - Maxmind Geolite (see <https://www.maxmind.com>)
- IPStackLookup  - IPStack (see <https://ipstack.com>)

Both services offer a free tier for non-commercial use. However,
a paid tier will normally get you more accuracy, more detail and
a higher throughput rate. Maxmind geolite uses a downloadable database,
while IPStack is an online lookup (API key required).

[GeoIP Lookup Notebook](https://github.com/microsoft/msticpy/blob/master/docs/notebooks/GeoIPLookups.ipynb)

### eventcluster

This module is intended to be used to summarize large numbers of
events into clusters of different patterns. High volume repeating
events can often make it difficult to see unique and interesting items.

<img src="https://github.com/microsoft/msticpy/blob/PandasMagicExtensions/docs/source/data_analysis/_static/EventClustering_2a.png"
  alt="Clustering"
  title="Clustering based on command-line variability" width="150" height="200" />

This is an unsupervised learning module implemented using SciKit Learn DBScan.

The module contains functions to generate clusterable features from
string data. For example, an administration command that
does some maintenance on thousands of servers with a commandline like the following

```bash
install-update -hostname {host.fqdn} -tmp:/tmp/{GUID}/rollback
```

can be collapsed into a single cluster pattern by ignoring the character
values of the host and guids in the string and using delimiters or tokens to
group the values. This allows you to more easily see distinct patterns of
activity.

[Event Clustering Notebook](https://github.com/microsoft/msticpy/blob/master/docs/notebooks/EventClustering.ipynb)

### outliers

Similar to the eventcluster module, but a little bit more experimental (read 'less tested').
It uses SkLearn Isolation Forest to identify outlier events in a single data set or using
one data set as training data and another on which to predict outliers.

### auditdextract

Module to load and decode Linux audit logs. It collapses messages sharing the same
message ID into single events, decodes hex-encoded data fields and performs some
event-specific formatting and normalization (e.g. for process start events it will
re-assemble the process command line arguments into a single string).

This is still a work-in-progress.

### syslog_utils

Module to support an investigation of a linux host with only syslog logging enabled.
This includes functions for collating host data, clusting logon events and detecting
user sessions containing suspicious activity.

### cmd_line

A module to support he detection of known malicious command line activity or suspicious
patterns of command line activity.

## Notebook tools sub-package - `nbtools`

This is a collection of display and utility modules designed to make working
with security data in Jupyter notebooks quicker and easier.

- nbwidgets - groups common functionality such as list pickers, time boundary settings, saving and retrieving environment variables into a single line callable command.
- nbdisplay - functions that implement common display of things like alerts, events in a slightly more consumable way than print()
- entityschema - implements entity classes (e.g. Host, Account, IPAddress) used in Log Analytics alerts and in many of these modules. Each entity encaspulates one or more properties related to the entity.

[Notebook Tools Notebook](https://github.com/microsoft/msticpy/blob/master/docs/notebooks/NotebookWidgets.ipynb) and [Event Timeline Visualization](https://github.com/microsoft/msticpy/blob/master/docs/notebooks/EventTimeline.ipynb)

## Data sub-package - `data`

These components are currently still part of the nbtools sub-package but will be
refactored to separate them into their own package.

- QueryProvider - extensible query library targeting Log Analytics or OData
  endpoints. Built-in parameterized queries allow complex queries to be run
  from a single function call. Add your own queries using a simple YAML
  schema.
- security_alert and security_event - encapsulation classes for alerts and events.
- entity_schema - definitions for multiple entities (Host, Account, File, IPAddress,
  etc.)

Each has a standard 'entities' property reflecting the entities found in the alert or event.
These can also be used as meta-parameters for many of the queries.
For example, the following query will extract the value for the `hostname` query parameter
from the alert:

`qry.list_host_logons(query_times, alert)`

[Data Queries Notebook](https://github.com/microsoft/msticpy/blob/master/docs/notebooks/Data_Queries.ipynb)

---

## Clone the notebooks in this repo to Azure Notebooks

Requires sign-in to Azure Notebooks
<a href="https://notebooks.azure.com/import/gh/Microsoft/msticpy">
<img src="https://notebooks.azure.com/launch.png" />
</a>

## More Notebooks

- [Account Explorer](https://github.com/Azure/Azure-Sentinel-Notebooks/blob/master/Entity%20Explorer%20-%20Account.ipynb)
- [Domain and URL Explorer](https://github.com/Azure/Azure-Sentinel-Notebooks/blob/master/Entity%20Explorer%20-%20Domain%20%26%20URL.ipynb)
- [IP Explorer](https://github.com/Azure/Azure-Sentinel-Notebooks/blob/master/Entity%20Explorer%20-%20IP%20Address.ipynb)
- [Linux Host Explorer](https://github.com/Azure/Azure-Sentinel-Notebooks/blob/master/Entity%20Explorer%20-%20Linux%20Host.ipynb)
- [Windows Host Explorer](https://github.com/Azure/Azure-Sentinel-Notebooks/blob/master/Entity%20Explorer%20-%20Windows%20Host.ipynb)

View directly on GitHub or copy and paste the link into [nbviewer.org](https://nbviewer.jupyter.org/)

## Notebook examples with saved data

See the following notebooks for more examples of the use of this package in practice:

- Windows Alert Investigation in [github](https://github.com/Azure/Azure-Sentinel-Notebooks/blob/master/Sample-Notebooks/Example%20-%20Guided%20Investigation%20-%20Process-Alerts.ipynb) or [NbViewer](https://nbviewer.jupyter.org/github/Azure/Azure-Sentinel-Notebooks/blob/master/Sample-Notebooks/Example%20-%20Guided%20Investigation%20-%20Process-Alerts.ipynb)
- Office 365 Exploration in [github](https://github.com/Azure/Azure-Sentinel-Notebooks/blob/master/Sample-Notebooks/Example%20-%20Guided%20Hunting%20-%20Office365-Exploring.ipynb) or [NbViewer](https://nbviewer.jupyter.org/github/Azure/Azure-Sentinel-Notebooks/blob/master/Sample-Notebooks/Example%20-%20Guided%20Hunting%20-%20Office365-Exploring.ipynb)
- Cross-Network Hunting in [github](https://github.com/Azure/Azure-Sentinel-Notebooks/blob/master/Sample-Notebooks/Example%20-%20Step-by-Step%20Linux-Windows-Office%20Investigation.ipynb) or [NbViewer](https://nbviewer.jupyter.org/github/Azure/Azure-Sentinel-Notebooks/blob/master/Sample-Notebooks/Example%20-%20Step-by-Step%20Linux-Windows-Office%20Investigation.ipynb)

## To-Do Items

- Add additional notebooks to document use of the tools.
- Expand list of supported TI provider classes.

## Supported Platforms and Packages

- msticpy is OS-independent
- Requires [Python 3.6 or later](https://www.python.org/dev/peps/pep-0494/)
- Requires the following python packages: pandas, bokeh, matplotlib, seaborn, setuptools, urllib3, ipywidgets, numpy, attrs, requests, networkx, ipython, scikit_learn, typing
- The following packages are recommended and needed for some specific functionality: Kqlmagic, maxminddb_geolite2, folium, dnspython, ipwhois

See [requirements.txt](requirements.txt) for more details and version requirements.

---

## Contributing

This project welcomes contributions and suggestions.  Most contributions require you to agree to a
Contributor License Agreement (CLA) declaring that you have the right to, and actually do, grant us
the rights to use your contribution. For details, visit <https://cla.microsoft.com>.

When you submit a pull request, a CLA-bot will automatically determine whether you need to provide
a CLA and decorate the PR appropriately (e.g., label, comment). Simply follow the instructions
provided by the bot. You will only need to do this once across all repos using our CLA.

This project has adopted the [Microsoft Open Source Code of Conduct](https://opensource.microsoft.com/codeofconduct/).
For more information see the [Code of Conduct FAQ](https://opensource.microsoft.com/codeofconduct/faq/) or
contact [opencode@microsoft.com](mailto:opencode@microsoft.com) with any additional questions or comments.
