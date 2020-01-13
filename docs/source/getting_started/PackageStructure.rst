Package Structure
=================

Security Tools Sub-package - *sectools*
---------------------------------------

This subpackage contains several modules helpful for working on security
investigations and hunting:


auditdextract
~~~~~~~~~~~~~

:py:mod:`msticpy.sectools.auditdextract`

Module to load and decode Linux audit logs. It collapses messages
sharing the same message ID into single events, decodes hex-encoded data
fields and performs some event-specific formatting and normalization
(e.g. for process start events it will re-assemble the process command
line arguments into a single string).


base64unpack
~~~~~~~~~~~~

:py:mod:`msticpy.sectools.base64unpack`

Base64 and archive (gz, zip, tar) extractor. Input can either be a
single string or a specified column of a pandas dataframe. It will try
to identify any base64 encoded strings and decode them. If the result
looks like one of the supported archive types it will unpack the
contents. The results of each decode/unpack are rechecked for further
base64 content and will recurse down up to 20 levels (default can be
overridden). Output is to a decoded string (for single string input) or
a DataFrame (for dataframe input).

See :doc:`../data_analysis/Base64Unpack`

Sample notebook - `Base64Unpack Usage Notebook <https://github.com/microsoft/msticpy/blob/master/docs/notebooks/Base64Unpack.ipynb>`__

iocextract
~~~~~~~~~~

:py:mod:`msticpy.sectools.iocextract`

Uses a set of builtin regular expressions to look for Indicator of
Compromise (IoC) patterns. Input can be a single string or a pandas
dataframe with one or more columns specified as input.

The following types are built-in:

-  IPv4 and IPv6
-  URL
-  DNS domain
-  Hashes (MD5, SHA1, SHA256)
-  Windows file paths
-  Linux file paths (this is kind of noisy because a legal linux file
   path can have almost any character) You can modify or add to the
   regular expressions used at runtime.

Output is a dictionary of matches (for single string input) or a
DataFrame (for dataframe input).

See :doc:`../data_analysis/IoCExtract`

Sample notebook - `IoCExtract Usage Notebook <https://github.com/microsoft/msticpy/blob/master/docs/notebooks/IoCExtract.ipynb>`__

tiproviders
~~~~~~~~~~~

:py:mod:`msticpy.sectools.tilookup`

The TILookup class can lookup IoCs across multiple TI providers. builtin
providers include AlienVault OTX, IBM XForce, VirusTotal and Azure Sentinel.

The input can be a single IoC observable or a pandas DataFrame containing
multiple observables. Depending on the provider, you may require an account
and an API key. Some providers also enforce throttling (especially for free
tiers), which might affect performing bulk lookups.

See :doc:`../data_acquisition/TIProviders`

Sample notebook - `TILookup Usage Notebook <https://github.com/microsoft/msticpy/blob/master/docs/notebooks/TIProviders.ipynb>`__

vtlookup
~~~~~~~~

:py:mod:`msticpy.sectools.vtlookup`

Wrapper class around `Virus Total
API <https://www.virustotal.com/en/documentation/public-api/>`__. Input
can be a single IoC observable or a pandas DataFrame containing multiple
observables. Processing requires a Virus Total account and API key and
processing performance is limited to the number of requests per minute
for the account type that you have. Support IoC Types:

-  Filehash
-  URL
-  DNS Domain
-  IPv4 Address

Sample notebook - `VTLookup Usage Notebook <https://github.com/microsoft/msticpy/blob/master/docs/notebooks/VirusTotalLookup.ipynb>`__

geoip
~~~~~

:py:mod:`msticpy.sectools.geoip`

Geographic location lookup for IP addresses. This module has two classes
for different services:

-  GeoLiteLookup - Maxmind Geolite (see https://www.maxmind.com)
-  IPStackLookup - IPStack (see https://ipstack.com) Both services offer
   a free tier for non-commercial use. However, a paid tier will
   normally get you more accuracy, more detail and a higher throughput
   rate. Maxmind geolite uses a downloadable database, while IPStack is
   an online lookup (API key required).

See :doc:`../data_acquisition/GeoIPLookups`

Sample notebook - `GeoIP Lookup Usage Notebook <https://github.com/microsoft/msticpy/blob/master/docs/notebooks/GeoIPLookups.ipynb>`__

eventcluster
~~~~~~~~~~~~

:py:mod:`msticpy.sectools.eventcluster`

This module is intended to be used to summarize large numbers of events
into clusters of different patterns. High volume repeating events can
often make it difficult to see unique and interesting items.

The module contains functions to generate clusterable features from
string data. For example, an administration command that does some
maintenance on thousands of servers with a commandline such as:
``install-update -hostname {host.fqdn} -tmp:/tmp/{GUID}/rollback``\  can
be collapsed into a single cluster pattern by ignoring the character
values in the string and using delimiters or tokens to group the values.

This is an unsupervised learning module implemented using SciKit Learn
DBScan.

See :doc:`../data_analysis/EventClustering`

Sample notebook - `Event Clustering Notebook <https://github.com/microsoft/msticpy/blob/master/docs/notebooks/EventClustering.ipynb>`__

outliers
~~~~~~~~

:py:mod:`msticpy.sectools.outliers`

Similar to the eventcluster module but a little bit more experimental
(read 'less tested'). It uses SkLearn Isolation Forest to identify
outlier events in a single data set or using one data set as training
data and another on which to predict outliers.


syslog_utils
~~~~~~~~~~~~~

:py:mod:`msticpy.sectools.syslog_utils`

Module to support the investigation of Linux hosts through Syslog.
Includes functions to create host records, cluster logon events, and
identify user sessions containing suspicious activity.

cmd_line
~~~~~~~~~~~~~

:py:mod:`msticpy.sectools.cmd_line`

Module to investigation of command line activity. Allows for the detection
of known malicious commands as well as suspicious patterns of behaviour.

domain_utils
~~~~~~~~~~~~~

:py:mod:`msticpy.sectools.domain_utils`

Module to support investigation of domain names and URLs with functions to
validate a domain name and screenshot a URL.


Notebook tools sub-package - *nbtools*
--------------------------------------

This is a collection of display and utility modules designed to make
working with security data in Jupyter notebooks quicker and easier.

See :doc:`../Visualization`

Notebook widgets
~~~~~~~~~~~~~~~~

:py:mod:`msticpy.nbtools.nbwidgets`


Common functionality such as list pickers, time
boundary settings, saving and retrieving environment variables into a
single line callable command.

See :doc:`../visualization/NotebookWidgets`

Sample notebook - `Event Clustering Notebook <https://github.com/microsoft/msticpy/blob/master/docs/notebooks/NotebookWidgets.ipynb>`__

Display functions
~~~~~~~~~~~~~~~~~

:py:mod:`msticpy.nbtools.nbdisplay`

Common display of things like
alerts, events in a slightly more consumable way than print()

Process tree
~~~~~~~~~~~~

:py:mod:`msticpy.nbtools.process_tree` - process tree visualization.

See :doc:`../visualization/ProcessTree`

Sample notebook - `Process Tree Visualization <https://github.com/microsoft/msticpy/blob/master/docs/notebooks/ProcessTree.ipynb>`_

Event timeline
~~~~~~~~~~~~~~

:py:mod:`msticpy.nbtools.timeline` - event timeline visualization.

See :doc:`../visualization/EventTimeline`

Sample notebook - `Event Timeline Visualization <https://github.com/microsoft/msticpy/blob/master/docs/notebooks/EventTimeline.ipynb>`_


Data sub-package - *data*
-------------------------

See :doc:`../DataAcquisition`

QueryProvider
~~~~~~~~~~~~~

:py:mod:`msticpy.data.data_providers.QueryProvider`

Extensible query library targeting Log Analytics or OData
endpoints. Built-in parameterized queries allow complex queries to be run
from a single function call. Add your own queries using a simple YAML
schema.

See :doc:`../data_acquisition/DataProviders`

Sample notebook - `Data Queries Notebook <https://github.com/microsoft/msticpy/blob/master/docs/notebooks/Data_Queries.ipynb>`__


----

.. note:: The following modules are currently part of the ``nbtools``
   sub-package but will be moved to the ``data`` package.

SecurityAlert and SecurityEvent
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:py:class:`msticpy.nbtools.security_alert.SecurityAlert`

:py:class:`msticpy.nbtools.security_event.SecurityEvent`

Encapsulation classes for
alerts and events. Each has a standard 'entities' property reflecting
the entities found in the alert or event. These can also be used as
meta-parameters for many of the queries. For example the query:
``qry.list_host_logons(query_times, alert)`` will extract
the value for the ``hostname`` query parameter from the alert.



Entities
~~~~~~~~

:py:mod:`msticpy.nbtools.entity_schema`

Entity classes (e.g. Host, Account, IPAddress) used in Azure Security Center
and Azure Sentinel alerts and in many of the modules of *msticpy*.

Each entity encapsulates one or more properties related to the entity.


--------------


To-Do Items
-----------

-  Add additional notebooks to document use of the tools.
-  Expand list of supported TI provider classes.
-  Expand Azure data enrichment options.

Supported Platforms and Packages
--------------------------------

-  msticpy is OS-independent
-  Requires Python 3.6 or later
-  Requires the following python packages: pandas, bokeh, matplotlib,
   seaborn, setuptools, urllib3, ipywidgets, numpy, attrs, requests,
   networkx, ipython, scikit\_learn, typing
-  The following packages are recommended and needed for some specific
   functionality: Kqlmagic, maxminddb\_geolite2, folium, dnspython,
   ipwhois

See `requirements.txt <requirements.txt>`__ for more details and version
requirements.
