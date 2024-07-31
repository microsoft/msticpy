Package Summary
===============

.. note:: This document is intended to summarize most of the functionality of
   *MSTICPy* in a single page but it is difficult to keep it up-to-date.
   You may find it easier to use the ReadTheDocs menu to browse around
   the various functions of this package.


Data Acquisition and Queries
----------------------------

See :doc:`../DataAcquisition`

QueryProviders
~~~~~~~~~~~~~~

:py:mod:`msticpy.data.data_providers.QueryProvider`

Extensible query library targeting Log Analytics, Splunk, OData
and other log data sources. Also special support for Mordor data
sets and using local data.

Built-in parameterized queries allow complex queries to be run
from a single function call. Add your own queries using a simple YAML
schema.

See :doc:`../data_acquisition/DataProviders`

Sample notebook - `Data Queries Notebook <https://github.com/microsoft/msticpy/blob/master/docs/notebooks/Data_Queries.ipynb>`__


Data Processing and Enrichment
------------------------------


Threat Intelligence Providers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:py:mod:`msticpy.context.tilookup`

The TILookup class can lookup IoCs across multiple TI providers. builtin
providers include AlienVault OTX, IBM XForce, VirusTotal and Azure Sentinel.

The input can be a single IoC observable or a pandas DataFrame containing
multiple observables. Depending on the provider, you may require an account
and an API key. Some providers also enforce throttling (especially for free
tiers), which might affect performing bulk lookups.

See :doc:`../data_acquisition/TIProviders`

Sample notebook - `TILookup Usage Notebook <https://github.com/microsoft/msticpy/blob/master/docs/notebooks/TIProviders.ipynb>`__

vtlookup and vtlookupv3
~~~~~~~~~~~~~~~~~~~~~~~

:py:mod:`msticpy.context.vtlookupv3`

Wrapper class around `Virus Total
API <https://www.virustotal.com/en/documentation/public-api/>`__.
Processing requires a Virus Total account and API key.
Supported IoC Types:

-  Filehash
-  URL
-  DNS Domain
-  IPv4 Address

Sample notebook - `VTLookup Usage Notebook <https://github.com/microsoft/msticpy/blob/master/docs/notebooks/VirusTotalLookup.ipynb>`__

IP geo-location lookups
~~~~~~~~~~~~~~~~~~~~~~~

:py:mod:`msticpy.context.geoip`

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

Azure Resource Data, Storage and Azure Sentinel API
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:py:mod:`msticpy.context.azure.azure_data`
:py:mod:`msticpy.context.azure.sentinel_core`
:py:mod:`msticpy.data.storage.azure_blob_storage`

The AzureData module contains functionality for enriching data regarding Azure host
details with additional host details exposed via the Azure API. The AzureSentinel
module allows you to query incidents, retrieve detector and hunting
queries. AzureBlogStorage lets you read and write data from blob storage.

See :doc:`../data_acquisition/AzureData`, :doc:`../data_acquisition/Sentinel`
:doc:`../data_acquisition/AzureBlobStorage`


Pivot Functions
~~~~~~~~~~~~~~~
:py:mod:`msticpy.data_model.pivot`
:py:mod:`msticpy.data_model.entities`

Lets you use *MSTICPy* functionality in an "entity-centric" way.
All functions, queries and lookups that relate to a particular entity type
(e.g. Host, IpAddress, Url) are collected together as methods of that
entity class. So, if you want to do things with an IP address, just load
the IpAddress entity and browse its methods.

See :doc:`../data_analysis/PivotFunctions`


Security Analysis
-----------------

Anomalous Sequence Detection
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:py:mod:`msticpy.analysis.anomalous_sequence.anomalous`

Detect unusual sequences of events in your Office, Active Directory or other log data.
You can extract sessions (e.g. activity initiated by the same account) and identify and
visualize unusual sequences of activity. For example, detecting an attacker setting
a mail forwarding rule on someone's mailbox.

See :doc:`../data_analysis/AnomalousSequence`

Sample notebook - `Anomalous Sequence Notebook <https://github.com/microsoft/msticpy/blob/master/docs/notebooks/AnomalousSequence.ipynb>`__

Time Series Analysis
~~~~~~~~~~~~~~~~~~~~

:py:mod:`msticpy.analysis.timeseries`

Time series analysis allows you to identify unusual patterns in your log data
taking into account normal seasonal variations (e.g. the regular ebb and flow of
events over hours of the day, days of the week, etc.). Using both analysis and
visualization highlights unusual traffic flows or event activity for any data
set.

See :doc:`../visualization/TimeSeriesAnomalies`

Sample notebook - `Time Series <https://github.com/microsoft/msticpy/blob/master/docs/notebooks/TimeSeriesAnomaliesVisualization.ipynb>`__

eventcluster
~~~~~~~~~~~~

:py:mod:`msticpy.analysis.eventcluster`

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

:py:mod:`msticpy.analysis.outliers`

Similar to the eventcluster module but a little bit more experimental
(read 'less tested'). It uses SkLearn Isolation Forest to identify
outlier events in a single data set or using one data set as training
data and another on which to predict outliers.


Visualization
-------------

This is a collection of display and utility modules designed to make
working with security data in Jupyter notebooks quicker and easier.

See :doc:`../Visualization`

Process tree
~~~~~~~~~~~~

:py:mod:`msticpy.vis.process_tree` - process tree visualization.

The process tree functionality has two main components:

-  Process Tree creation - taking a process creation log from a host and building
   the parent-child relationships between processes in the data set.
-  Process Tree visualization - this takes the processed output displays an interactive process tree using Bokeh plots.

There are a set of utility functions to extract individual and partial trees from the processed data set.

See :doc:`../visualization/ProcessTree`

Sample notebook - `Process Tree Visualization <https://github.com/microsoft/msticpy/blob/master/docs/notebooks/ProcessTree.ipynb>`_

Event timeline
~~~~~~~~~~~~~~

:py:mod:`msticpy.vis.timeline` - event timeline visualization.

Display any log events on an interactive timeline. Using the
`Bokeh Visualization Library <https://bokeh.org/>`__ the timeline control enables
you to visualize one or more event streams, interactively zoom into specific time
slots and view event details for plotted events.

See :doc:`../visualization/EventTimeline`

Sample notebook - `Event Timeline Visualization <https://github.com/microsoft/msticpy/blob/master/docs/notebooks/EventTimeline.ipynb>`_


Notebook widgets
~~~~~~~~~~~~~~~~

:py:mod:`msticpy.nbwidgets`

Common functionality such as list pickers, time
boundary settings, saving and retrieving environment variables into a
single line callable command.

See :doc:`../visualization/NotebookWidgets`

Sample notebook - `Event Clustering Notebook <https://github.com/microsoft/msticpy/blob/master/docs/notebooks/NotebookWidgets.ipynb>`__


Utility Functions
-----------------

base64unpack
~~~~~~~~~~~~

:py:mod:`msticpy.transform.base64unpack`

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

:py:mod:`msticpy.transform.iocextract`

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


auditdextract
~~~~~~~~~~~~~

:py:mod:`msticpy.transform.auditdextract`

Module to load and decode Linux audit logs. It collapses messages
sharing the same message ID into single events, decodes hex-encoded data
fields and performs some event-specific formatting and normalization
(e.g. for process start events it will re-assemble the process command
line arguments into a single string).


syslog_utils
~~~~~~~~~~~~

:py:mod:`msticpy.transform.syslog_utils`

Module to support the investigation of Linux hosts through Syslog.
Includes functions to create host records, cluster logon events, and
identify user sessions containing suspicious activity.

cmd_line
~~~~~~~~

:py:mod:`msticpy.transform.cmd_line`

Module to investigation of command line activity. Allows for the detection
of known malicious commands as well as suspicious patterns of behaviour.

domain_utils
~~~~~~~~~~~~

:py:mod:`msticpy.transform.domain_utils`

Module to support investigation of domain names and URLs with functions to
validate a domain name and screenshot a URL.



Data Masking
~~~~~~~~~~~~
:py:mod:`msticpy.data.data_obfus`

Lets you obfuscate senstive data in logs to allow sharing, presentations
without compromising privacy.

See :doc:`../data_acquisition/DataMasking`




Agentic Module
-----------------
aiagents Module: RAG Agent
~~~~~~~~~~~~~~~~~~~~~~~~~~

:py:mod:`msticpy.aiagents.mp_docs_rag_magic`

The **aiagents** module in MSTICpy introduces the **Retrieval-Augmented Generation (RAG) Agent**.
This agent is designed for Q&A about MSTICpy. It enhances InfoSec investigations and
threat hunting in Jupyter Notebooks by providing contextually relevant responses using large language
models and external knowledge retrieval from the MSTICpy documentation.

You can invoke the RAG agent in a Jupyter Notebook by adding the following:

.. code-block:: python

   # Load the RAG cell magic
   %load_ext msticpy.aiagents.mp_docs_rag_magic

   # The following command should be in a separate cell and be the first line of the cell
   %%ask
   What are the three things that I need to connect to Azure Query Provider?

You can also invoke the RAG Agent outside of a Jupyter Notebook as follows:

.. code-block:: python

   # Import the necessary module
   from .rag_agents import (
    ask_question,
    get_retrieval_assistant_agent,
    get_retrieval_user_proxy_agent,
   )

   # Initialize the agents
   assistant_agent = get_retrieval_assistant_agent()
   user_proxy_agent = get_retrieval_user_proxy_agent()

   # Define your question
   question = "Your MSTICpy-related question here"

   # Query the agents
   response = ask_question(
      assistant_agent,
      user_proxy_agent,
      question=question,
   )

   print(response.summary)


Add Autogen configurations to your msticpconfig - `Autogen LLM Configurations in msticpconfig <https://github.com/microsoft/msticpy/blob/master/docs/source/getting_started/msticpyconfig.rst>`__
Sample notebook - `RAG Agent Example Notebook <https://github.com/microsoft/msticpy/blob/master/docs/notebooks/RagAgent.ipynb>`__


Supported Platforms and Packages
--------------------------------

-  msticpy is OS-independent
-  Requires Python 3.8 or later
-  See `requirements.txt <https://github.com/microsoft/msticpy/blob/master/requirements.txt>`__
   for more details and version requirements.

