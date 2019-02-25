# MSTIC Jupyter and Python Security Tools

Microsoft Threat Intelligence Python Security Package:

## sectools
This subpackage contains several modules helpful for working on security
investigations and hunting:
### base64unpack
Base64 and archive (gz, zip, tar) extractor. Input can either be a single string or a specified column of a pandas dataframe. It will try to identify any base64 encoded strings and decode them. If the result looks like one of the supported archive types it will unpack the contents. The results of each decode/unpack are rechecked for further base64 content and will recurse down up to 20 levels (default can be overridden).
Output is to a decoded string (for single string input) or a DataFrame (for dataframe input).
[Base64Unpack Notebook](./doc/Base64Unpack.ipynb)

### iocextract
Uses a set of builtin regular expressions to look for Indicator of Compromise (IoC) patterns. Input can be a single string or a pandas dataframe with one or more columns specified as input.
The following types are built-in:
- IPv4 and IPv6
- URL
- DNS domain
- Hashes (MD5, SHA1, SHA256)
- Windows file paths
- Linux file paths (this is kind of noisy because a legal linux file path can have almost any character)
You can modify or add to the regular expressions used at runtime.

Output is a dictionary of matches (for single string input) or a DataFrame (for dataframe input).

### vtlookup
Wrapper class around Virus Total API (https://www.virustotal.com/en/documentation/public-api/).
Input can be a single IoC observable or a pandas DataFrame containing multiple observables.
Processing requires a Virus Total account and API key and processing performance is limited to 
the number of requests per minute for the account type that you have.
Support IoC Types:
- Filehash
- URL
- DNS Domain
- IPv4 Address

[VTLookup Notebook](./doc/VTLookup.ipynb)

### geoip
Geographic location lookup for IP addresses.
This module has two classes for different services:
- GeoLiteLookup - Maxmind Geolite (see https://www.maxmind.com)
- IPStackLookup  - IPStack (see https://ipstack.com)
Both services offer a free tier for non-commercial use. However, 
a paid tier will normally get you more accuracy, more detail and 
a higher throughput rate. Maxmind geolite uses a downloadable database, 
while IPStack is an online lookup (API key required).

### eventcluster
This module is intended to be used to summarize large numbers of 
events into clusters of different patterns. High volume repeating 
events can often make it difficult to see unique and interesting
items. The module uses a pattern-based approach rather than 
matching on exact strings - so an admin command that 
does some maintenance on thousands of servers with a commandline such as:
```install-update -hostname {host.fqdn} -tmp:/tmp/{GUID}/rollback```
Will be collapsed using the pattern of the command and ignoring 
individal host names and guids.
This is an unsupervised learning module implemented using SciKit Learn DBScan.
[EventClustering Notebook](./doc/EventClustering.ipynb)

## nbtools
This is a collection of data access, display and utility modules 
designed to make working with Log Analytics data in Jupyter notebooks 
quicker and easier.
- nbwidgets - groups common functionality such as list pickers, 
time boundary settings, saving and retrieving
environment variables into a single line callable command.
- nbdisplay - functions that implement common display of things like 
alerts, events in a slightly more consumable way than print()
- entityschema - implements entity classes (e.g. Host, Account, IPAddress) 
used in Log Analytics alerts and in many of these modules. 
Each entity encaspulates one or more properties related to the entity.
- query manager - collection of modules that implement common 
kql/Log Analytics queries using KqlMagic
- security_alert and security_event - encapsulation classes for alerts 
and events. Each has a standard 'entities' property reflecting the 
entities found in the alert or event. These can also be used as 
meta-parameters for many of the queries. For example the query:
```qry.list_host_logons(provs==[query_times, alert])``` will extract the
value for the ```hostname``` query parameter from the alert.

# Contributing

This project welcomes contributions and suggestions.  Most contributions require you to agree to a
Contributor License Agreement (CLA) declaring that you have the right to, and actually do, grant us
the rights to use your contribution. For details, visit https://cla.microsoft.com.

When you submit a pull request, a CLA-bot will automatically determine whether you need to provide
a CLA and decorate the PR appropriately (e.g., label, comment). Simply follow the instructions
provided by the bot. You will only need to do this once across all repos using our CLA.

This project has adopted the [Microsoft Open Source Code of Conduct](https://opensource.microsoft.com/codeofconduct/).
For more information see the [Code of Conduct FAQ](https://opensource.microsoft.com/codeofconduct/faq/) or
contact [opencode@microsoft.com](mailto:opencode@microsoft.com) with any additional questions or comments.
