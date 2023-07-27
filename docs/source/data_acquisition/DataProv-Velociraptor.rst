The Velociraptor provider
=========================

:py:mod:`Velociraptor driver documentation<msticpy.data.drivers.local_velociraptor_driver>`

The ``Velociraptor`` data provider can read Velociraptor
offline collection log files (see
`Velociraptor Offline Collection <https://docs.velociraptor.app/docs/offline_triage/#offline-collections>`__)
and provide convenient query functions for each data set
in the output logs.

The provider can read files from one or more hosts, stored in
in separate folders. The files are read, converted to pandas
DataFrames and grouped by table/event. Multiple log files of the
same type (when reading in data from multiple hosts) are concatenated
into a single DataFrame.

.. code::ipython3

    qry_prov = mp.QueryProvider("Velociraptor", data_paths=["~/my_logs"])
    qry_prov.connect()
    df_processes = qry_prov.velociraptor.Windows_Forensics_ProcessInfo()

The query provider query functions will ignore parameters and do
no further filtering. You can use pandas to do additional filtering
and sorting of the data, or use it directly with other MSTICPy
functionality.

.. note:: The examples used in this document were from data
    provided by Blue Team Village at Defcon 30. You can find
    this data at the
    `Project-Obsidian-DC30 GitHub <https://github.com/blueteamvillage/Project-Obsidian-DC30>`__
    and more about
    `Project Obsidian <https://media.blueteamvillage.org/DC30/Obsidian/>`__
    here.

Velociraptor Configuration
--------------------------

You can (optionally) store your connection details in *msticpyconfig.yaml*,
instead of supplying the ``data_paths`` parameter to
the ``QueryProvider`` class.

For more information on using and configuring *msticpyconfig.yaml* see
:doc:`msticpy Package Configuration <../getting_started/msticpyconfig>`
and :doc:`MSTICPy Settings Editor<../getting_started/SettingsEditor>`

The Velociraptor settings in the file should look like the following:

.. code:: yaml

    DataProviders:
        ...
        Velociraptor:
            data_paths:
                - /home/user1/sample_data
                - /home/shared/sample_data


Expected log file format
------------------------

The log file format must be a text file of JSON records. An example
is shown below

.. parsed-literal::

    {"Pid":1664,"Ppid":540,"Name":"spoolsv.exe","Path":"C:\\Windows\\System32\\spoolsv.exe","CommandLine":"C:\\Windows\\System32\\spoolsv.exe","Hash":{"MD5":"c111e3d38c71808a8289b0e49db40c96","SHA1":"e56df979d776fe9e8c3b84e6fef8559d6811898d","SHA256":"0ed0c6f4ddc620039f05719d783585d69f03d950be97b49149d4addf23609902"},"Username":"NT AUTHORITY\\SYSTEM","Authenticode":{"Filename":"C:\\Windows\\System32\\spoolsv.exe","ProgramName":"Microsoft Windows","PublisherLink":null,"MoreInfoLink":"http://www.microsoft.com/windows","SerialNumber":"33000002ed2c45e4c145cf48440000000002ed","IssuerName":"C=US, ST=Washington, L=Redmond, O=Microsoft Corporation, CN=Microsoft Windows Production PCA 2011","SubjectName":"C=US, ST=Washington, L=Redmond, O=Microsoft Corporation, CN=Microsoft Windows","Timestamp":null,"Trusted":"trusted","_ExtraInfo":{"Catalog":"C:\\Windows\\system32\\CatRoot\\{F750E6C3-38EE-11D1-85E5-00C04FC295EE}\\Package_6350_for_KB5007192~31bf3856ad364e35~amd64~~10.0.1.8.cat"}},"Family":"IPv4","Type":"TCP","Status":"LISTEN","Laddr.IP":"0.0.0.0","Laddr.Port":49697,"Raddr.IP":"0.0.0.0","Raddr.Port":0,"Timestamp":"2022-02-12T19:35:45Z"}
    {"Pid":548,"Ppid":416,"Name":"lsass.exe","Path":"C:\\Windows\\System32\\lsass.exe","CommandLine":"C:\\Windows\\system32\\lsass.exe","Hash":{"MD5":"93212fd52a9cd5addad2fd2a779355d2","SHA1":"49a814f72292082a1cfdf602b5e4689b0f942703","SHA256":"95888daefd187fac9c979387f75ff3628548e7ddf5d70ad489cf996b9cad7193"},"Username":"NT AUTHORITY\\SYSTEM","Authenticode":{"Filename":"C:\\Windows\\System32\\lsass.exe","ProgramName":"Microsoft Windows","PublisherLink":null,"MoreInfoLink":"http://www.microsoft.com/windows","SerialNumber":"33000002f49e469c54137b85e00000000002f4","IssuerName":"C=US, ST=Washington, L=Redmond, O=Microsoft Corporation, CN=Microsoft Windows Production PCA 2011","SubjectName":"C=US, ST=Washington, L=Redmond, O=Microsoft Corporation, CN=Microsoft Windows Publisher","Timestamp":null,"Trusted":"trusted","_ExtraInfo":null},"Family":"IPv4","Type":"TCP","Status":"LISTEN","Laddr.IP":"0.0.0.0","Laddr.Port":49722,"Raddr.IP":"0.0.0.0","Raddr.Port":0,"Timestamp":"2022-02-12T19:35:54Z"}
    {"Pid":540,"Ppid":416,"Name":"services.exe","Path":"C:\\Windows\\System32\\services.exe","CommandLine":"C:\\Windows\\system32\\services.exe","Hash":{"MD5":"fefc26105685c70d7260170489b5b520","SHA1":"d9b2cb9bf9d4789636b5fcdef0fdbb9d8bc0fb52","SHA256":"930f44f9a599937bdb23cf0c7ea4d158991b837d2a0975c15686cdd4198808e8"},"Username":"NT AUTHORITY\\SYSTEM","Authenticode":{"Filename":"C:\\Windows\\System32\\services.exe","ProgramName":"Microsoft Windows","PublisherLink":null,"MoreInfoLink":"http://www.microsoft.com/windows","SerialNumber":"33000002a5e1a081b7c895c0ed0000000002a5","IssuerName":"C=US, ST=Washington, L=Redmond, O=Microsoft Corporation, CN=Microsoft Windows Production PCA 2011","SubjectName":"C=US, ST=Washington, L=Redmond, O=Microsoft Corporation, CN=Microsoft Windows Publisher","Timestamp":null,"Trusted":"trusted","_ExtraInfo":null},"Family":"IPv4","Type":"TCP","Status":"LISTEN","Laddr.IP":"0.0.0.0","Laddr.Port":49728,"Raddr.IP":"0.0.0.0","Raddr.Port":0,"Timestamp":"2022-02-12T19:35:57Z"}


The columns in each JSON will be used to create the pandas DataFrame columns.


Using the Velociraptor provider
-------------------------------

To use the Velociraptor provider you need to create an QueryProvider
instance, passing the string "VelociraptorLogs" as the ``data_environment``
parameter. If you have not configured ``data_paths`` in msticpyconfig.yaml,
you also need to add the ``data_paths`` parameter to specify
specific folders or files that you want to read.

.. code::ipython3

    qry_prov = mp.QueryProvider("VelociraptorLogs", data_paths=["~/my_logs"])

Calling the ``connect`` method triggers the provider to register the paths of the
log files to be read (although the log files are not read and parsed
until the related query is run - see below).

.. code::ipython3

    qry_prov.connect()



Listing Velociraptor tables
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Until you run ``connect`` no queries will be available. After running
``connect`` you can list the available queries using the ``list_queries``

.. code:: ipython3

    qry_prov.list_queries()

.. parsed-literal::

    ['velociraptor.Custom_Windows_NetBIOS',
    'velociraptor.Custom_Windows_Patches',
    'velociraptor.Custom_Windows_Sysinternals_PSInfo',
    'velociraptor.Custom_Windows_Sysinternals_PSLoggedOn',
    'velociraptor.Custom_Windows_System_Services',
    'velociraptor.Windows_Applications_Chrome_Cookies',
    'velociraptor.Windows_Applications_Chrome_Extensions',
    'velociraptor.Windows_Applications_Chrome_History',
    'velociraptor.Windows_Applications_Edge_History',
    'velociraptor.Windows_Forensics_Lnk',
    'velociraptor.Windows_Forensics_Prefetch',
    'velociraptor.Windows_Forensics_ProcessInfo',
    'velociraptor.Windows_Forensics_Usn',
    ...]

Querying Velociraptor table schema
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The schema of the log tables is built by sampling the first record
from each log file type, so is relatively fast to retrieve even
if you have large numbers and sizes of logs.

.. code:: ipython3

    vc_prov.schema["Windows_Network_InterfaceAddresses"]

.. parsed-literal::

    {'Index': 'int64',
    'MTU': 'int64',
    'Name': 'object',
    'HardwareAddr': 'object',
    'Flags': 'int64',
    'IP': 'object',
    'Mask': 'object'}

Running a Velociraptor query
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Each query returns a pandas DataFrame retrieved
from the logs of that type (potentially containing records from
multiple hosts depending on the ``data_paths`` you specified).

.. code:: python3

    qry_prov.vc_prov.velociraptor.Windows_Forensics_ProcessInfo()


====  ===========  ================  =====  ===============================  ================================================================  ====================  ===================================
  ..  Name         PebBaseAddress      Pid  ImagePathName                    CommandLine                                                       CurrentDirectory      Env
====  ===========  ================  =====  ===============================  ================================================================  ====================  ===================================
  10  LogonUI.exe  0x95bd3d2000        804  C:\Windows\system32\LogonUI.exe  "LogonUI.exe" /flags:0x2 /state0:0xa3b92855 /state1:0x41c64e6d    C:\Windows\system32\  {'ALLUSERSPROFILE': 'C:\\ProgramD..
  11  dwm.exe      0x6cf4351000        848  C:\Windows\system32\dwm.exe      "dwm.exe"                                                         C:\Windows\system32\  {'ALLUSERSPROFILE': 'C:\\ProgramD..
  12  svchost.exe  0x6cd64d000         872  C:\Windows\System32\svchost.exe  C:\Windows\System32\svchost.exe -k termsvcs                       C:\Windows\system32\  {'ALLUSERSPROFILE': 'C:\\ProgramD..
  13  svchost.exe  0x7d18e99000        912  C:\Windows\System32\svchost.exe  C:\Windows\System32\svchost.exe -k LocalServiceNetworkRestricted  C:\Windows\system32\  {'ALLUSERSPROFILE': 'C:\\ProgramD..
  14  svchost.exe  0x5c762eb000        920  C:\Windows\system32\svchost.exe  C:\Windows\system32\svchost.exe -k LocalService                   C:\Windows\system32\  {'ALLUSERSPROFILE': 'C:\\ProgramD..
====  ===========  ================  =====  ===============================  ================================================================  ====================  ===================================
