IoC Extraction
==============


This class allows you to extract IoC patterns from a string or a
DataFrame. Several patterns are built in to the class and you can
override these or supply new ones.


.. code:: ipython3

    # Imports
    import sys
    MIN_REQ_PYTHON = (3,6)
    if sys.version_info < MIN_REQ_PYTHON:
        print('Check the Kernel->Change Kernel menu and ensure that Python 3.6')
        print('or later is selected as the active kernel.')
        sys.exit("Python %s.%s or later is required.\n" % MIN_REQ_PYTHON)

    from IPython.display import display, HTML
    import matplotlib.pyplot as plt
    import seaborn as sns
    sns.set()
    import pandas as pd
    pd.set_option('display.max_rows', 500)
    pd.set_option('display.max_columns', 50)
    pd.set_option('display.max_colwidth', 100)

.. code:: ipython3

    # Load test data
    process_tree = pd.read_csv('data/process_tree.csv')
    process_tree[['CommandLine']].head()




.. raw:: html

    <div>
    <style scoped>
        .dataframe tbody tr th:only-of-type {
            vertical-align: middle;
        }

        .dataframe tbody tr th {
            vertical-align: top;
        }

        .dataframe thead th {
            text-align: right;
        }
    </style>
    <table border="1" class="dataframe">
      <thead>
        <tr style="text-align: right;">
          <th></th>
          <th>CommandLine</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <th>0</th>
          <td>.\ftp  -s:C:\RECYCLER\xxppyy.exe</td>
        </tr>
        <tr>
          <th>1</th>
          <td>.\reg  not /domain:everything that /sid:shines is /krbtgt:golden !</td>
        </tr>
        <tr>
          <th>2</th>
          <td>cmd  /c "systeminfo &amp;&amp; systeminfo"</td>
        </tr>
        <tr>
          <th>3</th>
          <td>.\rundll32  /C 12345.exe</td>
        </tr>
        <tr>
          <th>4</th>
          <td>.\rundll32  /C c:\users\MSTICAdmin\12345.exe</td>
        </tr>
      </tbody>
    </table>
    </div>
    <br/>




Looking for IoC in a String
---------------------------

Just pass the string as a parameter to the extract() method.


Get a commandline from our data set.

.. code:: ipython3

    # get a commandline from our data set
    cmdline = process_tree['CommandLine'].loc[78]
    cmdline




.. parsed-literal::

    'netsh  start capture=yes IPv4.Address=1.2.3.4 tracefile=C:\\\\Users\\\\user\\\\AppData\\\\Local\\\\Temp\\\\bzzzzzz.txt'


Instantiate an IoCExtract instance and pass the string to the extract() method.

.. code:: ipython3

    # Instantiate an IoCExtract object
    from msticpy.sectools import IoCExtract
    ioc_extractor = IoCExtract()

    # any IoCs in the string?
    iocs_found = ioc_extractor.extract(cmdline)

    if iocs_found:
        print('\nPotential IoCs found in alert process:')
        display(iocs_found)



.. parsed-literal::


    Potential IoCs found in alert process:



.. parsed-literal::

    defaultdict(set,
                {'ipv4': {'1.2.3.4'},
                 'windows_path': {'C:\\\\Users\\\\user\\\\AppData\\\\Local\\\\Temp\\\\bzzzzzz.txt'}})


The following IoC patterns are searched for:

* ipv4
* ipv6
* dns
* url
* windows_path
* linux_path
* md5_hash
* sha1_hash
* sha256_hash


Using a DataFrame as Input
--------------------------

You can use the ``data=`` parameter to
IoCExtract.extract() to pass a DataFrame. Use the ``columns``
parameter to specify which column or columns that you want to search.

.. note:: When searching a DataFrame
    the following types are not included in the search by default
    ``windows_path`` and ``linux_path`` because of the likely high volume
    of results and number of false positive matches. You can
    include them by specifing ``include_paths=True`` as a parameter to
    ``extract()``.

    You can also use the ``ioc_types`` parameter to explicitly list the
    ioc_types that you want to search for. This should be a list of
    strings of valid types.
    See :py:meth:`ioc_types<msticpy.sectools.ioc_extractor.IoCExtract.ioc_types>`


.. code:: ipython3

    ioc_extractor = IoCExtract()
    ioc_df = ioc_extractor.extract(data=process_tree, columns=['CommandLine'])
    if len(ioc_df):
        display(HTML("<h3>IoC patterns found in process tree.</h3>"))
        display(ioc_df)



.. raw:: html

    <h3>IoC patterns found in process tree.</h3>



.. raw:: html

    <div>
    <style scoped>
        .dataframe tbody tr th:only-of-type {
            vertical-align: middle;
        }

        .dataframe tbody tr th {
            vertical-align: top;
        }

        .dataframe thead th {
            text-align: right;
        }
    </style>
    <table border="1" class="dataframe">
      <thead>
        <tr style="text-align: right;">
          <th></th>
          <th>IoCType</th>
          <th>Observable</th>
          <th>SourceIndex</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <th>48</th>
          <td>windows_path</td>
          <td>.\powershell</td>
          <td>36</td>
        </tr>
        <tr>
          <th>49</th>
          <td>url</td>
          <td>http://somedomain/best-kitten-names-1.jpg'</td>
          <td>37</td>
        </tr>
        <tr>
          <th>53</th>
          <td>windows_path</td>
          <td>.\pOWErS^H^ElL^.eX^e^</td>
          <td>37</td>
        </tr>
        <tr>
          <th>58</th>
          <td>md5_hash</td>
          <td>81ed03caf6901e444c72ac67d192fb9c</td>
          <td>44</td>
        </tr>
        <tr>
          <th>59</th>
          <td>url</td>
          <td>http://badguyserver/pwnme"</td>
          <td>46</td>
        </tr>
        <tr>
          <th>68</th>
          <td>windows_path</td>
          <td>.\reg  query add mscfile\\\\open</td>
          <td>59</td>
        </tr>
        <tr>
          <th>72</th>
          <td>windows_path</td>
          <td>\system\CurrentControlSet\Control\Terminal</td>
          <td>63</td>
        </tr>
        <tr>
          <th>92</th>
          <td>ipv4</td>
          <td>1.2.3.4</td>
          <td>78</td>
        </tr>
        <tr>
          <th>108</th>
          <td>ipv4</td>
          <td>127.0.0.1</td>
          <td>102</td>
        </tr>
        <tr>
          <th>109</th>
          <td>url</td>
          <td>http://127.0.0.1/</td>
          <td>102</td>
        </tr>
        <tr>
          <th>110</th>
          <td>windows_path</td>
          <td>\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Svchost\MyNastySvcHostConfig</td>
          <td>103</td>
        </tr>
      </tbody>
    </table>
    </div>
    <br/>


IoCExtractor API
----------------

See :py:class:`IoCExtract<msticpy.sectools.ioc_extractor.IoCExtract>`
and See :py:func:`IoCExtract<msticpy.sectools.ioc_extractor.IoCExtract.extract>`


Predefined Regex Patterns
-------------------------

.. code:: ipython3

    from html import escape
    extractor = IoCExtract()

    for ioc_type, pattern in extractor.ioc_types.items():
        esc_pattern = escape(pattern.comp_regex.pattern)
        display(HTML(f'<b>{ioc_type}</b>'))
        display(HTML(f'<div style="margin-left:20px"><pre>{esc_pattern}</pre></div>)'))



.. raw:: html

    <table border="1">
      <thead>
        <tr style="text-align: right;">
          <th>IoCType</th>
          <th>Regex</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <td>ipv4</td>
          <td><pre>(?P&lt;ipaddress&gt;(?:[0-9]{1,3}\\.){3}[0-9]{1,3})</pre></td>
        </tr>
        <tr>
          <td>ipv6</td>
          <td><pre>(?&lt;![:.\\w])(?:[A-F0-9]{1,4}:){7}[A-F0-9]{1,4}(?![:.\\w])</pre></td>
        </tr>
        <tr>
          <td>dns</td>
          <td><pre>((?=[a-z0-9-]{1,63}\\.)[a-z0-9]+(-[a-z0-9]+)*\\.){2,}[a-z]{2,63}</pre></td>
        </tr>
        <tr>
          <td>url</td>
          <td>
            <pre>(?P&lt;protocol&gt;(https?|ftp|telnet|ldap|file)://)&#10;</pre>
            <pre>(?P&lt;userinfo&gt;([a-z0-9-._~!$&amp;\\'()*+,;=:]|%[0-9A-F]{2})*@)?&#10;</pre>
            <pre>(?P&lt;host&gt;([a-z0-9-._~!$&amp;\\'()*+,;=]|%[0-9A-F]{2})*)</pre>
          </td>
        </tr>
        <tr>
          <td>windows_path</td>
          <td>
            <pre>&#13;&#10;(?P&lt;root&gt;[a-z]:|\\\\\\\\[a-z0-9_.$-]+||[.]+)&#10;</pre>
            <pre>(?P&lt;folder&gt;\\\\(?:[^\\/:*?&quot;\\\'&lt;&gt;|\\r\\n]+\\\\)*)&#10;></pre>
            <pre>(?P&lt;file&gt;[^\\\\/*?&quot;&quot;&lt;&gt;|\\r\\n ]+)</pre>
          </td>
        </tr>
        <tr>
          <td>linux_path</td>
          <td>
            <pre>(?P&lt;root&gt;/+||[.]+)&#10;</pre>
            <pre>(?P&lt;folder&gt;/(?:[^\\\\/:*?&lt;&gt;|\\r\\n]+/)*)&#10;</pre>
            <pre>(?P&lt;file&gt;[^/\\0&lt;&gt;|\\r\\n ]+)</pre>
          </td>
        <tr>
          <td>md5_hash</td>
          <td><pre>(?:^|[^A-Fa-f0-9])(?P&lt;hash&gt;[A-Fa-f0-9]{32})(?:$|[^A-Fa-f0-9])</pre></td>
        </tr>
        <tr>
          <td>sha1_hash</td>
          <td><pre>(?:^|[^A-Fa-f0-9])(?P&lt;hash&gt;[A-Fa-f0-9]{40})(?:$|[^A-Fa-f0-9])</pre></td>
        </tr>
          <tr>
          <td>ipv6</td>
          <td><pre>(?:^|[^A-Fa-f0-9])(?P&lt;hash&gt;[A-Fa-f0-9]{64})(?:$|[^A-Fa-f0-9])</pre></td>
        </tr>
      </table>
      <br>



Adding your own pattern(s)
--------------------------


See :py:func:`add_ioc_type<msticpy.sectools.ioc_extractor.IoCExtract.add_ioc_type>`


Add an IoC type and regular expression to use to the built-in set.

.. warning:: Adding an ioc_type that exists in the internal set will overwrite that item

Regular expressions are compiled with re.I | re.X | re.M (Ignore case, Verbose
and MultiLine)

add_ioc_type parameters:

-  ioc_type{str} - a unique name for the IoC type
-  ioc_regex{str} - a regular expression used to search for the type


.. code:: ipython3

    import re
    rcomp = re.compile(r'(?P<pipe>\\\\\.\\pipe\\[^\s\\]+)')

.. code:: ipython3

    extractor.add_ioc_type(ioc_type='win_named_pipe', ioc_regex=r'(?P<pipe>\\\\\.\\pipe\\[^\s\\]+)')

    # Check that it added ok
    print(extractor.ioc_types['win_named_pipe'])

    # Use it in our data set
    ioc_extractor.extract(data=process_tree, columns=['CommandLine']).query('IoCType == \'win_named_pipe\'')


.. parsed-literal::

    IoCPattern(ioc_type='win_named_pipe', comp_regex=re.compile('(?P<pipe>\\\\\\\\\\.\\\\pipe\\\\[^\\s\\\\]+)', re.IGNORECASE|re.MULTILINE|re.VERBOSE), priority=0)




.. raw:: html

    <div>
    <style scoped>
        .dataframe tbody tr th:only-of-type {
            vertical-align: middle;
        }

        .dataframe tbody tr th {
            vertical-align: top;
        }

        .dataframe thead th {
            text-align: right;
        }
    </style>
    <table border="1" class="dataframe">
      <thead>
        <tr style="text-align: right;">
          <th></th>
          <th>IoCType</th>
          <th>Observable</th>
          <th>SourceIndex</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <th>116</th>
          <td>win_named_pipe</td>
          <td>\\.\pipe\blahtest"</td>
          <td>107</td>
        </tr>
      </tbody>
    </table>
    </div>
    <br>
extract_df()
~~~~~~~~~~~~

``extract_df`` functions identically to ``extract`` with a ``data``
parameter. It may be more convenient to use this when you know that your
input is a DataFrame

.. code:: ipython3

    ioc_extractor.extract_df(process_tree, columns=['NewProcessName', 'CommandLine']).head(10)






Merging output with source data
-------------------------------

The SourceIndex column allows you to merge the
results with the input DataFrame Where an input row has multiple IoC
matches the output of this merge will result in duplicate rows from the
input (one per IoC match). The previous index is preserved in the second
column (and in the SourceIndex column).

Note: you will need to set the type of the SourceIndex column. In the
example below case we are matching with the default numeric index so we
force the type to be numeric. In cases where you are using an index of a
different dtype you will need to convert the SourceIndex (dtype=object)
to match the type of your index column.

.. code:: ipython3

    input_df = data=process_tree.head(20)
    output_df = ioc_extractor.extract(data=input_df, columns=['NewProcessName', 'CommandLine'])
    # set the type of the SourceIndex column. In this case we are matching with the default numeric index.
    output_df['SourceIndex'] = pd.to_numeric(output_df['SourceIndex'])
    merged_df = pd.merge(left=input_df, right=output_df, how='outer', left_index=True, right_on='SourceIndex')
    merged_df.head()




.. raw:: html

    <div>
    <style scoped>
        .dataframe tbody tr th:only-of-type {
            vertical-align: middle;
        }

        .dataframe tbody tr th {
            vertical-align: top;
        }

        .dataframe thead th {
            text-align: right;
        }
    </style>
    <table border="1" class="dataframe">
      <thead>
        <tr style="text-align: right;">
          <th></th>
          <th>Unnamed: 0</th>
          <th>TenantId</th>
          <th>Account</th>
          <th>EventID</th>
          <th>TimeGenerated</th>
          <th>Computer</th>
          <th>SubjectUserSid</th>
          <th>SubjectUserName</th>
          <th>SubjectDomainName</th>
          <th>SubjectLogonId</th>
          <th>NewProcessId</th>
          <th>NewProcessName</th>
          <th>TokenElevationType</th>
          <th>ProcessId</th>
          <th>CommandLine</th>
          <th>ParentProcessName</th>
          <th>TargetLogonId</th>
          <th>SourceComputerId</th>
          <th>TimeCreatedUtc</th>
          <th>NodeRole</th>
          <th>Level</th>
          <th>ProcessId1</th>
          <th>NewProcessId1</th>
          <th>IoCType</th>
          <th>Observable</th>
          <th>SourceIndex</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <th>0</th>
          <td>0</td>
          <td>802d39e1-9d70-404d-832c-2de5e2478eda</td>
          <td>MSTICAlertsWin1\MSTICAdmin</td>
          <td>4688</td>
          <td>2019-01-15 05:15:15.677</td>
          <td>MSTICAlertsWin1</td>
          <td>S-1-5-21-996632719-2361334927-4038480536-500</td>
          <td>MSTICAdmin</td>
          <td>MSTICAlertsWin1</td>
          <td>0xfaac27</td>
          <td>0x1580</td>
          <td>C:\Diagnostics\UserTmp\ftp.exe</td>
          <td>%%1936</td>
          <td>0xbc8</td>
          <td>.\ftp  -s:C:\RECYCLER\xxppyy.exe</td>
          <td>C:\Windows\System32\cmd.exe</td>
          <td>0x0</td>
          <td>46fe7078-61bb-4bed-9430-7ac01d91c273</td>
          <td>2019-01-15 05:15:15.677</td>
          <td>source</td>
          <td>0</td>
          <td>NaN</td>
          <td>NaN</td>
          <td>windows_path</td>
          <td>C:\Diagnostics\UserTmp\ftp.exe</td>
          <td>0</td>
        </tr>
        <tr>
          <th>1</th>
          <td>0</td>
          <td>802d39e1-9d70-404d-832c-2de5e2478eda</td>
          <td>MSTICAlertsWin1\MSTICAdmin</td>
          <td>4688</td>
          <td>2019-01-15 05:15:15.677</td>
          <td>MSTICAlertsWin1</td>
          <td>S-1-5-21-996632719-2361334927-4038480536-500</td>
          <td>MSTICAdmin</td>
          <td>MSTICAlertsWin1</td>
          <td>0xfaac27</td>
          <td>0x1580</td>
          <td>C:\Diagnostics\UserTmp\ftp.exe</td>
          <td>%%1936</td>
          <td>0xbc8</td>
          <td>.\ftp  -s:C:\RECYCLER\xxppyy.exe</td>
          <td>C:\Windows\System32\cmd.exe</td>
          <td>0x0</td>
          <td>46fe7078-61bb-4bed-9430-7ac01d91c273</td>
          <td>2019-01-15 05:15:15.677</td>
          <td>source</td>
          <td>0</td>
          <td>NaN</td>
          <td>NaN</td>
          <td>windows_path</td>
          <td>C:\RECYCLER\xxppyy.exe</td>
          <td>0</td>
        </tr>
        <tr>
          <th>2</th>
          <td>0</td>
          <td>802d39e1-9d70-404d-832c-2de5e2478eda</td>
          <td>MSTICAlertsWin1\MSTICAdmin</td>
          <td>4688</td>
          <td>2019-01-15 05:15:15.677</td>
          <td>MSTICAlertsWin1</td>
          <td>S-1-5-21-996632719-2361334927-4038480536-500</td>
          <td>MSTICAdmin</td>
          <td>MSTICAlertsWin1</td>
          <td>0xfaac27</td>
          <td>0x1580</td>
          <td>C:\Diagnostics\UserTmp\ftp.exe</td>
          <td>%%1936</td>
          <td>0xbc8</td>
          <td>.\ftp  -s:C:\RECYCLER\xxppyy.exe</td>
          <td>C:\Windows\System32\cmd.exe</td>
          <td>0x0</td>
          <td>46fe7078-61bb-4bed-9430-7ac01d91c273</td>
          <td>2019-01-15 05:15:15.677</td>
          <td>source</td>
          <td>0</td>
          <td>NaN</td>
          <td>NaN</td>
          <td>windows_path</td>
          <td>.\ftp</td>
          <td>0</td>
        </tr>
        <tr>
          <th>3</th>
          <td>1</td>
          <td>802d39e1-9d70-404d-832c-2de5e2478eda</td>
          <td>MSTICAlertsWin1\MSTICAdmin</td>
          <td>4688</td>
          <td>2019-01-15 05:15:16.167</td>
          <td>MSTICAlertsWin1</td>
          <td>S-1-5-21-996632719-2361334927-4038480536-500</td>
          <td>MSTICAdmin</td>
          <td>MSTICAlertsWin1</td>
          <td>0xfaac27</td>
          <td>0x16fc</td>
          <td>C:\Diagnostics\UserTmp\reg.exe</td>
          <td>%%1936</td>
          <td>0xbc8</td>
          <td>.\reg  not /domain:everything that /sid:shines is /krbtgt:golden !</td>
          <td>C:\Windows\System32\cmd.exe</td>
          <td>0x0</td>
          <td>46fe7078-61bb-4bed-9430-7ac01d91c273</td>
          <td>2019-01-15 05:15:16.167</td>
          <td>sibling</td>
          <td>1</td>
          <td>NaN</td>
          <td>NaN</td>
          <td>windows_path</td>
          <td>C:\Diagnostics\UserTmp\reg.exe</td>
          <td>1</td>
        </tr>
        <tr>
          <th>4</th>
          <td>1</td>
          <td>802d39e1-9d70-404d-832c-2de5e2478eda</td>
          <td>MSTICAlertsWin1\MSTICAdmin</td>
          <td>4688</td>
          <td>2019-01-15 05:15:16.167</td>
          <td>MSTICAlertsWin1</td>
          <td>S-1-5-21-996632719-2361334927-4038480536-500</td>
          <td>MSTICAdmin</td>
          <td>MSTICAlertsWin1</td>
          <td>0xfaac27</td>
          <td>0x16fc</td>
          <td>C:\Diagnostics\UserTmp\reg.exe</td>
          <td>%%1936</td>
          <td>0xbc8</td>
          <td>.\reg  not /domain:everything that /sid:shines is /krbtgt:golden !</td>
          <td>C:\Windows\System32\cmd.exe</td>
          <td>0x0</td>
          <td>46fe7078-61bb-4bed-9430-7ac01d91c273</td>
          <td>2019-01-15 05:15:16.167</td>
          <td>sibling</td>
          <td>1</td>
          <td>NaN</td>
          <td>NaN</td>
          <td>windows_path</td>
          <td>.\reg</td>
          <td>1</td>
        </tr>
      </tbody>
    </table>
    </div>
    <br>


IPython magic
-------------

You can use the line magic ``%ioc`` or cell magic ``%%ioc`` to extract
IoCs from text pasted directly into a cell

The ioc magic supports the following options:

::

   --out OUT, -o OUT
       The variable to return the results in the variable `OUT`
       Note: the output variable is a dictionary iocs grouped by IoC Type
   --ioc_types IOC_TYPES, -i IOC_TYPES
       The types of IoC to search for (comma-separated string)

.. code:: ipython3

    %%ioc --out ioc_capture
    netsh  start capture=yes IPv4.Address=1.2.3.4 tracefile=C:\Users\user\AppData\Local\Temp\bzzzzzz.txt
    hostname	customers-service.ddns.net		Feb 5, 2020, 2:20:35 PM		7
    URL	https://two-step-checkup.site/securemail/secureLogin/challenge/url?ucode=d50a3eb1-9a6b-45a8-8389-d5203bbddaa1&amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;service=mailservice&amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;type=password		Feb 5, 2020, 2:20:35 PM		1
    hostname	mobile.phonechallenges-submit.site		Feb 5, 2020, 2:20:35 PM		8
    hostname	youtube.service-activity-checkup.site		Feb 5, 2020, 2:20:35 PM		8
    hostname	www.drive-accounts.com		Feb 5, 2020, 2:20:35 PM		7
    hostname	google.drive-accounts.com		Feb 5, 2020, 2:20:35 PM		7
    domain	niaconucil.org		Feb 5, 2020, 2:20:35 PM		11
    domain	isis-online.net		Feb 5, 2020, 2:20:35 PM		11
    domain	bahaius.info		Feb 5, 2020, 2:20:35 PM		11
    domain	w3-schools.org		Feb 5, 2020, 2:20:35 PM		12
    domain	system-services.site		Feb 5, 2020, 2:20:35 PM		11
    domain	accounts-drive.com		Feb 5, 2020, 2:20:35 PM		8
    domain	drive-accounts.com		Feb 5, 2020, 2:20:35 PM		10
    domain	service-issues.site		Feb 5, 2020, 2:20:35 PM		8
    domain	two-step-checkup.site		Feb 5, 2020, 2:20:35 PM		8
    domain	customers-activities.site		Feb 5, 2020, 2:20:35 PM		11
    domain	seisolarpros.org		Feb 5, 2020, 2:20:35 PM		11
    domain	yah00.site		Feb 5, 2020, 2:20:35 PM		4
    domain	skynevvs.com		Feb 5, 2020, 2:20:35 PM		11
    domain	recovery-options.site		Feb 5, 2020, 2:20:35 PM		4
    domain	malcolmrifkind.site		Feb 5, 2020, 2:20:35 PM		8
    domain	instagram-com.site		Feb 5, 2020, 2:20:35 PM		8
    domain	leslettrespersanes.net		Feb 5, 2020, 2:20:35 PM		11
    domain	software-updating-managers.site		Feb 5, 2020, 2:20:35 PM		8
    domain	cpanel-services.site		Feb 5, 2020, 2:20:35 PM		8
    domain	service-activity-checkup.site		Feb 5, 2020, 2:20:35 PM		7
    domain	inztaqram.ga		Feb 5, 2020, 2:20:35 PM		8
    domain	unirsd.com		Feb 5, 2020, 2:20:35 PM		8
    domain	phonechallenges-submit.site		Feb 5, 2020, 2:20:35 PM		7
    domain	acconut-verify.com		Feb 5, 2020, 2:20:35 PM		11
    domain	finance-usbnc.info		Feb 5, 2020, 2:20:35 PM		8
    FileHash-MD5	542128ab98bda5ea139b169200a50bce		Feb 5, 2020, 2:20:35 PM		3
    FileHash-MD5	3d67ce57aab4f7f917cf87c724ed7dab		Feb 5, 2020, 2:20:35 PM		3
    hostname	x09live-ix3b.account-profile-users.info		Feb 6, 2020, 2:56:07 PM		0
    hostname	www.phonechallenges-submit.site		Feb 6, 2020, 2:56:07 PM




.. parsed-literal::

    [('ipv4', ['1.2.3.4']),
     ('dns',
      ['malcolmrifkind.site',
       'w3-schools.org',
       'niaconucil.org',
       'software-updating-managers.site',
       'isis-online.net',
       'accounts-drive.com',
       'cpanel-services.site',
       'service-activity-checkup.site',
       'service-issues.site',
       'recovery-options.site',
       'instagram-com.site',
       'mobile.phonechallenges-submit.site',
       'youtube.service-activity-checkup.site',
       'google.drive-accounts.com',
       'phonechallenges-submit.site',
       'drive-accounts.com',
       'www.phonechallenges-submit.site',
       'yah00.site',
       'seisolarpros.org',
       'customers-activities.site',
       'bahaius.info',
       'system-services.site',
       'two-step-checkup.site',
       'x09live-ix3b.account-profile-users.info',
       'customers-service.ddns.net',
       'leslettrespersanes.net',
       'www.drive-accounts.com',
       'acconut-verify.com',
       'finance-usbnc.info',
       'unirsd.com',
       'skynevvs.com',
       'inztaqram.ga']),
     ('url',
      ['https://two-step-checkup.site/securemail/secureLogin/challenge/url?ucode=d50a3eb1-9a6b-45a8-8389-d5203bbddaa1&amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;service=mailservice&amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;type=password']),
     ('windows_path', ['C:\\Users\\user\\AppData\\Local\\Temp\\bzzzzzz.txt']),
     ('linux_path',
      ['//two-step-checkup.site/securemail/secureLogin/challenge/url?ucode=d50a3eb1-9a6b-45a8-8389-d5203bbddaa1&amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;service=mailservice&amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;type=password\t\tFeb']),
     ('md5_hash',
      ['3d67ce57aab4f7f917cf87c724ed7dab', '542128ab98bda5ea139b169200a50bce'])]


.. code:: ipython3

    %%ioc --ioc_types "ipv4, ipv6, linux_path, md5_hash"
    netsh  start capture=yes IPv4.Address=1.2.3.4 tracefile=C:\Users\user\AppData\Local\Temp\bzzzzzz.txt
    tracefile2=/usr/localbzzzzzz.sh
    hostname	customers-service.ddns.net		Feb 5, 2020, 2:20:35 PM		7
    URL	https://two-step-checkup.site/securemail/secureLogin/challenge/url?ucode=d50a3eb1-9a6b-45a8-8389-d5203bbddaa1&amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;service=mailservice&amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;type=password		Feb 5, 2020, 2:20:35 PM		1
    hostname	mobile.phonechallenges-submit.site		Feb 5, 2020, 2:20:35 PM		8
    hostname	youtube.service-activity-checkup.site		Feb 5, 2020, 2:20:35 PM		8
    hostname	www.drive-accounts.com		Feb 5, 2020, 2:20:35 PM		7
    hostname	google.drive-accounts.com		Feb 5, 2020, 2:20:35 PM		7
    domain	niaconucil.org		Feb 5, 2020, 2:20:35 PM		11
    domain	isis-online.net		Feb 5, 2020, 2:20:35 PM		11
    domain	bahaius.info		Feb 5, 2020, 2:20:35 PM		11
    domain	w3-schools.org		Feb 5, 2020, 2:20:35 PM		12
    domain	system-services.site		Feb 5, 2020, 2:20:35 PM		11
    domain	accounts-drive.com		Feb 5, 2020, 2:20:35 PM		8
    domain	drive-accounts.com		Feb 5, 2020, 2:20:35 PM		10
    domain	service-issues.site		Feb 5, 2020, 2:20:35 PM		8
    domain	two-step-checkup.site		Feb 5, 2020, 2:20:35 PM		8
    domain	customers-activities.site		Feb 5, 2020, 2:20:35 PM		11
    domain	seisolarpros.org		Feb 5, 2020, 2:20:35 PM		11
    domain	yah00.site		Feb 5, 2020, 2:20:35 PM		4
    domain	skynevvs.com		Feb 5, 2020, 2:20:35 PM		11
    domain	recovery-options.site		Feb 5, 2020, 2:20:35 PM		4
    domain	malcolmrifkind.site		Feb 5, 2020, 2:20:35 PM		8
    domain	instagram-com.site		Feb 5, 2020, 2:20:35 PM		8
    domain	leslettrespersanes.net		Feb 5, 2020, 2:20:35 PM		11
    domain	software-updating-managers.site		Feb 5, 2020, 2:20:35 PM		8
    domain	cpanel-services.site		Feb 5, 2020, 2:20:35 PM		8
    domain	service-activity-checkup.site		Feb 5, 2020, 2:20:35 PM		7
    domain	inztaqram.ga		Feb 5, 2020, 2:20:35 PM		8
    domain	unirsd.com		Feb 5, 2020, 2:20:35 PM		8
    domain	phonechallenges-submit.site		Feb 5, 2020, 2:20:35 PM		7
    domain	acconut-verify.com		Feb 5, 2020, 2:20:35 PM		11
    domain	finance-usbnc.info		Feb 5, 2020, 2:20:35 PM		8
    FileHash-MD5	542128ab98bda5ea139b169200a50bce		Feb 5, 2020, 2:20:35 PM		3
    FileHash-MD5	3d67ce57aab4f7f917cf87c724ed7dab		Feb 5, 2020, 2:20:35 PM		3
    hostname	x09live-ix3b.account-profile-users.info		Feb 6, 2020, 2:56:07 PM		0
    hostname	www.phonechallenges-submit.site		Feb 6, 2020, 2:56:07 PM




.. parsed-literal::

    [('ipv4', ['1.2.3.4']),
     ('linux_path',
      ['//two-step-checkup.site/securemail/secureLogin/challenge/url?ucode=d50a3eb1-9a6b-45a8-8389-d5203bbddaa1&amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;service=mailservice&amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;amp;type=password\t\tFeb',
       '/usr/localbzzzzzz.sh']),
     ('md5_hash',
      ['3d67ce57aab4f7f917cf87c724ed7dab', '542128ab98bda5ea139b169200a50bce'])]




Pandas Extension
----------------

The decoding functionality is also available in a pandas extension
``mp_ioc``. This supports a single method ``extract()``.

This supports the same syntax as ``extract`` (described earlier).

.. code:: ipython3

    process_tree.mp_ioc.extract(columns=['CommandLine'])


.. raw:: html

    <div>
    <style scoped>
        .dataframe tbody tr th:only-of-type {
            vertical-align: middle;
        }

        .dataframe tbody tr th {
            vertical-align: top;
        }

        .dataframe thead th {
            text-align: right;
        }
    </style>
    <table border="1" class="dataframe">
      <thead>
        <tr style="text-align: right;">
          <th></th>
          <th>IoCType</th>
          <th>Observable</th>
          <th>SourceIndex</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <th>0</th>
          <td>dns</td>
          <td>microsoft.com</td>
          <td>24</td>
        </tr>
        <tr>
          <th>1</th>
          <td>url</td>
          <td>http://server/file.sct</td>
          <td>31</td>
        </tr>
        <tr>
          <th>2</th>
          <td>dns</td>
          <td>server</td>
          <td>31</td>
        </tr>
        <tr>
          <th>3</th>
          <td>dns</td>
          <td>evil.ps</td>
          <td>35</td>
        </tr>
        <tr>
          <th>4</th>
          <td>url</td>
          <td>http://somedomain/best-kitten-names-1.jpg'</td>
          <td>37</td>
        </tr>
        <tr>
          <th>5</th>
          <td>dns</td>
          <td>somedomain</td>
          <td>37</td>
        </tr>
        <tr>
          <th>6</th>
          <td>dns</td>
          <td>blah.ps</td>
          <td>40</td>
        </tr>
        <tr>
          <th>7</th>
          <td>md5_hash</td>
          <td>aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa</td>
          <td>40</td>
        </tr>
        <tr>
          <th>8</th>
          <td>dns</td>
          <td>blah.ps</td>
          <td>41</td>
        </tr>
        <tr>
          <th>9</th>
          <td>md5_hash</td>
          <td>aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa</td>
          <td>41</td>
        </tr>
        <tr>
          <th>10</th>
          <td>md5_hash</td>
          <td>81ed03caf6901e444c72ac67d192fb9c</td>
          <td>44</td>
        </tr>
        <tr>
          <th>11</th>
          <td>url</td>
          <td>http://badguyserver/pwnme</td>
          <td>46</td>
        </tr>
        <tr>
          <th>12</th>
          <td>dns</td>
          <td>badguyserver</td>
          <td>46</td>
        </tr>
        <tr>
          <th>13</th>
          <td>url</td>
          <td>http://badguyserver/pwnme</td>
          <td>47</td>
        </tr>
        <tr>
          <th>14</th>
          <td>dns</td>
          <td>badguyserver</td>
          <td>47</td>
        </tr>
        <tr>
          <th>15</th>
          <td>dns</td>
          <td>Invoke-Shellcode.ps</td>
          <td>48</td>
        </tr>
        <tr>
          <th>16</th>
          <td>dns</td>
          <td>Invoke-ReverseDnsLookup.ps</td>
          <td>49</td>
        </tr>
        <tr>
          <th>17</th>
          <td>dns</td>
          <td>Wscript.Shell</td>
          <td>67</td>
        </tr>
        <tr>
          <th>18</th>
          <td>url</td>
          <td>http://system.management.automation.amsiutils').getfield('amsiinitfailed','nonpublic,static').se...</td>
          <td>77</td>
        </tr>
        <tr>
          <th>19</th>
          <td>dns</td>
          <td>system.management.automation.amsiutils').getfield('amsiinitfailed','nonpublic,static').setvalue(...</td>
          <td>77</td>
        </tr>
        <tr>
          <th>20</th>
          <td>ipv4</td>
          <td>1.2.3.4</td>
          <td>78</td>
        </tr>
        <tr>
          <th>21</th>
          <td>dns</td>
          <td>wscript.shell</td>
          <td>81</td>
        </tr>
        <tr>
          <th>22</th>
          <td>dns</td>
          <td>abc.com</td>
          <td>90</td>
        </tr>
        <tr>
          <th>23</th>
          <td>ipv4</td>
          <td>127.0.0.1</td>
          <td>102</td>
        </tr>
        <tr>
          <th>24</th>
          <td>url</td>
          <td>http://127.0.0.1/</td>
          <td>102</td>
        </tr>
        <tr>
          <th>25</th>
          <td>win_named_pipe</td>
          <td>\\.\pipe\blahtest"</td>
          <td>107</td>
        </tr>
      </tbody>
    </table>
    </div>

