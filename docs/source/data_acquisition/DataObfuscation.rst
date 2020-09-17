Data Obfuscation Functions
==========================

Sharing data, creating documents and doing public demonstrations often
require that data containing PII or other sensitive material be
obfuscated.

MSTICPy contains a simple library to obfuscation data using hashing and
random mapping of values. You can use these functions on a single data
items or entire DataFrames.

Import the module
-----------------

.. code:: ipython3

    from msticpy.data import data_obfus

See :py:mod:`data_obfus<msticpy.data.data_obfus>` for API details.


Individual Obfuscation Functions
--------------------------------

In the examples below we’re importing individual functions from the data_obfus module
but you can access them with the single import statement show above as
attributes of that module.

.. code:: ipython3

   data_obfus.hash_string(...)



hash_string
~~~~~~~~~~~

:py:func:`hash_string<msticpy.data.data_obfus.hash_string>`
does a simple hash of the input. If the input is a numeric string it will output a numeric.


.. parsed-literal::


    Hash a simple string.

    Parameters
    ----------
    input_str : str
        The input string

    Returns
    -------
    str
        The obfuscated output string


**Examples**

.. code:: ipython3

    > hash_string('sensitive data')
    jdiqcnrqmlidkd

    > hash_string('12345')
    59944



hash_item
~~~~~~~~~

:py:func:`hash_item<msticpy.data.data_obfus.hash_item>`
allows specification of delimiters. This is useful for preserving the
look of domains, emails, etc.


.. parsed-literal::


    Hash a simple string.

    Parameters
    ----------
    input_item : str
        The input string
    delim: str, optional
        A string of delimiters to use to split the input string
        prior to hashing.

    Returns
    -------
    str
        The obfuscated output string



**Examples**

.. code:: ipython3

    > hash_item('sensitive data', delim=' ')
    kdneqoiia laoe

    > hash_item('most-sensitive-data/here', delim=' /-')
    kmea-kdneqoiia-laoe/fcec



hash_ip
~~~~~~~

:py:func:`hash_ip<msticpy.data.data_obfus.hash_ip>`
will output random mappings of input IP V4 and V6 addresses.
For IPV4 addresses this works by creating a random mapping of each byte
of the address. So multiple occurrences of the the same IP address will
be converted to the same randomized output address.
The mapping remains for the Python session.

.. note: IPV6 addresses have their individual components hashed to a
   hex string and do not use this mapping. This should still result in
   a given input IP address being mapped to the same obfuscated address.
   The output IPV6 address will usually not be a value IP address though.


.. parsed-literal::


    Hash IP address or list of IP addresses.

    Parameters
    ----------
    input_item : Union[List[str], str]
        List of IP addresses or single IP address.

    Returns
    -------
    Union[List[str], str]
        List of hashed addresses or single address.
        (depending on input)



**Examples**

.. code:: ipython3

    > hash_ip('192.168.3.1')
    160.21.239.194

    > hash_ip('2001:0db8:85a3:0000:0000:8a2e:0370:7334')
    85d6:7819:9cce:9af1:9af1:24ad:d338:7d03

    > hash_ip('['192.168.3.1', '192.168.5.2', '192.168.10.2']')
    ['160.21.239.194', '160.21.103.84', '160.21.149.84']


hash_sid
~~~~~~~~

:py:func:`hash_sid<msticpy.data.data_obfus.hash_sid>`
will randomize the domain-specific parts of a Windows SID.
It preserves built-in SIDs and well known RIDs (e.g. Admins '-500' RID will be
preserved in the obfuscated output). Built-in SIDs (such as LocalSystem and
NetworkService are preserved as-is.

.. parsed-literal::


    Hash a SID preserving well-known SIDs and the RID.

    Parameters
    ----------
    sid : str
        SID string

    Returns
    -------
    str
        Hashed SID

**Examples**

.. code:: ipython3

    > hash_sid('S-1-5-21-1180699209-877415012-3182924384-1004')
    S-1-5-21-3321821741-636458740-4143214142-1004

    > hash_sid('S-1-5-18')
    S-1-5-18


hash_list
~~~~~~~~~

:py:func:`hash_list<msticpy.data.data_obfus.hash_list>`
will randomize a list of items preserving the list structure but
treating each element as a simple string to hash.

.. parsed-literal::


    Hash list of strings.

    Parameters
    ----------
    item_list : List[str]
        Input list

    Returns
    -------
    List[str]
        Hashed list


**Examples**

.. code:: ipython3

    >> hash_list('['S-1-5-21-1180699209-877415012-3182924384-1004', 'S-1-5-18']')
    ['elkbjiboklpknokdeflikamojqjflqmicqiorqfbqboqe', 'nrllmpbd']



hash_dict
~~~~~~~~~

:py:func:`hash_dict<msticpy.data.data_obfus.hash_dict>`
will randomize a dict of items preserving the structure and the name of
the dictionary keys. Only the values of the keys are hashed.

.. parsed-literal::


    Hash dictionary values.

    Parameters
    ----------
    item_dict : Dict[str, Union[Dict[str, Any], List[Any], str]]
        Input item can be a Dict of strings, lists or other
        dictionaries.

    Returns
    -------
    Dict[str, Any]
        Dictionary with hashed values.

**Examples**

.. code:: ipython3

    > hash_dict('{'SID1': 'S-1-5-21-1180699209-877415012-3182924384-1004', 'SID2': 'S-1-5-18'}')
    {'SID1': 'elkbjiboklpknokdeflikamojqjflqmicqiorqfbqboqe', 'SID2': 'nrllmpbd'}



replace_guid
~~~~~~~~~~~~

:py:func:`replace_guid<msticpy.data.data_obfus.replace_guid>`
will output a random UUID mapped to the input.
The same input UUUD will be mapped to the same newly-generated output UUID
for the current Python session.

In the example below you can see that UUID #4 is the same as #1 and mapped
to the same output UUID.


.. parsed-literal::


    Replace GUID/UUID with mapped random UUID.

    Parameters
    ----------
    guid : str
        Input UUID.

    Returns
    -------
    str
        Mapped UUID


**Examples**

.. code:: ipython3

    > replace_guid('cf1b0b29-08ae-4528-839a-5f66eca2cce9')
    9ef6c321-14f3-4681-8c3b-b596de52d8b0

    > replace_guid('ed63d29e-6288-4d66-b10d-8847096fc586')
    219a5b0c-3985-49cc-9016-7b23a98c3d53

    > replace_guid('ac561203-99b2-4067-a525-60d45ea0d7ff')
    8e8ec1e1-6df6-4b41-bbff-b73b1614430b

    > replace_guid('cf1b0b29-08ae-4528-839a-5f66eca2cce9')
    9ef6c321-14f3-4681-8c3b-b596de52d8b0



Obfuscating DataFrames
----------------------

We can use the msticpy pandas extension to obfuscate an entire
DataFrame.

See :py:meth:`mp_obf.obfuscate<msticpy.data.data_obfus.ObfuscationAccessor.obfuscate>`

The obfuscation library contains a mapping for a number of common field
names. You can view this list by displaying the attribute:

::

   data_obfus.OBFUS_COL_MAP

In the first example, the TenantId, ResourceGroup, VMName have been
obfuscated.

.. code:: ipython3

    display(netflow_df.head(3))
    netflow_df.head(3).mp_obf.obfuscate()



Input DataFrame

====================================  =======================  =======================  =====================  ===============  =============  ==================================  =======  ========  ============  =============
TenantId                              TimeGenerated            FlowStartTime            ResourceGroup          VMName           VMIPAddress    PublicIPs                             SrcIP    DestIP  L4Protocol    AllExtIPs
====================================  =======================  =======================  =====================  ===============  =============  ==================================  =======  ========  ============  =============
52b1ab41-869e-4138-9e40-2a4457f09bf0  2019-02-12 14:22:40.697  2019-02-12 13:00:07.000  asihuntomsworkspacerg  msticalertswin1  10.0.3.5       ['65.55.44.109']                        nan       nan  T             65.55.44.109
52b1ab41-869e-4138-9e40-2a4457f09bf0  2019-02-12 14:22:40.681  2019-02-12 13:00:48.000  asihuntomsworkspacerg  msticalertswin1  10.0.3.5       ['13.71.172.130', '13.71.172.128']      nan       nan  T             13.71.172.128
52b1ab41-869e-4138-9e40-2a4457f09bf0  2019-02-12 14:22:40.681  2019-02-12 13:00:48.000  asihuntomsworkspacerg  msticalertswin1  10.0.3.5       ['13.71.172.130', '13.71.172.128']      nan       nan  T             13.71.172.130
====================================  =======================  =======================  =====================  ===============  =============  ==================================  =======  ========  ============  =============

Output DataFrame

====================================  =======================  =======================  =====================  ===============  =============  ==================================  =======  ========  ============  =============
TenantId                              TimeGenerated            FlowStartTime            ResourceGroup          VMName           VMIPAddress    PublicIPs                             SrcIP    DestIP  L4Protocol    AllExtIPs
====================================  =======================  =======================  =====================  ===============  =============  ==================================  =======  ========  ============  =============
68a5a31d-7516-4c54-ad27-3b1360ce0b56  2019-02-12 14:22:40.697  2019-02-12 13:00:07.000  ibmkajbmepnmiaeilfofa  msticalertswin1  10.0.3.5       ['65.55.44.109']                        nan       nan  T             65.55.44.109
68a5a31d-7516-4c54-ad27-3b1360ce0b56  2019-02-12 14:22:40.681  2019-02-12 13:00:48.000  ibmkajbmepnmiaeilfofa  msticalertswin1  10.0.3.5       ['13.71.172.130', '13.71.172.128']      nan       nan  T             13.71.172.128
68a5a31d-7516-4c54-ad27-3b1360ce0b56  2019-02-12 14:22:40.681  2019-02-12 13:00:48.000  ibmkajbmepnmiaeilfofa  msticalertswin1  10.0.3.5       ['13.71.172.130', '13.71.172.128']      nan       nan  T             13.71.172.130
====================================  =======================  =======================  =====================  ===============  =============  ==================================  =======  ========  ============  =============

.. note:: TenantId, ResourceGroup and VMName have been obfuscated.




Adding custom column mappings
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the previous example you probably spotted that the VMIPAddress, PublicIPs and
AllExtIPs columns were all unchanged. This is because there is no default mapping
for these column names in the builtin mapping table.

We can add these columns to a custom mapping dictionary and re-run the
obfuscation. See the later section on :ref:`creating_custom_mappings`.

.. code:: ipython3

    col_map = {
        "VMName": ".",
        "VMIPAddress": "ip",
        "PublicIPs": "ip",
        "AllExtIPs": "ip"
    }

    netflow_df.head(3).mp_obf.obfuscate(column_map=col_map)

Output DataFrame after applying custom column mappings

====================================  =======================  =======================  =====================  ===============  ===============  ==================================  =======  ========  ============  =============
TenantId                              TimeGenerated            FlowStartTime            ResourceGroup          VMName           VMIPAddress      PublicIPs                             SrcIP    DestIP  L4Protocol    AllExtIPs
====================================  =======================  =======================  =====================  ===============  ===============  ==================================  =======  ========  ============  =============
68a5a31d-7516-4c54-ad27-3b1360ce0b56  2019-02-12 14:22:40.697  2019-02-12 13:00:07.000  ibmkajbmepnmiaeilfofa  fmlmbnlpdcbnbnn  149.172.239.103  ['62.100.208.57']                       nan       nan  T             62.100.208.57
68a5a31d-7516-4c54-ad27-3b1360ce0b56  2019-02-12 14:22:40.681  2019-02-12 13:00:48.000  ibmkajbmepnmiaeilfofa  fmlmbnlpdcbnbnn  149.172.239.103  ['156.64.40.139', '156.64.40.236']      nan       nan  T             156.64.40.236
68a5a31d-7516-4c54-ad27-3b1360ce0b56  2019-02-12 14:22:40.681  2019-02-12 13:00:48.000  ibmkajbmepnmiaeilfofa  fmlmbnlpdcbnbnn  149.172.239.103  ['156.64.40.139', '156.64.40.236']      nan       nan  T             156.64.40.139
====================================  =======================  =======================  =====================  ===============  ===============  ==================================  =======  ========  ============  =============


obfuscate_df
~~~~~~~~~~~~

You can also call the standard function
:py:func:`obfuscate_df<msticpy.data.data_obfus.obfuscate_df>` to perform the
same operation on the DataFrame passed as the *data* parameter.

.. code:: ipython3

    data_obfus.obfuscate_df(data=netflow_df.head(3), column_map=col_map)

====================================  =======================  =======================  =====================  ===============  ===============  ==================================  =======  ========  ============  =============
TenantId                              TimeGenerated            FlowStartTime            ResourceGroup          VMName           VMIPAddress      PublicIPs                             SrcIP    DestIP  L4Protocol    AllExtIPs
====================================  =======================  =======================  =====================  ===============  ===============  ==================================  =======  ========  ============  =============
68a5a31d-7516-4c54-ad27-3b1360ce0b56  2019-02-12 14:22:40.697  2019-02-12 13:00:07.000  ibmkajbmepnmiaeilfofa  fmlmbnlpdcbnbnn  149.172.239.103  ['62.100.208.57']                       nan       nan  T             62.100.208.57
68a5a31d-7516-4c54-ad27-3b1360ce0b56  2019-02-12 14:22:40.681  2019-02-12 13:00:48.000  ibmkajbmepnmiaeilfofa  fmlmbnlpdcbnbnn  149.172.239.103  ['156.64.40.139', '156.64.40.236']      nan       nan  T             156.64.40.236
68a5a31d-7516-4c54-ad27-3b1360ce0b56  2019-02-12 14:22:40.681  2019-02-12 13:00:48.000  ibmkajbmepnmiaeilfofa  fmlmbnlpdcbnbnn  149.172.239.103  ['156.64.40.139', '156.64.40.236']      nan       nan  T             156.64.40.139
====================================  =======================  =======================  =====================  ===============  ===============  ==================================  =======  ========  ============  =============


.. _creating_custom_mappings:

Creating custom mappings
------------------------

A custom mapping dictionary has entries in the following form:

::

       "ColumnName": "operation"

The *operation* defines the type of obfuscation method used for that
column. Both the column and the operation code must be quoted.

============== ====================
operation code obfuscation function
============== ====================
“uuid”         replace_guid
“ip”           hash_ip
“str”          hash_string
“dict”         hash_dict
“list”         hash_list
“sid”          hash_sid
“null”         “null”\*
None           hash_str\*
delims_str     hash_item\*
============== ====================

\*The last three items require some explanation:

- null - the *null* operation code means set the value to empty -
  i.e. delete the value in the output frame.
- None (i.e. the dictionary value is *None*) default
  to hash_string.
- *delims_str* - any string other than those named above
  is assumed to be a string of delimiters.

See next section for a discussion of use of delimiters.


.. note:: If you want to *only* use custom mappings and ignore the
   builtin mapping table, specify *use_default=False* as a parameter
   to either *mp_obf.obfuscate()* or *obfuscate_df*.


Using *hash_item* to preserve the structure/look of the hashed input
--------------------------------------------------------------------

Using hash_item with a delimiters string lets you create output that
reflects the structure of the input. The delimiters string is specified as
a simple string of delimiter characters, e.g. *"@\,-"*

The input string is broken into substrings using each of the delimiters
in the delims_str. The substrings are individually hashed and the
resulting substrings joined together using the original delimiters. The
string is split in the order of the characters in the delims string.

This allows you to create hashed values that bear some resemblance to
the original structure of the string. This might be useful for email
address, qualified domain names and other structure text.

For example : "ian@mydomain.com"

Using the simple *hash_string* function the output bears no
resemblance to an email address

.. code:: ipython3

    hash_string("ian@mydomain.com")


.. parsed-literal::

    'prqocjmdpbodrafn'



Using *hash_item* and specifying the expected delimiters we get
something like an email address in the output.

.. code:: ipython3

    hash_item("ian@mydomain.com", "@.")



.. parsed-literal::

    'bnm@blbbrfbk.pjb'



You use *hash_item* in your Custom Mapping dictionary by specifying a
delimiters string as the *operation*.

