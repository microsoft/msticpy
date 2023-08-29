Data Uploaders
==============

As well as retrieving data from a data source you may wish to upload a data set to a data source.
This may be a local data file you want to add to you centralized data source or they may be findings
from your investigation that you want to store long term.
MSTICpy contains data uploader functions for both Azure Sentinel/Log Analytics, and Splunk data sources.
Data can be provided to both uploaders as a Pandas DataFrame, value separated file (e.g. csv, tsv),
or a folder path of value separated files.

Uploading data to Azure Sentinel/Log Analytics
----------------------------------------------

Instantiating the Azure Sentinel uploader
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The first step in uploading data is to instantiate an uploader for the location we wish to upload data to.
For Azure Sentinel there are two parameters that need to be passed at this stage,
the workspace ID of the workspace to upload data to, and the workspace key.

**Note that these are different from the details required to query data from Log Analytics using the DataProvider.
Your workspace key can be found under the Advanced setting tab of your Log Analytics workspace.**

.. code:: ipython3

	from msticpy.data.uploaders.loganalytics_uploader import LAUploader
	laup = LAUploader(workspace=WORKSPACE_ID, workspace_secret=WORKSPACE_KEY)

You can also set a ``debug`` flag when instantiating which will provide additional progress messages during an upload process.

Uploading a DataFrame to Azure Sentinel
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

To upload a Pandas DataFrame to Log Analytics you simply pass the DataFrame to ``.upload_df()`` along with the name of a table
you wish the data to be uploaded to. If that table exists the data will be appended to it, alternatively the table will be created.
Note that all tables fall under the Custom Log category so any name you provide will be appended with _CL (i.e. table_name will be table_name_CL).
Log Analytics will parse each column in the DataFrame into a column in the resulting table.

*Note: table_name cannot contain any special characters except `_` all other characters will be removed.*

.. code:: ipython3

	laup.upload_df(data=DATAFRAME, table_name=TABLE_NAME)

Uploading a File to Azure Sentinel
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

To upload a file to Log Analytics pass the path to the file to ``.upload_file()``. By default a comma separated
value file is expected but if you have some other separator value you can pass this with the ``delim`` parameter.
You can specify a table name to upload the data to with that ``table_name`` parameter but by default the uploader
will upload to a table with the same name as the file.

.. code:: ipython3

	laup.upload_file(file_path=FILE_PATH)

Uploading a Folder to Azure Sentinel
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

You can also upload a whole folder of files. To do this simply pass the folder path to ``.upload_folder()``.
By default this will upload all csv files in that folder to the Log Analytics workspace, with each file being
uploaded to a table with a name corresponding to the file name. Alternatively you can also specify single a table
name under which all files will be uploaded. If you have some other separated value file type you can pass ``delim``,
and the specified delimiter value, however currently there is only support for a single delim type across files.
By default this method attempts to upload all files in the specified folders, if you want to only process certain file
extensions you can pass the ``glob`` keyword parameter with the a pattern for files to attempt to upload. The
pattern format required follows the ``pathlib.glob()`` pattern - more details are avaliable `here <"https://docs.python.org/3/library/pathlib.html#pathlib.Path.glob>`_

.. code:: ipython3

	laup.upload_folder(folder_path=FOLDER_PATH, glob="*.csv")

During upload a progress bar will be displayed showing the upload process of the files within the folder.

Uploading data to Splunk
------------------------

Instantiating the Splunk uploader
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The first step in uploading data is to instantiate an uploader for the location we wish to upload data to.
For Splunk there are three parameters that need to be passed at this stage, the Splunk host name, a username,
and a password. You can also pass a parameter for ``port``, by default this value is 8089.

.. code:: ipython3

	from msticpy.data.uploaders.splunk_uploader import SplunkUploader
	spup = SplunkUploader(username=USERNAME, host=HOST, password=PASSWORD)

You can also set a ``debug`` flag when instantiating which will provide additional progress messages during an upload process.

*Note: Due to the way Splunk API's work the time taken to upload a file to Splunk can be significantly longer than
with Log Analytics.*

Uploading a DataFrame to Splunk
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

To upload a Pandas DataFrame to Splunk you simply pass the DataFrame to ``.upload_df()`` along with the name of a table,
and index you wish the data to be uploaded to. If the index provided does not exist and you want it to be created,
you can pass the parameter ``create_index = True``.

.. Note – table name for Splunk refers to sourcetype.

.. code:: ipython3

	spup.upload_df(data=DATAFRAME, table_name=TABLE_NAME, index_name=INDEX_NAME)

During upload a progress bar will be shown showing the upload process of the upload.

Uploading a File to Splunk
^^^^^^^^^^^^^^^^^^^^^^^^^^

To upload a file to Splunk pass the path to the file to ``.upload_file()`` along with the name of the index you
want the data uploaded to. By default a comma separated value file is expected but if you have some other separator
value you can pass this with the ``delim`` parameter. You can specify a table name to upload the data to with that ``table_name``
parameter but by default the uploader will upload to a table with the same name as the file. As with uploading a DataFrame
if the index provided does not exist and you want it to be created, you can pass the parameter ``create_index = True``.

.. code:: ipython3

	spup.upload_file(file_path=FILE_PATH, index_name=INDEX_NAME)

Uploading a Folder to Splunk
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

You can also upload a whole folder of files. To do this simply pass the folder path to ``.upload_folder()`` along with the
name of the index you want the data uploaded to. By default this will upload all csv files in that folder to Splunk,
with each file being uploaded to a sourcetype with a name corresponding to the file name. Alternatively you can also
specify single a table sourcetype which all files will be uploaded with the ``table_name`` parameter. If you have some
other separated value file type you can pass ``delim``, and the specified delimiter value, however currently there is
only support for a single delim type across files. By default this method attempts to upload all files in the specified
folders, if you want to only process certain file extensions you can pass the ``glob`` keyword parameter with the a pattern
for files to attempt to upload. The pattern format required follows the ``pathlib.glob()`` pattern - more details are
avaliable `here <"https://docs.python.org/3/library/pathlib.html#pathlib.Path.glob>`_
As with the other methods if the index provided does not exist and you want it to be created, you can pass the parameter ``create_index = True``.

.. code:: ipython3

	spup.upload_folder(folder_path=FOLDER_PATH, index_name=INDEX_NAME)

During upload a progress bar will be shown showing the upload process of the files within the folder.
