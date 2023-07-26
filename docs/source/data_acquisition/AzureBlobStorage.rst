Reading from and writing to Azure Blob AzureBlobStorage
=======================================================

Description
-----------

Azure Blob Storage provides a simple and flexible way to store and access data of any kind.
This makes it ideal for storing a range of data relating to security investigations, whether
it be raw data to analyze or to store outputs and findings.

This class wraps the `Azure Python SDK <https://github.com/Azure/azure-sdk-for-python>`_
and integrates it with other MSTICpy features.


Import the module
-----------------

.. code:: ipython3

    from msticpy.data.storage import AzureBlobStorage

See :py:mod:`azure_blob_storage<msticpy.data.storage.azure_blob_storage>` for API details.

Initialize  the class and connect
---------------------------------

Azure Blob Storage works on the basis of accounts, these are top level
objects under which everything sits. When initializing ``AzureBlobStorage``
you need to provide the name of the account you wish to interact with.
You then need to authenticate with the ``connect`` function.
Authentication uses the ``az_connect`` feature of
MSTICpy and the authentication methods can be customized by passing them
to ``connect`` with the ``auth_methods`` keyword.

.. code:: ipython3

    abs = AzureBlobStorage("MyABSAccount")
    abs.connect(auth_methods=["cli"])

For more details on Azure authentication see :doc:`../getting_started/AzureAuthentication`.

List Containers
---------------

``Containers`` returns details on all the containers within an account.

.. code:: ipython3

    abs.containers()

.. raw:: html

    <table border="1" class="dataframe"><thead><tr style="text-align: right;"><th></th><th>name</th><th>last_modified</th><th>etag</th><th>public_access</th><th>has_immutability_policy</th><th>deleted</th><th>version</th><th>has_legal_hold</th><th>metadata</th></tr></thead><tbody><tr><th>0</th><td>papermill</td><td>2020-11-06 21:53:33+00:00</td><td>"0x8D8829E684FCAA2"</td><td>None</td><td>False</td><td>None</td><td>None</td><td>False</td><td>None</td></tr><tr><th>1</th><td>testcontainer</td><td>2020-11-19 15:22:38+00:00</td><td>"0x8D88C9EF3328E1F"</td><td>None</td><td>False</td><td>None</td><td>None</td><td>False</td><td>None</td></tr></tbody></table>

See :py:mod:`containers<msticpy.data.storage.azure_blob_storage.AzureBlobStorage.containers>` for API details.

Create a Container
------------------

``create_container`` creates a new container within the account.

.. code:: ipython3

    abs.create_container(conatiner_name="MyNewContainer")

.. raw:: html

    <table border="1" class="dataframe"><thead><tr style="text-align: right;"><th></th><th>name</th><th>last_modified</th><th>etag</th><th>public_access</th><th>has_immutability_policy</th><th>deleted</th><th>version</th><th>has_legal_hold</th></tr></thead><tbody><tr><th>0</th><td>MyNewContainer</td><td>2020-11-25 16:28:54+00:00</td><td>"0x8D8915F336764B3"</td><td>None</td><td>False</td><td>None</td><td>None</td><td>False</td></tr></tbody></table>

See :py:mod:`create_container<msticpy.data.storage.azure_blob_storage.AzureBlobStorage.create_container>` for API details.

List Blobs
----------

``blobs`` returns details on all the blobs in a container, due to the container scope it is required that you pass this function
the name of the container you want to list blobs from.

.. code:: ipython3

    blobs = abs.blobs(container_name="MyNewContainer")
    display(blobs[['name', 'container', 'snapshot', 'blob_type', 'last_modified']])

.. raw:: html

    <table border="1" class="dataframe"><thead><tr style="text-align: right;"><th></th><th>name</th><th>container</th><th>snapshot</th><th>blob_type</th><th>last_modified</th></tr></thead><tbody><tr><th>0</th><td>test-blob</td><td>MyNewContainer</td><td>None</td><td>BlobType.BlockBlob</td><td>2020-11-25 17:26:44+00:00</td></tr></tbody></table>

See :py:mod:`blobs<msticpy.data.storage.azure_blob_storage.AzureBlobStorage.blobs>` for API details.

Write to a Blob
---------------

``upload_to_blob`` writes data to a blob as specified. By default this will overwrite anything in the blob
but you can set ``overwrite=False`` to stop an overwrite if the blob already has contents.
The function returns True if the upload was successful.

.. code:: ipython3

    >abs.upload_to_blob(blob="Here is some test data", container_name="MyNewContainer", blob_name="test-blob")
    True

See :py:mod:`upload_to_blob<msticpy.data.storage.azure_blob_storage.AzureBlobStorage.upload_to_blob>` for API details.

Read from a Blob
----------------

``get_blob`` returns the contents of the specified blob.

.. code:: ipython3

    > blob_contents = abs.get_blob(container_name="MyNewContainer", blob_name="test-blob")
    > print(blob_contents)
    b"Here is some test data"

See :py:mod:`get_blob<msticpy.data.storage.azure_blob_storage.AzureBlobStorage.get_blob>` for API details.

Delete a Blob
-------------

``delete_blob`` deletes a blob. By default this will also delete any blob snapshots.
Returns True if blob is successfully deleted.

.. code:: ipython3

    >abs.delete_blob(container_name="MyNewContainer", blob_name="test-blob")
    True

See :py:mod:`delete_blob<msticpy.data.storage.azure_blob_storage.AzureBlobStorage.delete_blob>` for API details.

Generate a SAS Token for a Blob
-------------------------------

``get_sas_token`` generates a `SAS token <https://docs.microsoft.com/azure/storage/common/storage-sas-overview>`__ for the specified blob.
By default the token generated is valid for read access for 7 days but permissions can be modified with the
``permission`` keyword, and validity time-frame with the ``start`` and ``end`` keywords.
The returned string is a full URI for the blob, with the SAS token appended.

.. code:: ipython3

    >abs.get_sas_token(container_name="MyNewContainer", blob_name="test-blob")
    "https://myabsaccount.blob.core.windows.net/MyNewContainer/test-blob?SASTOKENSTRING

See :py:mod:`get_sas_token<msticpy.data.storage.azure_blob_storage.AzureBlobStorage.get_sas_token>` for API details.
