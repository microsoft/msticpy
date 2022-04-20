# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Uses the Azure Python SDK to interact with Azure Blob Storage."""
import datetime
from typing import Any, List, Optional

import pandas as pd
from azure.common.exceptions import CloudError
from azure.core.exceptions import (
    ResourceExistsError,
    ResourceNotFoundError,
    ServiceRequestError,
)
from azure.storage.blob import BlobServiceClient, generate_blob_sas

from ..._version import VERSION
from ...auth.azure_auth import az_connect
from ...auth.azure_auth_core import AzCredentials, AzureCloudConfig

__version__ = VERSION
__author__ = "Pete Bryan"


class AzureBlobStorage:
    """Class for interacting with Azure Blob Storage."""

    def __init__(
        self,
        abs_name: str = None,
        connect: bool = False,
        abs_connection_string: str = None,
    ):
        """Initialize connector for Azure Python SDK."""
        self.connected = False
        self.abs_site = f"{abs_name}.blob.core.windows.net"
        self.connection_string = abs_connection_string
        self.credentials: Optional[AzCredentials] = None
        self.abs_client: Optional[BlobServiceClient] = None
        if connect:
            self.connect()

    def connect(
        self,
        auth_methods: List = None,
        silent: bool = False,
    ):
        """Authenticate with the SDK."""
        self.credentials = az_connect(auth_methods=auth_methods, silent=silent)
        if not self.credentials:
            raise CloudError("Could not obtain credentials.")
        if not self.connection_string:
            self.abs_client = BlobServiceClient(self.abs_site, self.credentials.modern)
        else:
            self.abs_client = BlobServiceClient.from_connection_string(
                self.connection_string
            )
        if not self.abs_client:
            raise CloudError("Could not create a Blob Storage client.")
        self.connected = True

    def containers(self) -> pd.DataFrame:
        """Return containers in the Azure Blob Storage Account."""
        try:
            container_list = self.abs_client.list_containers()  # type:ignore
        except ServiceRequestError as err:
            raise CloudError(
                "Unable to connect check the Azure Blob Store account name"
            ) from err
        return (
            _parse_returned_items(  # type:ignore
                container_list, remove_list=["lease", "encryption_scope"]
            )
            if container_list
            else None
        )

    def create_container(self, container_name: str, **kwargs) -> pd.DataFrame:
        """
        Create a new container within the Azure Blob Storage account.

        Parameters
        ----------
        container_name : str
            The name for the new container.
        Additional container parameters can be passed as kwargs

        Returns
        -------
        pd.DataFrame
            Details of the created container.

        """
        try:
            new_container = self.abs_client.create_container(  # type: ignore
                container_name, **kwargs
            )  # type:ignore
        except ResourceExistsError as err:
            raise CloudError(f"Container {container_name} already exists.") from err
        properties = new_container.get_container_properties()
        return _parse_returned_items([properties], ["encryption_scope", "lease"])

    def blobs(self, container_name: str) -> Optional[pd.DataFrame]:
        """
        Get a list of blobs in a container.

        Parameters
        ----------
        container_name : str
            The name of the container to get blobs from.

        Returns
        -------
        pd.DataFrame
            Details of the blobs.

        """
        container_client = self.abs_client.get_container_client(container_name)  # type: ignore
        blobs = list(container_client.list_blobs())
        return _parse_returned_items(blobs) if blobs else None

    def upload_to_blob(
        self, blob: Any, container_name: str, blob_name: str, overwrite: bool = True
    ):
        """
        Upload a blob of data.

        Parameters
        ----------
        blob : Any
            The data to upload.
        container_name : str
            The name of the container to upload the blob to.
        blob_name : str
            The name to give the blob.
        overwrite : bool, optional
            Whether or not you want to overwrite the blob if it exists, by default True.

        """
        try:
            blob_client = self.abs_client.get_blob_client(  # type:ignore
                container=container_name, blob=blob_name
            )
            upload = blob_client.upload_blob(blob, overwrite=overwrite)
        except ResourceNotFoundError as err:
            raise CloudError(
                "Unknown container, check container name or create it first."
            ) from err
        if not upload["error_code"]:
            print("Upload complete")
        else:
            raise CloudError(
                f"There was a problem uploading the blob: {upload['error_code']}"
            )
        return True

    def get_blob(self, container_name: str, blob_name: str) -> bytes:
        """
        Get a blob from the Azure Blob Storage account.

        Parameters
        ----------
        container_name : str
            The name of the container that holds the blob.
        blob_name : str
            The name of the blob to download.

        Returns
        -------
        bytes
            The content of the blob in bytes.

        """
        blob_client = self.abs_client.get_blob_client(  # type: ignore
            container=container_name, blob=blob_name
        )
        if not blob_client.exists():
            raise CloudError(f"The blob {blob_name} does not exist in {container_name}")
        data_stream = blob_client.download_blob()
        return data_stream.content_as_bytes()

    def delete_blob(self, container_name: str, blob_name: str) -> bool:
        """
        Delete a blob from the Azure Blob Storage account.

        Parameters
        ----------
        container_name : str
            The container name that has the blob.
        blob_name : str
            The name of the blob to delete.
        Note deleting a blob also deletes associated snapshots.

        Returns
        -------
        bool
            True if blob successfully deleted

        """
        blob_client = self.abs_client.get_blob_client(  # type: ignore
            container=container_name, blob=blob_name
        )
        if blob_client.exists():
            blob_client.delete_blob(delete_snapshots="include")
        else:
            raise CloudError(f"The blob {blob_name} does not exist in {container_name}")

        return True

    def get_sas_token(
        self,
        container_name: str,
        blob_name: str,
        end: datetime.datetime = None,
        permission: str = "r",
    ) -> str:
        """
        Generate a shared access string (SAS) token for a blob.

        Parameters
        ----------
        container_name : str
            The name of the Azure Blob Storage container that holds the blob.
        blob_name : str
            The name of the blob to generate the SAS token for.
        end : datetime.datetime, optional
            The datetime the SAS token should expire, by default this is 7 days from now.
        permission : str, optional
            The permissions to give the SAS token, by default 'r' for read.

        Returns
        -------
        str
            A URI of the blob with SAS token.

        """
        start = datetime.datetime.now()
        if not end:
            end = start + datetime.timedelta(days=7)
        key = self.abs_client.get_user_delegation_key(start, end)  # type: ignore
        abs_name = self.abs_client.account_name  # type: ignore
        sast = generate_blob_sas(
            abs_name,
            container_name,
            blob_name,
            user_delegation_key=key,
            permission=permission,
            expiry=end,
            start=start,
        )
        suffix = AzureCloudConfig().suffixes.storage_endpoint
        return f"https://{abs_name}.blob.{suffix}/{container_name}/{blob_name}?{sast}"


def _parse_returned_items(items, remove_list: list = None) -> pd.DataFrame:
    """Parse a list of containers into a DataFrame."""
    out_items = []
    for item in items:
        item = dict(item)
        if remove_list:
            for remove_item in remove_list:
                item.pop(remove_item)
        out_items.append(item)
    return pd.json_normalize(out_items)
