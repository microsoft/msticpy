# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Uses the Azure Python SDK to interact with Azure Blob Storage."""
from typing import Dict, List, Optional
from uuid import uuid4

import pandas as pd
from azure.common.exceptions import CloudError
import json
import os
import nbformat
import requests
import uuid
from azure.storage.blob import generate_blob_sas, BlobServiceClient 
import datetime
from ..common.azure_auth import az_connect
from .azure_data import AzureData
from ..common.azure_auth_core import AzCredentials


_PATH_MAPPING = {
    "ops_path": "/providers/Microsoft.SecurityInsights/operations",
    "alert_rules": "/providers/Microsoft.SecurityInsights/alertRules",
    "ss_path": "/savedSearches",
    "bookmarks": "/providers/Microsoft.SecurityInsights/bookmarks",
    "incidents": "/providers/Microsoft.SecurityInsights/incidents",
}


class AzureBlobStorage():
    """Class for interacting with Azure Blob Storage."""

    def __init__(self, abs_site: str, connect: bool = False):
        """Initialize connector for Azure Python SDK."""
        self.connected = False
        self.abs_site = f"{abs_site}.blob.core.windows.net"
        self.credentials: Optional[AzCredentials] = None
        self.abs_client: Optional[BlobServiceClient] = None
        if connect is True:
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
        self._check_client("sub_client")
        self.abs_client = BlobServiceClient(self.abs_site, self.credentials.modern)
        if not self.abs_client:
            raise CloudError("Could not create a Blob Storage client.")
        self.connected = True



        blob_service_client = BlobServiceClient(f"{abs_site}.blob.core.windows.net", creds.modern)
        blob_client = blob_service_client.get_blob_client(container=abs_container, blob=notebook_name)
        blob_client.upload_blob(notebook, overwrite=True)
        incident_id = path.name.split('.')[0]
        start = datetime.datetime.now()
        end = start + datetime.timedelta(days=7)
        key = blob_service_client.get_user_delegation_key(start,end)
        sast= generate_blob_sas(abs_site, abs_container, notebook_name, user_delegation_key=key, permission='r', expiry=end, start=start)
        path = f"https://{abs_site}.blob.core.windows.net/{abs_container}/{notebook_name}?{sast}"
        write_to_incident(incident_id, path)
        update_incident(incident_id)