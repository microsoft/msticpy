# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Notebook tools MSSparkTools fixure."""
from collections import namedtuple
from typing import List

__author__ = "Ian Hellen"

# pylint: disable=redefined-outer-name, invalid-name, too-few-public-methods

MountPointInfo = namedtuple("MountPointInfo", "mountPoint, source, linkedService")
# MountPointInfo(
#   mountPoint=/msticpy,
#   source=abfss://sentinelfiles@mynsynapse.dfs.core.windows.net/,
#   linkedService=my-synapse1-WorkspaceDefaultStorage
# )


class MSSparkUtils:
    """Synapse mssparkutils emulator."""

    def __init__(self, data=None):
        """Initialize sparkutils mock."""
        self.fs = MsstFileSystem(self)
        self.env = MsstEnvironment(self)
        self.credentials = MsstCredentials(self)
        self.data = data or {}


class MsstFileSystem:
    """Mock for fs sub-module."""

    def __init__(self, container):
        """Initialize the sub-component."""
        self.container = container

    def mount(self, source, mountPoint, extraConfigs=None) -> bool:
        """Mock mounting a storage location."""
        if self.container.data.get("mount", False):
            if "mounts" not in self.container.data:
                self.container.data["mounts"] = []
            self.container.data["mounts"].append(
                MountPointInfo(
                    source=source, mountPoint=mountPoint, linkedService=extraConfigs
                )
            )
        return self.container.data.get("mount", False)

    def mounts(self) -> List[MountPointInfo]:
        """Return list of mocked mounts."""
        return self.container.data.get("mounts", [])

    def add_mount(self, mountPoint: str, source: str, linkedService: str):
        """Add mock mount entry."""
        curr_mounts = self.container.data.get("mounts")
        if not curr_mounts:
            self.container.data["mounts"] = []
        self.container.data["mounts"].append(
            MountPointInfo(
                mountPoint=mountPoint, source=source, linkedService=linkedService
            )
        )


class MsstEnvironment:
    """Mock for env submodule."""

    def __init__(self, container):
        """Initialize the sub-component."""
        self.container = container

    def getJobId(self) -> str:
        """Return job ID."""
        return str(self.container.data.get("jobId", 1))

    def getWorkspaceName(self) -> str:
        """Return workspace name."""
        return self.container.data.get("workspace", "default")


class MsstCredentials:
    """Mock for credentials sub-component."""

    def __init__(self, container):
        """Initialize the sub-component."""
        self.container = container

    def getSecret(self, akvName, secret, linkedService=""):
        """Return mocked value from KV."""
        del linkedService
        value = self.container.data.get("secrets", {}).get(secret)
        if not value and akvName:
            value = self.container.data.get("secrets", {}).get(akvName, {}).get(secret)
        return value

    def getToken(self, audience, name=""):
        """Return mocked token."""
        del audience, name
        return "TOKEN"


class Jwt:
    """Mock class for PyJWT."""

    def get_unverified_header(self, token):
        """Return fake token header dict."""
        del token
        return {"alg": "RS256"}

    def decode(self, ws_token, algorithms, options=None):
        """Return decoded token."""
        del ws_token, algorithms, options
        return _JWT_TOKEN


_JWT_TOKEN = {
    "aud": "https://vault.azure.net",
    "iss": "https://sts.windows.net/AAD-GUID/",
    "iat": 1661881736,
    "nbf": 1661881736,
    "exp": 1661968436,
    "aio": "U29tZSBWYWx1ZQ",
    "appid": "App-ID",
    "appidacr": "2",
    "idp": "https://sts.windows.net/AAD-GUID/",
    "oid": "App-OID",
    "rh": "0.U29tZSBWYWx1ZQ-U29tZSBWYWx1ZQ.",
    "sub": "Sub-GUID",
    "tid": "AAD-GUID",
    "uti": "U29tZSBWYWx1ZQ",
    "ver": "1.0",
    "xms_az_tm": "azureinfra",
    "xms_mirid": (
        "/subscriptions/Sub-GUID/resourcegroups/MyRG/"
        "providers/Microsoft.Synapse/workspaces/my-synapse1"
    ),
}


LINKED_SERVICES_RESP = {
    "value": [
        {
            "id": (
                "/subscriptions/UUID/resourceGroups/MyRG/providers/Microsoft.Synapse"
                "/workspaces/my-synapse1/linkedservices/my-synapse1-WorkspaceDefaultStorage"
            ),
            "name": "my-synapse1-WorkspaceDefaultStorage",
            "type": "Microsoft.Synapse/workspaces/linkedservices",
            "etag": "e0029d27-0000-0200-0000-615e09630000",
            "properties": {
                "typeProperties": {"url": "https://mynsynapse.dfs.core.windows.net"},
                "type": "AzureBlobFS",
                "connectVia": {
                    "referenceName": "AutoResolveIntegrationRuntime",
                    "type": "IntegrationRuntimeReference",
                },
            },
        },
        {
            "id": (
                "/subscriptions/UUID/resourceGroups/MyRG/providers/Microsoft.Synapse"
                "/workspaces/my-synapse1/linkedservices/my-synapse1-WorkspaceDefaultSqlServer"
            ),
            "name": "my-synapse1-WorkspaceDefaultSqlServer",
            "type": "Microsoft.Synapse/workspaces/linkedservices",
            "etag": "e0029d28-0000-0200-0000-615e09670000",
            "properties": {
                "typeProperties": {
                    "connectionString": (
                        "Data Source=tcp:my-synapse1.sql.azuresynapse.net,"
                        "1433;Initial Catalog=@{linkedService().DBName}"
                    )
                },
                "parameters": {"DBName": {"type": "String"}},
                "type": "AzureSqlDW",
                "connectVia": {
                    "referenceName": "AutoResolveIntegrationRuntime",
                    "type": "IntegrationRuntimeReference",
                },
            },
        },
        {
            "id": (
                "/subscriptions/UUID/resourceGroups/MyRG/providers/Microsoft.Synapse"
                "/workspaces/my-synapse1/linkedservices/my_synapse_blob"
            ),
            "name": "my_synapse_blob",
            "type": "Microsoft.Synapse/workspaces/linkedservices",
            "etag": "89028550-0000-0200-0000-62d080970000",
            "properties": {
                "annotations": [],
                "type": "AzureBlobStorage",
                "typeProperties": {
                    "connectionString": (
                        "DefaultEndpointsProtocol=https;"
                        "AccountName=mysynapsefs;EndpointSuffix=core.windows.net;"
                    ),
                },
                "connectVia": {
                    "referenceName": "AutoResolveIntegrationRuntime",
                    "type": "IntegrationRuntimeReference",
                },
            },
        },
        {
            "id": (
                "/subscriptions/UUID/resourceGroups/MyRG/providers/Microsoft.Synapse"
                "/workspaces/my-synapse1/linkedservices/my_dls_hs"
            ),
            "name": "my_dls_hs",
            "type": "Microsoft.Synapse/workspaces/linkedservices",
            "etag": "8f024422-0000-0200-0000-62d0a9500000",
            "properties": {
                "annotations": [],
                "type": "AzureBlobFS",
                "typeProperties": {"url": "https://mysynapsedls.dfs.core.windows.net/"},
                "connectVia": {
                    "referenceName": "AutoResolveIntegrationRuntime",
                    "type": "IntegrationRuntimeReference",
                },
            },
        },
        {
            "id": (
                "/subscriptions/UUID/resourceGroups/MyRG/providers/Microsoft.Synapse"
                "/workspaces/my-synapse1/linkedservices/KeyVault1"
            ),
            "name": "KeyVault1",
            "type": "Microsoft.Synapse/workspaces/linkedservices",
            "etag": "2803ab6a-0000-0200-0000-62d5f6480000",
            "properties": {
                "annotations": [],
                "type": "AzureKeyVault",
                "typeProperties": {"baseUrl": "https://my-synapse.vault.azure.net/"},
            },
        },
    ]
}
