# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Azure Sentinel unit tests."""
import datetime
from collections import namedtuple
from unittest.mock import patch

import pandas as pd

from msticpy.data.storage.azure_blob_storage import AzureBlobStorage

_CONTAINERS = [
    {
        "name": "test",
        "last_modified": datetime.datetime(
            2020, 11, 6, 21, 53, 33, tzinfo=datetime.timezone.utc
        ),
        "etag": '"0x00000000000000001"',
        "lease": {"status": "unlocked", "state": "available", "duration": None},
        "public_access": None,
        "has_immutability_policy": False,
        "deleted": None,
        "version": None,
        "has_legal_hold": False,
        "metadata": None,
        "encryption_scope": "Test",
    }
]
_BLOBS = [
    {
        "name": "test.blob",
        "container": "test",
        "snapshot": None,
        "version_id": None,
        "is_current_version": None,
        "blob_type": "test",
        "metadata": {},
        "encrypted_metadata": None,
        "last_modified": datetime.datetime(
            2020, 11, 9, 1, 24, 52, tzinfo=datetime.timezone.utc
        ),
        "etag": "0x0000000000001",
        "size": 38630,
        "content_range": None,
        "append_blob_committed_block_count": None,
        "is_append_blob_sealed": None,
        "page_blob_sequence_number": None,
        "server_encrypted": True,
        "copy": {
            "id": None,
            "source": None,
            "status": None,
            "progress": None,
            "completion_time": None,
            "status_description": None,
            "incremental_copy": None,
            "destination_snapshot": None,
        },
        "content_settings": {
            "content_type": "application/octet-stream",
            "content_encoding": None,
            "content_language": None,
            "content_md5": bytearray(b"test"),
            "content_disposition": None,
            "cache_control": None,
        },
        "lease": {"status": "unlocked", "state": "available", "duration": None},
        "blob_tier": "Hot",
        "rehydrate_priority": None,
        "blob_tier_change_time": None,
        "blob_tier_inferred": True,
        "deleted": None,
        "deleted_time": None,
        "remaining_retention_days": None,
        "creation_time": datetime.datetime(
            2020, 11, 9, 1, 24, 52, tzinfo=datetime.timezone.utc
        ),
        "archive_status": None,
        "encryption_key_sha256": None,
        "encryption_scope": None,
        "request_server_encrypted": None,
        "object_replication_source_properties": [],
        "object_replication_destination_policy": None,
        "tag_count": None,
        "tags": None,
    }
]


class _DummyContainerClient:
    def get_container_properties(self):
        return _CONTAINERS[0]

    def list_blobs(self):
        return _BLOBS


class _DummyBlobClient:
    def upload_blob(self, blob, overwrite):
        return {"error_code": None}

    def exists(self):
        return True

    def download_blob(self):
        return _DummyContent()

    def delete_blob(self, delete_snapshots):
        pass


class _DummyContent:
    def content_as_bytes(self):
        return "test_data"


def test_abs_init():
    """Test class initalization."""
    azs = AzureBlobStorage("Test")
    assert isinstance(azs, AzureBlobStorage)


@patch(AzureBlobStorage.__module__ + ".BlobServiceClient")
@patch(AzureBlobStorage.__module__ + ".az_connect")
def test_abs_containers(mock_creds, mock_abs_client):
    """Test abs container feature."""
    AzCredentials = namedtuple("AzCredentials", ["legacy", "modern"])
    mock_abs_client().list_containers.return_value = _CONTAINERS
    mock_abs_client().create_container.return_value = _DummyContainerClient()
    mock_creds.return_value = AzCredentials("cred", "cred")
    abs = AzureBlobStorage("Test")
    abs.connect()
    containers = abs.containers()
    assert isinstance(containers, pd.DataFrame)
    assert containers.iloc[0]["name"] == "test"
    new_container = abs.create_container("test")
    assert isinstance(new_container, pd.DataFrame)
    assert new_container.iloc[0]["name"] == "test"


@patch(AzureBlobStorage.__module__ + ".BlobServiceClient")
@patch(AzureBlobStorage.__module__ + ".az_connect")
def test_abs_blobs(mock_creds, mock_abs_client):
    """Test abs blob feature."""
    AzCredentials = namedtuple("AzCredentials", ["legacy", "modern"])
    mock_abs_client().get_container_client.return_value = _DummyContainerClient()
    mock_abs_client().get_blob_client.return_value = _DummyBlobClient()
    mock_creds.return_value = AzCredentials("cred", "cred")
    abs = AzureBlobStorage("Test")
    abs.connect()
    blobs = abs.blobs("test")
    assert isinstance(blobs, pd.DataFrame)
    assert blobs.iloc[0]["name"] == "test.blob"
    upload = abs.upload_to_blob(
        "test_data", "test_container", "test_blob", overwrite=False
    )
    assert upload is True
    delete = abs.delete_blob("test_container", "test_blob")
    assert delete is True
    blob_data = abs.get_blob("test_container", "test_blob")
    assert blob_data == "test_data"


@patch(AzureBlobStorage.__module__ + ".BlobServiceClient")
@patch(AzureBlobStorage.__module__ + ".az_connect")
@patch(AzureBlobStorage.__module__ + ".generate_blob_sas")
def test_sas_token_creation(mock_sas_token, mock_creds, mock_abs_client):
    AzCredentials = namedtuple("AzCredentials", ["legacy", "modern"])
    mock_abs_client().get_user_delegation_key.return_value = "Test_Key"
    mock_abs_client().account_name = "test_name"
    mock_sas_token.return_value = "TestSASToken"
    mock_creds.return_value = AzCredentials("cred", "cred")
    abs = AzureBlobStorage("test")
    abs.connect()
    path = abs.get_sas_token("test_container", "test_blob")
    # assert isinstance(str, path)
    assert (
        path
        == "https://test_name.blob.core.windows.net/test_container/test_blob?TestSASToken"
    )
