# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Tests for the Splunk Uploader class."""
from pathlib import Path
from unittest.mock import MagicMock

import pandas as pd
import pytest

from msticpy.common.exceptions import MsticpyConnectionError, MsticpyUserError
from msticpy.data.uploaders.splunk_uploader import SplunkUploader

from ...unit_test_lib import get_test_data_path

_TEST_DATA = get_test_data_path()


def load_index(sel, index_name, create):
    """Return None if magic isn't == kql."""
    del sel, index_name, create
    return _MockSplunkIndex()


class _MockSplunkIndex(MagicMock):
    """Splunk index mock."""

    def submit(self, data, sourcetype, host):  # pylint: disable=no-self-use
        """Mock submit call."""
        del data, sourcetype, host


# pylint: disable=protected-access, redefined-outer-name
@pytest.fixture(scope="module")
def sp_upload():
    """Generate SplunkUploader for testing."""
    SplunkUploader._load_index = load_index
    SplunkUploader._check_index.return_value = True
    sp_upload = SplunkUploader(  # nosec
        host="test",
        username="test",
        password="[PLACEHOLDER]",
        debug=True,
        connect=False,
    )
    sp_upload.connected = True
    return sp_upload


def test_df_upload(sp_upload):
    """Test DataFrame upload."""
    data_file = Path(_TEST_DATA).joinpath("syslog_data.csv")
    data = pd.read_csv(data_file, parse_dates=["TimeGenerated"])
    sp_upload.upload_df(data, index_name="test_upload", table_name="test_upload")


def test_df_failure(sp_upload):
    """Test DataFrame upload failure."""
    with pytest.raises(MsticpyUserError):
        sp_upload.upload_df("123", index_name="test_upload", table_name="test_upload")


def test_file_upload(sp_upload):
    """Test file upload."""
    data_file = Path(_TEST_DATA).joinpath("syslog_data.csv")
    sp_upload.upload_file(data_file, index_name="test_upload", table_name="test_upload")


def test_file_failure(sp_upload):
    """Test file upload failure."""
    data_file = Path(_TEST_DATA).joinpath("win_proc_test.pkl")
    with pytest.raises(MsticpyUserError):
        sp_upload.upload_file(
            data_file, index_name="test_upload", table_name="test_upload"
        )


def test_folder_upload(sp_upload):
    """Test folder upload."""
    data_folder = Path(_TEST_DATA).joinpath("uploader")
    sp_upload.upload_folder(
        data_folder, index_name="test_upload", table_name="test_upload"
    )


def test_folder_upload_no_name(sp_upload):
    """Test folder upload with no table name specified."""
    data_folder = Path(_TEST_DATA).joinpath("uploader")
    sp_upload.upload_folder(data_folder, index_name="test_upload")


def test_not_connected(sp_upload):
    """Test no connection is handled correctly."""
    sp_upload.connected = False
    data_file = Path(_TEST_DATA).joinpath("syslog_data.csv")
    with pytest.raises(MsticpyConnectionError):
        sp_upload.upload_file(
            data_file, index_name="test_upload", table_name="test_upload"
        )
