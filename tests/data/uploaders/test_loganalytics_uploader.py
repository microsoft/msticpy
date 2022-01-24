# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Tests for the LogAnlaytics Uploader class."""

from pathlib import Path
from unittest.mock import patch
import pytest

from httpx import Response
import pandas as pd

from msticpy.data.uploaders.loganalytics_uploader import LAUploader
from msticpy.common.exceptions import MsticpyConnectionError

from ...unit_test_lib import get_test_data_path

_TEST_DATA = get_test_data_path()


# pylint: disable=protected-access, redefined-outer-name
@pytest.fixture(scope="module")
@patch("httpx.post")
def la_uploader(mock_put):
    """Generate LAUploader for testing."""
    response = Response(200)
    mock_put.return_value = response
    la_uploader = LAUploader(workspace="1234", workspace_secret="password", debug=True)
    return la_uploader


@patch("httpx.post")
def test_df_upload(mock_put, la_uploader):
    """Check DataFrame upload."""
    response = Response(200)
    mock_put.return_value = response
    data_path = Path(_TEST_DATA).joinpath("syslog_data.csv")
    data = pd.read_csv(data_path)
    la_uploader.upload_df(data, "test")


@patch("httpx.post")
def test_file_upload(mock_put, la_uploader):
    """Check file upload."""
    response = Response(200)
    mock_put.return_value = response
    data_path = Path(_TEST_DATA).joinpath("syslog_data.csv")
    la_uploader.upload_file(data_path, "test")


@patch("httpx.post")
def test_folder_upload(mock_put, la_uploader):
    """Check folder upload."""
    response = Response(200)
    mock_put.return_value = response
    data_path = Path(_TEST_DATA).joinpath("uploader")
    la_uploader.upload_folder(data_path, "test")
    la_uploader.upload_folder(data_path)


@patch("httpx.post")
def test_upload_fails(mock_put, la_uploader):
    """Check upload failure."""
    response = Response(503)
    mock_put.return_value = response
    data_path = Path(_TEST_DATA).joinpath("syslog_data.csv")
    data = pd.read_csv(data_path)
    with pytest.raises(MsticpyConnectionError) as err:
        la_uploader.upload_df(data, "test")
        assert "LogAnalytics data upload failed with code 503" in str(err.value)
