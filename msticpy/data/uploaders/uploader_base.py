# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Data uploader base class."""
import abc
from abc import ABC

import pandas as pd

from ..._version import VERSION
from ...common.pkg_config import get_http_timeout

__version__ = VERSION
__author__ = "Pete Bryan"


class UploaderBase(ABC):
    """Base class for data providers."""

    def __init__(self, **kwargs):
        """Initialize new instance."""
        self._kwargs = kwargs
        self.workspace = None
        self.workspace_secret = None
        self._connected = False
        self._debug = False

    @abc.abstractmethod
    def upload_file(self, file_path: str, table_name: str, delim: str = ",", **kwargs):
        """
        Upload a file to the data store.

        Parameters
        ----------
        file_path : str
            Path to the file to upload
        table_name : str
            The name of the table to upload the file to
        delim : Optional[str]
            Column deliminator in data file, default is ,

        """

    @abc.abstractmethod
    def upload_folder(
        self, folder_path: str, table_name: str = None, delim: str = ",", **kwargs
    ):
        """
        Upload a folder of files to the data store.

        Parameters
        ----------
        folder_path : str
            Path to the folder of files to upload
        table_name : Optional[str]
            The name of the table to upload the file to, if not set file name is used as table name
        delim : Optional[str]
            Column deliminator in data file, default is ,

        """

    @abc.abstractmethod
    def upload_df(self, data: pd.DataFrame, table_name: str, **kwargs):
        """
        Upload a Pandas DataFrame to the data store.

        Parameters
        ----------
        data : pd.DataFrame
            The DataFrame to upload
        table_name : str
            The name of the table to upload the DataFrame to

        """

    @staticmethod
    def get_http_timeout(**kwargs):
        """Get http timeout from settings or kwargs."""
        return get_http_timeout(**kwargs)
