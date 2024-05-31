# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Splunk Uploader class."""
import logging
from pathlib import Path
from typing import Any, Optional

import pandas as pd
from pandas.errors import ParserError
from tqdm.notebook import tqdm

from ..._version import VERSION
from ...common.exceptions import MsticpyConnectionError, MsticpyUserError
from ..drivers.splunk_driver import SplunkDriver
from .uploader_base import UploaderBase

__version__ = VERSION
__author__ = "Pete Bryan, Tatsuya Hasegawa"

logger = logging.getLogger(__name__)


class SplunkUploader(UploaderBase):
    """Uploader class for Splunk."""

    def __init__(self, **kwargs):
        """Initialize a Splunk Uploader instance."""
        super().__init__()
        self._kwargs = kwargs
        self.host = kwargs.get("host")
        self.password = kwargs.get("password")
        self.username = kwargs.get("username")
        self.bearer_token = kwargs.get("bearer_token")
        self.driver = SplunkDriver()
        self.port = kwargs.get("port", 8089)
        self._debug = kwargs.get("debug", False)
        self._connect = kwargs.get("connect", True)
        self.connected = False
        if self._connect:
            self.connect()

    def connect(self):
        """Connect to Splunk host."""
        if self._kwargs:
            if self.username:
                self.driver.connect(
                    host=self.host,
                    username=self.username,
                    password=self.password,
                    port=self.port,
                )
            elif self.bearer_token:
                self.driver.connect(
                    host=self.host,
                    bearer_token=self.bearer_token,
                    port=self.port,
                )
            else:
                raise MsticpyConnectionError(
                    "One of these parameters must be supplied.",
                    title="Both 'username' and 'bearer_token' are missing.",
                )
        else:
            self.driver.connect()

        self.connected = True

    def _post_data(
        self,
        data: pd.DataFrame,
        index_name: str,
        source_type: Any,
        host: str = None,
        **kwargs,
    ):
        """
        Write data to the Splunk instance connected to.

        Parameters
        ----------
        data : pd.DataFrame
            Data to upload.
        index_name : str
            Name of the Splunk Index to add data to.
        source_type : str
            The sourcetype in Splunk data will be uploaded to.
            csv, json or other can be input and then passed to
            df.to_csv(), df.to_json(), df.to_string() styles respectively.
        host : str, optional
            The hostname associated with the uploaded data, by default "msticpy_splunk_uploader".

        """
        if not self.connected:
            raise MsticpyConnectionError(
                "Splunk host not connected, please call .connect before proceeding.",
                title="Splunk host not connected",
            )
        if not host:
            host = "msticpy_splunk_uploader"
        index = self._load_index(index_name, kwargs.get("create_index", False))
        progress = tqdm(total=len(data.index), desc="Rows", position=0)
        source_types = []
        for row in data.iterrows():
            if source_type == "json":
                data = row[1].to_json()  # type: ignore
            elif source_type == "csv":
                data = row[1].to_csv()  # type: ignore
            else:
                data = row[1].to_string()  # type: ignore
            try:
                data.encode(encoding="latin-1")  # type: ignore
            except UnicodeEncodeError:
                data = data.encode(encoding="utf-8")  # type: ignore
            index.submit(data, sourcetype=source_type, host=host)
            source_types.append(source_type)
            progress.update(1)
        progress.close()
        if self._debug is True:
            print(f"Upload complete: Splunk sourcetype = {source_types}")

    # pylint: disable=arguments-differ
    def upload_df(  # type: ignore
        self,
        data: pd.DataFrame,
        table_name: Optional[str] = None,
        index_name: Optional[str] = None,
        create_index: bool = False,
        source_type: Optional[str] = None,
        **kwargs,
    ):
        """
        Upload a Pandas DataFrame to Splunk.

        Parameters
        ----------
        data : pd.DataFrame
            Data to upload.
        source_type : str, optional
            The sourcetype in Splunk data will be uploaded to.
            csv, json or other can be input and then passed to
            df.to_csv(), df.to_json(), df.to_string() styles respectively.
            "json" is by default.
        table_name: str, optional
            The backward compatibility of source_type.
        index_name : str
            Name of the Splunk Index to add data to.
        host : str, optional
            Host name to upload data with, default will be 'Upload'
        create_index : bool, optional
            Set this to true to create the index if it doesn't already exist. Default is False.

        """
        if not source_type:
            if not table_name:
                source_type = "json"
            else:
                source_type = table_name

        host = kwargs.get("host", None)
        if not index_name:
            raise ValueError("parameter `index_name` must be specified")
        if not isinstance(data, pd.DataFrame):
            raise MsticpyUserError(
                "Data must be in Pandas DataFrame format.",
                title="incorrect data format",
            )
        self._post_data(
            data=data,
            index_name=index_name,
            host=host,
            source_type=source_type,
            create_index=create_index,
        )

    def upload_file(  # type: ignore
        self,
        file_path: str,
        table_name: Optional[str] = None,
        delim: str = ",",
        index_name: Optional[str] = None,
        create_index: bool = False,
        source_type: Optional[str] = None,
        **kwargs,
    ):
        """
        Upload a seperated value file to Splunk.

        Parameters
        ----------
        file_path : str
            Path to the file to upload.
        index_name : str
            Name of the Splunk Index to add data to.
        source_name : str, optional
            The sourcetype in Splunk data will be uploaded to.
            csv, json or other can be input and then passed to
            df.to_csv(), df.to_json(), df.to_string() styles respectively.
            If not set the file name will be used.
            "json" is by default.
        table_name: str, optional
            The backward compatibility of source_type.
        delim : str, optional
            Seperator value in file, by default ","
        host : str, optional
            Host name to upload data with, default will be 'Upload'
        create_index : bool, optional
            Set this to true to create the index if it doesn't already exist. Default is False.

        """
        host = kwargs.get("host", None)
        if not index_name:
            raise ValueError("parameter `index_name` must be specified")
        path = Path(file_path)
        try:
            data = pd.read_csv(path, delimiter=delim)
        except (ParserError, UnicodeDecodeError) as parse_err:
            raise MsticpyUserError(
                "The file specified is not a seperated value file.",
                "Incorrect file type.",
            ) from parse_err

        if not source_type:
            if table_name:
                source_type = table_name
            else:
                source_type = path.stem

        self._post_data(
            data=data,
            index_name=index_name,
            host=host,
            source_type=source_type,
            create_index=create_index,
        )

    def upload_folder(  # type: ignore
        self,
        folder_path: str,
        table_name: Optional[str] = None,
        delim: str = ",",
        index_name: Optional[str] = None,
        create_index=False,
        source_type: Optional[str] = None,
        **kwargs,
    ):
        """
        Upload all files in a folder to Splunk.

        Parameters
        ----------
        folder_path : str
            Path to folder to upload.
        index_name : str
            Name of the Splunk Index to add data to, if it doesn't exist it will be created.
        source_type : str, optional
            The sourcetype in Splunk data will be uploaded to.
            csv, json or other can be input and then passed to
            df.to_csv(), df.to_json(), df.to_string() styles respectively.
            If not set the file name will be used. "json" is by default.
        table_name: str, optional
            The backward compatibility of source_type.
        delim : str, optional
            Seperator value in files, by default ","
        host : str, optional
            Host name to upload data with, default will be 'Upload'
        create_index : bool, optional
            Set this to true to create the index if it doesn't already exist. Default is False.

        """
        host = kwargs.get("host", None)
        glob_pat = kwargs.get("glob", "*")
        if not index_name:
            raise ValueError("parameter `index_name` must be specified")
        input_files = Path(folder_path).glob(glob_pat)
        f_progress = tqdm(total=len(list(input_files)), desc="Files", position=0)
        for path in input_files:
            try:
                data = pd.read_csv(path, delimiter=delim)
            except (ParserError, UnicodeDecodeError) as parse_err:
                raise MsticpyUserError(
                    "The file specified is not a seperated value file.",
                    title="Incorrect file type.",
                ) from parse_err

            if not source_type:
                if table_name:
                    source_type = table_name
                else:
                    source_type = path.stem

            self._post_data(
                data=data,
                index_name=index_name,
                host=host,
                source_type=source_type,
                create_index=create_index,
            )
            f_progress.update(1)
            if self._debug is True:
                print(
                    f"{str(path)} uploaded to \
                    index={index_name} \
                    source_type={source_type} \
                    host={host}"
                )
        f_progress.close()

    # pylint: enable=arguments-differ

    def _check_index(self, index_name: str):
        """Check if index exists in Splunk host."""
        service_list = [item.name for item in self.driver.service.indexes]
        if index_name in service_list:
            return True
        return False

    def _load_index(self, index_name, create: bool = True):
        """Load specified Index or create if it doesn't exist."""
        if self._check_index(index_name):
            return self.driver.service.indexes[index_name]
        if not self._check_index(index_name) and create:
            return self.driver.service.indexes.create(index_name)

        raise MsticpyConnectionError("Index not present in Splunk host.")
