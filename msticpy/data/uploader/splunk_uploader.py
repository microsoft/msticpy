# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""LogAnayltics Uploader class."""
import json
import requests
import datetime
import hashlib
import hmac
import base64
from pathlib import Path
import pandas as pd
import splunklib.client
import splunklib.results

from .uploader_base import UploaderBase
from ..._version import VERSION

# https://docs.microsoft.com/en-us/azure/azure-monitor/platform/data-collector-api#python-3-sample
# https://github.com/Cyb3rWard0g/azure-loganalytics-api-clients/blob/master/python/ala-python-data-producer.py


__version__ = VERSION
__author__ = "Pete Bryan"

class LAUploader(UploaderBase):
    """Uploader class for LogAnalytics."""

    def __init__(self, username:str, workspace:str, workspace_secret:str, **kwargs):
        """Initialize a LogAnalytics Uploader instance."""
        self._kwargs = kwargs
        self.workspace = workspace
        self.workspace_secret = workspace_secret
        self.user = username
        self.service = None
        self.index = None

    def connect(self):
        # Handle auth failure
        self.service = splunklib.client.connect(host=self.workspace, username=self.user, password=self.workspace_secret)
        self.connected = True

    def _post_data(self, data:pd.DataFrame, index_name:str, table_name:str, host:str = "Upload"):
        index = load_index(index_name)
        for row in data.iterows():
            data = row[1].to_csv()
            index.submit(data, sourcetype=table_name, host=host)
    
    # Allow passing through host value
    def upload_df(self, data:pd.DataFrame, table_name:str, index_name:str):
        self._post_data(data, table_name, index_name)
    
    # Allow passing through host value
    def upload_file(self, file_path:str, table_name:str, delim:str = ','):
        file = Path(path)
        data = pd.read_csv(path, delim=delim)
        self._post_data(data, table_name, index_name)
    
    # Allow passing through host value
    def upload_folder(self, folder_path:str, table_name:str = None, delim:str = ',', index_name):
        if delim != ",":
            ext = "*"
        else:
            ext = "*.csv"
        input_files = Path(in_dir).glob(ext)
        for file in list(input_files):
            data = pd.read_csv(file, delimiter=delim)
            if not table_name:
                table_name = str(file).split("\\")[-1].split(".")[0]
            self._post_data(data, table_name, index_name)

    def check_index(self, index_name:str):
        service_list = [item.name for item in service.indexes]
        if index_name in service_list:
            return True
        else: 
            return False

    def load_index(self, index_name):
        if check_index(index_name) is False:
            return self.service.indexes.create(index_name)
        else:
            return self.service.indexes[index_name]

