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
import sys
from tqdm import tqdm
from pathlib import Path
import pandas as pd

from ...common.exceptions import MsticpyConnectionError

from .uploader_base import UploaderBase
from ..._version import VERSION

# Credits
# https://docs.microsoft.com/en-us/azure/azure-monitor/platform/data-collector-api#python-3-sample
# https://github.com/Cyb3rWard0g/azure-loganalytics-api-clients/blob/master/python/ala-python-data-producer.py


__version__ = VERSION
__author__ = "Pete Bryan"

class LAUploader(UploaderBase):
    """Uploader class for LogAnalytics."""

    def __init__(self, workspace:str, workspace_secret:str, **kwargs):
        """Initialize a LogAnalytics Uploader instance."""
        self._kwargs = kwargs
        self.workspace = workspace
        self.workspace_secret = workspace_secret
        self._debug = kwargs.get("debug", False)
        self.ops_loc = kwargs.get("opsinsight_loc", ".ods.opinsights.azure.com")

    def _build_signature(self, date, content_length, method, content_type, resource) -> str:
        x_headers = 'x-ms-date:' + date
        string_to_hash = method + "\n" + str(content_length) + "\n" + content_type + "\n" + x_headers + "\n" + resource
        bytes_to_hash = bytes(string_to_hash, encoding="utf-8") 
        decoded_key = base64.b64decode(self.workspace_secret)
        encoded_hash = base64.b64encode(hmac.new(decoded_key, bytes_to_hash, digestmod=hashlib.sha256).digest()).decode()
        authorization = f"SharedKey {self.workspace}:{encoded_hash}"
        if self._debug is True:
            print(authorization)
        return authorization

    def _post_data(self, body, table_name):
        resource = '/api/logs'
        content_type = 'application/json'
        rfc1123date = datetime.datetime.utcnow().strftime('%a, %d %b %Y %H:%M:%S GMT')
        content_length = len(body)
        signature = self._build_signature(rfc1123date, content_length, 'POST', content_type, resource)
        uri = 'https://' + self.workspace + self.ops_loc + resource + '?api-version=2016-04-01'
        headers = {
            'content-type': content_type,
            'Authorization': signature,
            'Log-Type': table_name,
            'x-ms-date': rfc1123date
        }
        response = requests.post(uri,data=body, headers=headers)
        if self._debug is True:
            print(response.status_code)
        if (response.status_code < 200 or response.status_code > 299):
            raise MsticpyConnectionError("LogAnalytics data upload failed, check Workspace ID and key")

    def upload_df(self, data:pd.DataFrame, table_name:str):
        events = []
        for row in data.iterrows():
            events.append(row[1].astype(str).to_dict())
            # Due to 30MB limit if data is larger than 25Mb upload that chunk then continue
            if sys.getsizeof(json.dumps(events)) > 26214400:
                if self._debug is True:
                    print("Data larger than 25MB spliting data requests.")
                body = json.dumps(events)
                self._post_data(body, table_name)
                events = []

        if events:
            body = json.dumps(events)
            self._post_data(body, table_name)
    
    def file_upload(self, file_path:str, table_name:str, delim:str = ',',):
        file = Path(path)
        data = pd.read_csv(path, delim=delim)
        self.upload_df(data, table_name)

    def folder_upload(self, folder_path:str, table_name:str = None, delim:str = ','):
        if delim != ",":
            ext = "*"
        else:
            ext = "*.csv"
        input_files = Path(in_dir).glob(ext)
        progress = tqdm(total=len(list(input_files)), desc='Files', position=0)
        for file in list(input_files):
            data = pd.read_csv(file, delimiter=delim)
            if not table_name:
                table_name = str(file).split("\\")[-1].split(".")[0]
            self.upload_df(data, table_name)
            progress.update(1)
        progress.close()