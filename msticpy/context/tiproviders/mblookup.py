# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# Author: Thomas Roccia - @fr0gger_
# --------------------------------------------------------------------------
"""
MalwareBazaar Provider.

Input can be a hash, tag, signature, filetype, clamav signature, imphash, dhash,
tlsh, telfhash, certificate issuer info, certificate subject info, or certificate.

The class can be used to:
    - Get data about a sample from MalwareBazaar
    - Download malware sample from MalwareBazaar
    - Query Code Signing Certificate Blocklist (CSCB)
    - Get the recent addition from MalwareBazaar

"""
from enum import Enum
from typing import Set

import json
import requests
import pandas as pd

from pandas import json_normalize
from ..._version import VERSION
from ...common.provider_settings import get_provider_settings

__version__ = VERSION
__author__ = "Thomas Roccia | @fr0gger_"

_BASE_URL = "https://mb-api.abuse.ch/api/v1/"

class MBEntityType(Enum):
    """MBEntityType: Enum class for MalwareBazaar entity types."""

    HASH = "hash"
    TAG = "tag"
    SIGNATURE = "signature"
    FILETYPE = "filetype"
    CLAMAV = "clamav"
    IMPHASH = "imphash"
    DHASH = "dhash"
    YARA = "yara"
    TLSH = "tlsh"
    TELFHASH = "telfhash"
    CODESIGNISSUER = "issuerinfo"
    CODESIGNSUBJECT = "subjectinfo"
    CODESIGNSN = "certificate"

class MBlookup:
    """ MBlookup Python Class wrapper for MalwareBazaar API.

        Classes:
            MBLookup: Class for MalwareBazaar API.

        Functions:
            lookup_ioc(self, ioc_type: MBEntityType, ioc_value: str) -> pandas.DataFrame
            download_sample(self, sha2: str) -> str
            get_recent(self, selector: str) -> pandas.DataFrame
            get_cscb(self) -> pandas.DataFrame
    """

    _SUPPORTED_MB_TYPES: Set[MBEntityType] = {
        MBEntityType.HASH,
        MBEntityType.TAG,
        MBEntityType.SIGNATURE,
        MBEntityType.FILETYPE,
        MBEntityType.CLAMAV,
        MBEntityType.IMPHASH,
        MBEntityType.DHASH,
        MBEntityType.YARA,
        MBEntityType.TLSH,
        MBEntityType.TELFHASH,
        MBEntityType.CODESIGNISSUER,
        MBEntityType.CODESIGNSUBJECT,
        MBEntityType.CODESIGNSN,
    }

    def __init__(self, mb_key=None):
        """Init function to get the API key if necessary"""
        self.mb_key = mb_key # or_get_mb_api_key()

    def lookup_ioc(self, observable: str, mb_type: str, limit=10) -> pd.DataFrame:
        """
        Lookup an IOC in MalwareBazaar.
        """

        if MBEntityType(mb_type) not in self._SUPPORTED_MB_TYPES:
            raise KeyError(
                f"Property type {mb_type} not supported",
                "Valid types are",
                ", ".join(x.value for x in MBEntityType.__members__.values()),
            )

        if mb_type == "hash":
            try:
                data = {'query': 'get_info', "hash": observable}
                res = requests.post(_BASE_URL, data=data)
                res = json.loads(res.text)
                if res["query_status"] == 'ok':
                    observable_df = json_normalize(res['data'])
                    return observable_df
                return res["query_status"]
            except requests.exceptions.RequestException as err:
                return err

        elif mb_type == "tag":
            try:
                data = {'query': 'get_taginfo', "tag": observable, "limit": limit}
                res = requests.post(_BASE_URL, data=data)
                res = json.loads(res.text)

                if res["query_status"] == 'ok':
                    observable_df = json_normalize(res['data'])
                    return observable_df
                return res["query_status"]
            except requests.exceptions.RequestException as err:
                return err

        elif mb_type == "signature":
            try:
                data = {'query': 'get_siginfo', "signature": observable, "limit": limit}
                res = requests.post(_BASE_URL, data=data)
                res = json.loads(res.text)

                if res["query_status"] == 'ok':
                    observable_df = json_normalize(res['data'])
                    return observable_df
                return res["query_status"]
            except requests.exceptions.RequestException as err:
                return err

        elif mb_type == "filetype":
            try:
                data = {'query': 'get_file_type', "file_type": observable, "limit": limit}
                res = requests.post(_BASE_URL, data=data)
                res = json.loads(res.text)

                if res["query_status"] == 'ok':
                    observable_df = json_normalize(res['data'])
                    return observable_df
                return res["query_status"]
            except requests.exceptions.RequestException as err:
                return err

        elif mb_type == "clamav":
            try:
                data = {'query': 'get_clamavinfo', "clamav": observable, "limit": limit}
                res = requests.post(_BASE_URL, data=data)
                res = json.loads(res.text)
                if res["query_status"] == 'ok':
                    observable_df = json_normalize(res['data'])
                    return observable_df
                return res["query_status"]
            except requests.exceptions.RequestException as err:
                return err

        elif mb_type == "imphash":
            try:
                data = {'query': 'get_imphash', "imphash": observable, "limit": limit}
                res = requests.post(_BASE_URL, data=data)
                res = json.loads(res.text)
                if res["query_status"] == 'ok':
                    observable_df = json_normalize(res['data'])
                    return observable_df
                return res["query_status"]
            except requests.exceptions.RequestException as err:
                return err

        elif mb_type == "dhash":
            try:
                data = {'query': 'get_dhash_icon', "dhash_icon": observable, "limit": limit}
                res = requests.post(_BASE_URL, data=data)
                res = json.loads(res.text)
                if res["query_status"] == 'ok':
                    observable_df = json_normalize(res['data'])
                    return observable_df
                return res["query_status"]
            except requests.exceptions.RequestException as err:
                return err

        elif mb_type == "yara":
            try:
                data = {'query': 'get_yarainfo', "yara_rule": observable, "limit": limit}
                res = requests.post(_BASE_URL, data=data)
                res = json.loads(res.text)
                if res["query_status"] == 'ok':
                    observable_df = json_normalize(res['data'])
                    return observable_df
                return res["query_status"]
            except requests.exceptions.RequestException as err:
                return err

        elif mb_type == "tlsh":
            try:
                data = {'query': 'get_tlsh', "tlsh": observable, "limit": limit}
                res = requests.post(_BASE_URL, data=data)
                res = json.loads(res.text)
                if res["query_status"] == 'ok':
                    observable_df = json_normalize(res['data'])
                    return observable_df
                return res["query_status"]
            except requests.exceptions.RequestException as err:
                return err

        elif mb_type == "telfhash":
            try:
                data = {'query': 'get_telfhash', "telfhash": observable, "limit": limit}
                res = requests.post(_BASE_URL, data=data)
                res = json.loads(res.text)
                if res["query_status"] == 'ok':
                    observable_df = json_normalize(res['data'])
                    return observable_df
                return res["query_status"]
            except requests.exceptions.RequestException as err:
                return err

        elif mb_type == "issuerinfo":
            try:
                data = {'query': 'get_issuerinfo', "issuer_cn": observable}
                res = requests.post(_BASE_URL, data=data)
                res = json.loads(res.text)
                if res["query_status"] == 'ok':
                    observable_df = json_normalize(res['data'])
                    return observable_df
                return res["query_status"]
            except requests.exceptions.RequestException as err:
                return err

        elif mb_type == "subjectinfo":
            try:
                data = {'query': 'get_subjectinfo', "subject_cn": observable}
                res = requests.post(_BASE_URL, data=data)
                res = json.loads(res.text)
                if res["query_status"] == 'ok':
                    observable_df = json_normalize(res['data'])
                    return observable_df
                return res["query_status"]
            except requests.exceptions.RequestException as err:
                return err

        elif mb_type == "certificate":
            try:
                data = {'query': 'get_certificate', "serial_number": observable}
                res = requests.post(_BASE_URL, data=data)
                res = json.loads(res.text)
                if res["query_status"] == 'ok':
                    observable_df = json_normalize(res['data'])
                    return observable_df
                return res["query_status"]
            except requests.exceptions.RequestException as err:
                return err

    def download_sample(self, sha2):
        """ Download specified sample from MB"""
        try:
            data = {'query': 'get_file', "sha256_hash": sha2}
            res = requests.post(_BASE_URL, data=data)
            return res.content
        except requests.exceptions.RequestException as err:
            return err

    def get_recent(self, selector):
        """ Get the recent MB additions.

            selector:
                - time: get the latest sample from the last 60 min.
                - 100: get the latest 100 samples.
        """
        try:
            data = {'query': 'get_recent', "selector": selector}
            res = requests.post(_BASE_URL, data=data)
            res = json.loads(res.text)
            if res["query_status"] == 'ok':
                observable_df = json_normalize(res['data'])
                return observable_df
            return res["query_status"]
        except requests.exceptions.RequestException as err:
            return err

    def get_cscb(self):
        """ Query Code Signing Certificate Blocklist (CSCB) """
        try:
            data = {'query': 'get_cscb'}
            res = requests.post(_BASE_URL, data=data)
            res = json.loads(res.text)
            if res["query_status"] == 'ok':
                observable_df = json_normalize(res['data'])
                return observable_df
            return res["query_status"]
        except requests.exceptions.RequestException as err:
            return err

def _get_mb_api_key():
    """Retrieve the MB key from settings."""
    prov_settings = get_provider_settings("TIProviders")
    mb_settings = prov_settings.get("MalwareBazaar")
    if mb_settings:
        return mb_settings.args.get("AuthKey")
    return None
