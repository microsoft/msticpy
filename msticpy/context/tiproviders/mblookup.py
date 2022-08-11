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

import httpx 
import json
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
    GIMPHASH = "gimphash"

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
        MBEntityType.GIMPHASH,
    }

    def __init__(self, mb_key=None):
        """Init function to get the API key if necessary"""
        self.mb_key = mb_key # or_get_mb_api_key()


    def _make_mb_request(self, data):
        """
        Request to the malware bazaar api.
        """
        try:
            res = httpx.post(_BASE_URL, data=data, timeout=None)
            res_data = json.loads(res.text)
            if res_data["query_status"] == 'ok':
                return json_normalize(res_data['data'])
            return res["query_status"]
        except requests.exceptions.RequestException as err:
                return err

    def lookup_ioc(self, observable: str, mb_type: str, limit=10) -> pd.DataFrame:
        """
        Lookup for IOC in MalwareBazaar.

        Parameters:
            observable (str): The observable to lookup. It can be a hash, a signature 
                            or a tag(e.g: emotet, filetype, clamav, imphash, dhash...
            
            mb_type (str): The type of the observable. It can be a hash, a signature (refer to MBEntityType).
            limit (int): The number of results to return, default is 100 or 50 in some cases. 

        Returns:
            pandas.DataFrame: The results of the lookup.
        """

        if MBEntityType(mb_type) not in self._SUPPORTED_MB_TYPES:
            raise KeyError(
                f"Property type {mb_type} not supported",
                "Valid types are",
                ", ".join(x.value for x in MBEntityType.__members__.values()),
            )

        if mb_type == "hash":
            return self._make_mb_request({'query': 'get_info', "hash": observable})

        elif mb_type == "tag":
            return self._make_mb_request({'query': 'get_taginfo', "tag": observable, "limit": limit})
    
        elif mb_type == "signature":
            return self._make_mb_request({'query': 'get_siginfo', "signature": observable, "limit": limit})

        elif mb_type == "filetype":
            return self._make_mb_request({'query': 'get_file_type', "file_type": observable, "limit": limit})

        elif mb_type == "clamav":
            return self._make_mb_request({'query': 'get_clamavinfo', "clamav": observable, "limit": limit})
        
        elif mb_type == "imphash":
            return self._make_mb_request({'query': 'get_imphash', "imphash": observable, "limit": limit})

        elif mb_type == "dhash":
            return self._make_mb_request({'query': 'get_dhash_icon', "dhash_icon": observable, "limit": limit})

        elif mb_type == "yara":
            return self._make_mb_request({'query': 'get_yarainfo', "yara_rule": observable, "limit": limit})

        elif mb_type == "tlsh":
            return self._make_mb_request({'query': 'get_tlsh', "tlsh": observable, "limit": limit})

        elif mb_type == "telfhash":
            return self._make_mb_request({'query': 'get_telfhash', "telfhash": observable, "limit": limit})

        elif mb_type == "gimphash":
            return self._make_mb_request({'query': 'get_gimphash', "gimphash": observable, "limit": limit})
            
        elif mb_type == "issuerinfo":
            return self._make_mb_request({'query': 'get_issuerinfo', "issuer_cn": observable})

        elif mb_type == "subjectinfo":
            return self._make_mb_request({'query': 'get_subjectinfo', "subject_cn": observable})
            
        elif mb_type == "certificate":
            return self._make_mb_request({'query': 'get_certificate', "serial_number": observable})

    def download_sample(self, sha2):
        """ Download specified sample from MB"""
        return self._make_mb_request({'query': 'get_file', "sha256_hash": sha2})

    def get_recent(self, selector):
        """ Get the recent MB additions.
            The selector can be either "time" or number of samples.
            https://bazaar.abuse.ch/api/#latest_additions

            selector:
                time (str): get the latest sample from the last 60 min.
                100 (int): get the latest 100 samples.

            Returns:
            pandas.DataFrame: The results of the latest addition.
        """
        return self._make_mb_request({'query': 'get_recent', "selector": selector})

    def get_cscb(self):
        """ Query Code Signing Certificate Blocklist (CSCB) """
        return self._make_mb_request({'query': 'get_cscb'})

def _get_mb_api_key():
    """Retrieve the MB key from settings."""
    prov_settings = get_provider_settings("TIProviders")
    mb_settings = prov_settings.get("MalwareBazaar")
    if mb_settings:
        return mb_settings.args.get("AuthKey")
    return None
