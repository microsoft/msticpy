# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# Author: Thomas Roccia - @fr0gger_
# --------------------------------------------------------------------------
"""MalwareBazaar TI Provider."""
import json
from enum import Enum
from typing import Set

import httpx
import pandas as pd
from pandas import json_normalize

from ..._version import VERSION
from ...common.provider_settings import get_provider_settings

__version__ = VERSION
__author__ = "Thomas Roccia | @fr0gger_"

_BASE_URL = "https://mb-api.abuse.ch/api/v1/"

_QUERY_OBJECTS_MAPPINGS = {
    "hash": {"query": "get_info", "hash": "observable"},
    "tag": {"query": "get_taginfo", "tag": "observable", "limit": "limit"},
    "signature": {"query": "get_siginfo", "signature": "observable", "limit": "limit"},
    "filetype": {"query": "get_file_type", "file_type": "observable", "limit": "limit"},
    "clamav": {"query": "get_clamavinfo", "clamav": "observable", "limit": "limit"},
    "imphash": {"query": "get_imphash", "imphash": "observable", "limit": "limit"},
    "dhash": {"query": "get_dhash_icon", "dhash_icon": "observable", "limit": "limit"},
    "yara": {"query": "get_yarainfo", "yara_rule": "observable", "limit": "limit"},
    "tlsh": {"query": "get_tlsh", "tlsh": "observable", "limit": "limit"},
    "telfhash": {"query": "get_telfhash", "telfhash": "observable", "limit": "limit"},
    "gimphash": {"query": "get_gimphash", "gimphash": "observable", "limit": "limit"},
    "issuerinfo": {"query": "get_issuerinfo", "issuer_cn": "observable"},
    "subjectinfo": {"query": "get_subjectinfo", "subject_cn": "observable"},
    "certificate": {"query": "get_certificate", "serial_number": "observable"},
}


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
    """MBlookup Python Class wrapper for MalwareBazaar API."""

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
        """Init function to get the API key if necessary."""
        self.mb_key = mb_key  # or_get_mb_api_key()

    def _make_mb_request(self, data):
        """Request to the malware bazaar api."""
        try:
            res = httpx.post(_BASE_URL, data=data, timeout=None)
            res_data = json.loads(res.text)
            if res_data["query_status"] == "ok":
                return json_normalize(res_data["data"])
            return res["query_status"]
        except httpx.ConnectError as err:
            return err

    def lookup_ioc(self, observable: str, mb_type: str, limit=10) -> pd.DataFrame:
        """
        Lookup for IOC in MalwareBazaar.

        Parameters
        ----------
        observable : str
            The observable to lookup. It can be a hash, a signature
        mb_type : str
            The type of the observable. It can be a hash, a signature (refer to MBEntityType).
        limit : int, optional
            The number of results to return, default is 100 or 50 in some cases, by default 10

        Returns
        -------
        pd.DataFrame
            The results of the lookup.

        Raises
        ------
        KeyError
            If invalid IoC type is provided.

        """
        if MBEntityType(mb_type) not in self._SUPPORTED_MB_TYPES:
            raise KeyError(
                f"Property type {mb_type} not supported",
                "Valid types are",
                ", ".join(x.value for x in MBEntityType.__members__.values()),
            )

        query_parameters = _build_query_string(observable, mb_type, limit)

        return self._make_mb_request(query_parameters)

    def download_sample(self, sha2: str):
        """Download specified sample from MB."""
        return self._make_mb_request({"query": "get_file", "sha256_hash": sha2})

    def get_recent(self, selector: str):
        """
        Get the recent MB additions.

        Parameters
        ----------
        selector : str
            Get the latest sample from the last 60 min.

        Returns
        -------
        pd.DataFrame
           The results of the latest addition.

        """
        return self._make_mb_request({"query": "get_recent", "selector": selector})

    def get_cscb(self):
        """Query Code Signing Certificate Blocklist (CSCB)."""
        return self._make_mb_request({"query": "get_cscb"})


def _get_mb_api_key():
    """Retrieve the MB key from settings."""
    prov_settings = get_provider_settings("TIProviders")
    mb_settings = prov_settings.get("MalwareBazaar")
    if mb_settings:
        return mb_settings.args.get("AuthKey")
    return None


def _build_query_string(observable, mb_type, limit):
    """Build the query string for the MB API."""
    query_items = _QUERY_OBJECTS_MAPPINGS[mb_type]
    if "limit" in query_items:
        query_items["limit"] = limit
    for key, value in query_items.items():
        if value == "observable":
            query_items[key] = observable
    return query_items
