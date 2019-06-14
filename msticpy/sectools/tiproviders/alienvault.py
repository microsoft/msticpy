# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
AlienVault OTX Provider.

Input can be a single IoC observable or a pandas DataFrame containing
multiple observables. Processing may require a an API key and
processing performance may be limited to a specific number of
requests per minute for the account type that you have.

"""
from typing import List, Mapping, Any, Tuple, Union, Iterable, Set

from collections import namedtuple

# from ipaddress import IPv4Address, ip_address
import warnings

# from socket import gaierror

import pandas as pd

import requests

# from urllib3.exceptions import LocationParseError
# from urllib3.util import parse_url

from .ti_provider import TIProvider
from ..iocextract import IoCExtract, IoCType

from ...nbtools.utility import export
from ..._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"

IoCLookupParams = namedtuple(
    "IoCLookupParams", ["path", "verb", "headers", "params", "auth_type", "auth_str"]
)


class HttpTIProvider(TIProvider):

    _BASE_URL = "https://otx.alienvault.com"

    _file_hash_params = IoCLookupParams(
        "/api/v1/indicators/file/{observable}/general",
        "GET",
        {"X-OTX-API-KEY": "{API_KEY}"},
        None,
        None,
        None,
    )

    _IOC_QUERIES = {
        "ipv4": IoCLookupParams(
            "/api/v1/indicators/IPv4/{observable}/general",
            "GET",
            {"X-OTX-API-KEY": "{API_KEY}"},
            None,
            None,
            None,
        ),
        "ipv6": IoCLookupParams(
            "/api/v1/indicators/IPv6/{observable}/general",
            "GET",
            {"X-OTX-API-KEY": "{API_KEY}"},
            None,
            None,
            None,
        ),
        "dns": IoCLookupParams(
            "/api/v1/indicators/domain/{observable}/general",
            "GET",
            {"X-OTX-API-KEY": "{API_KEY}"},
            None,
            None,
            None,
        ),
        "hostname": IoCLookupParams(
            "/api/v1/indicators/hostname/{observable}/general",
            "GET",
            {"X-OTX-API-KEY": "{API_KEY}"},
            None,
            None,
            None,
        ),
        "md5_hash": _file_hash_params,
        "sha1_hash": _file_hash_params,
        "sha256_hash": _file_hash_params,
        "url": IoCLookupParams(
            "/api/v1/indicators/url/{observable}/general",
            "GET",
            {"X-OTX-API-KEY": "{API_KEY}"},
            None,
            None,
            None,
        ),
    }

    def __init__(self, **kwargs):
        super().__init__(**kwargs)

        self._requests_session = None

    def lookup_ioc(self, ioc: str, ioc_type: str = None) -> Tuple[bool, str]:
        """Lookup a single IoC observable."""
        pass

    def lookup_iocs(
        self,
        data: Union[pd.DataFrame, Mapping[str, str], Iterable[str]],
        obs_col: str = None,
        ioc_type_col: str = None,
    ) -> pd.DataFrame:
        """
        Lookup collection of IoC observables.

        Parameters
        ----------
        data : Union[pd.DataFrame,Mapping[str, str], Iterable[str]]
            Data input in one of three formats:
            1. Pandas dataframe (you must supply the column name in
            `obs_col` parameter)
            2. Mapping (e.g. a dict) of observable, IoCType
            3. Iterable of observables - IoCTypes will be inferred
        obs_col : str, optional
            DataFrame column to use for observables, by default None
        ioc_type_col : str, optional
            DataFrame column to use for IoCTypes, by default None

        Returns
        -------
        pd.DataFrame
            DataFrame of results.

        """
