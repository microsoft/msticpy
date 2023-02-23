# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Test module for plugin import tests."""
from typing import Any, Dict, Tuple

import pandas as pd

from msticpy.context.contextproviders.context_provider_base import ContextProvider
from msticpy.context.http_provider import APILookupParams
from msticpy.context.tiproviders import TIProvider
from msticpy.context.tiproviders.ti_http_provider import HttpTIProvider, ResultSeverity

__author__ = "Ian Hellen"


def _parse_results(response: Dict) -> Tuple[bool, ResultSeverity, Any]:
    """Return the details of the response."""
    return (
        (
            True,
            ResultSeverity.information,
            {"Detail1": "something", "Detail2": "something_else"},
        )
        if response
        else (False, ResultSeverity.information, "Not found.")
    )


_QUERIES = {
    "ipv4": APILookupParams(path="/api/v1/indicators/IPv4/{observable}/general"),
    "ipv6": APILookupParams(path="/api/v1/indicators/IPv6/{observable}/general"),
    "file_hash": APILookupParams(path="/api/v1/indicators/file/{observable}/general"),
    "url": APILookupParams(path="/api/v1/indicators/url/{observable}/general"),
}
# aliases
_QUERIES["md5_hash"] = _QUERIES["file_hash"]
_QUERIES["sha1_hash"] = _QUERIES["file_hash"]
_QUERIES["sha256_hash"] = _QUERIES["file_hash"]


_df_results = pd.DataFrame(
    [
        {
            "ID": "tests results",
            "Status": 0,
            **{name: str(idx) for idx, name in enumerate("ABC")},
        }
    ]
)


class TIProviderTest(TIProvider):
    """Custom IT provider TI Base."""

    PROVIDER_NAME = "Provider1"
    _BASE_URL = "https://api.service.com"
    _QUERIES = _QUERIES
    _REQUIRED_PARAMS = ["AuthKey"]

    def lookup_ioc(
        self,
        ioc: str,
        ioc_type: str = None,
        query_type: str = None,
        **kwargs,
    ) -> pd.DataFrame:
        """Lookup a single IoC observable."""
        return _df_results

    def parse_results(self, response: Dict) -> Tuple[bool, ResultSeverity, Any]:
        """Return the details of the response."""
        return _parse_results(response)


class TIProviderHttpTest(HttpTIProvider):
    """Custom IT provider TI HTTP."""

    PROVIDER_NAME = "Provider2"
    _BASE_URL = "https://api.service.com"
    _QUERIES = _QUERIES
    _REQUIRED_PARAMS = ["AuthKey"]

    def parse_results(self, response: Dict) -> Tuple[bool, ResultSeverity, Any]:
        """Return the details of the response."""
        return _parse_results(response)


class ContextProviderTest(ContextProvider):
    """Custom context provider TI HTTP."""

    name = "ContextTest"

    _BASE_URL = "https://api.service.com"
    _QUERIES = _QUERIES
    _REQUIRED_PARAMS = ["AuthKey"]

    def lookup_observable(  # type: ignore
        self,
        observable: str,
        observable_type: str = None,
        query_type: str = None,
        **kwargs,
    ) -> pd.DataFrame:
        """Lookup a single observable."""
        return _df_results

    def parse_results(self, response: Dict) -> Tuple[bool, Any]:
        """Return the details of the response."""
        results = _parse_results(response)
        return results[0], results[2]
