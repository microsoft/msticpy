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
from typing import Any

from .http_base import HttpProvider, IoCLookupParams
from ...nbtools.utility import export
from ..._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


@export
class OTX(HttpProvider):
    """AlientVault OTX Lookup."""

    _BASE_URL = "https://otx.alienvault.com"

    _file_hash_params = IoCLookupParams(
        path="/api/v1/indicators/file/{observable}/general",
        headers={"X-OTX-API-KEY": "{API_KEY}"},
    )

    _IOC_QUERIES = {
        "ipv4": IoCLookupParams(
            path="/api/v1/indicators/IPv4/{observable}/general",
            headers={"X-OTX-API-KEY": "{API_KEY}"},
        ),
        "ipv6": IoCLookupParams(
            path="/api/v1/indicators/IPv6/{observable}/general",
            headers={"X-OTX-API-KEY": "{API_KEY}"},
        ),
        "dns": IoCLookupParams(
            path="/api/v1/indicators/domain/{observable}/general",
            headers={"X-OTX-API-KEY": "{API_KEY}"},
        ),
        "hostname": IoCLookupParams(
            path="/api/v1/indicators/hostname/{observable}/general",
            headers={"X-OTX-API-KEY": "{API_KEY}"},
        ),
        "file_hash": _file_hash_params,
        "md5_hash": _file_hash_params,
        "sha1_hash": _file_hash_params,
        "sha256_hash": _file_hash_params,
        "url": IoCLookupParams(
            path="/api/v1/indicators/url/{observable}/general",
            headers={"X-OTX-API-KEY": "{API_KEY}"},
        ),
    }

    @staticmethod
    def get_result(response: Any) -> bool:
        """
        Return True if the response indicates a hit.

        Parameters
        ----------
        response : Any
            The returned data response

        Returns
        -------
        bool
            True if a positive match

        """
        return "pulse_info" in response.keys()

    @staticmethod
    def get_result_details(response: Any) -> Any:
        """
        Return the details of the response.

        Parameters
        ----------
        response : Any
            The returned data response

        Returns
        -------
        Any
            Object with match details

        """
        if "pulse_info" in response.keys():
            pulses = response["pulse_info"].get("pulses", [])
            return {
                "pulse_count": len(pulses),
                "tags": pulses.get("tags", []),
                "reference": pulses.get("references", []),
            }

        return {}
