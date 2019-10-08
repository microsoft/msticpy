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
from typing import Any, Tuple

import attr

from .ti_provider_base import LookupResult, TISeverity
from .http_base import HttpProvider, IoCLookupParams
from ...nbtools.utility import export
from ..._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


# pylint: disable=too-few-public-methods
@attr.s
class _OTXParams(IoCLookupParams):
    # override IoCLookupParams to set common defaults
    def __attrs_post_init__(self):
        self.headers = {"X-OTX-API-KEY": "{API_KEY}"}


@export
class OTX(HttpProvider):
    """AlientVault OTX Lookup."""

    _BASE_URL = "https://otx.alienvault.com"

    _IOC_QUERIES = {
        "ipv4": _OTXParams(path="/api/v1/indicators/IPv4/{observable}/general"),
        "ipv6": _OTXParams(path="/api/v1/indicators/IPv6/{observable}/general"),
        "ipv4-passivedns": _OTXParams(
            path="/api/v1/indicators/IPv4/{observable}/passive_dns"
        ),
        "ipv6-passivedns": _OTXParams(
            path="/api/v1/indicators/IPv6/{observable}/passive_dns"
        ),
        "ipv4-geo": _OTXParams(path="/api/v1/indicators/IPv4/{observable}/geo"),
        "ipv6-geo": _OTXParams(path="/api/v1/indicators/IPv6/{observable}/geo"),
        "dns": _OTXParams(path="/api/v1/indicators/domain/{observable}/general"),
        "dns-passivedns": _OTXParams(
            path="/api/v1/indicators/domain/{observable}/passive_dns"
        ),
        "dns-geo": _OTXParams(path="/api/v1/indicators/domain/{observable}/geo"),
        "hostname": _OTXParams(path="/api/v1/indicators/hostname/{observable}/general"),
        "file_hash": _OTXParams(path="/api/v1/indicators/file/{observable}/general"),
        "url": _OTXParams(path="/api/v1/indicators/url/{observable}/general"),
    }

    # pylint: disable=duplicate-code
    # aliases
    _IOC_QUERIES["md5_hash"] = _IOC_QUERIES["file_hash"]
    _IOC_QUERIES["sha1_hash"] = _IOC_QUERIES["file_hash"]
    _IOC_QUERIES["sha256_hash"] = _IOC_QUERIES["file_hash"]

    _REQUIRED_PARAMS = ["API_KEY"]

    def parse_results(self, response: LookupResult) -> Tuple[bool, TISeverity, Any]:
        """
        Return the details of the response.

        Parameters
        ----------
        response : LookupResult
            The returned data response

        Returns
        -------
        Tuple[bool, TISeverity, Any]
            bool = positive or negative hit
            TISeverity = enumeration of severity
            Object with match details

        """
        if self._failed_response(response) or not isinstance(response.raw_result, dict):
            return False, TISeverity.information, "Not found."
        if "pulse_info" in response.raw_result:
            pulses = response.raw_result["pulse_info"].get("pulses", {})
            pulse_count = len(pulses)
            if pulse_count == 0:
                severity = TISeverity.information
                return (
                    True,
                    severity,
                    {
                        "pulse_count": pulse_count,
                        "sections_available": response.raw_result["sections"],
                    },
                )
            if pulse_count == 1:
                severity = TISeverity.warning
            else:
                severity = TISeverity.high
            return (
                True,
                severity,
                {
                    "pulse_count": pulse_count,
                    "names": [p.get("name") for p in pulses],
                    "tags": [p.get("tags") for p in pulses],
                    "references": [p.get("references") for p in pulses],
                },
            )
        return True, TISeverity.information, {}

    # pylint: enable=duplicate-code
