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
from __future__ import annotations

from dataclasses import dataclass
from typing import Any, ClassVar

from typing_extensions import Self

from ..._version import VERSION
from ...common.utility import export
from ..http_provider import APILookupParams
from .ti_http_provider import HttpTIProvider
from .ti_provider_base import ResultSeverity

__version__ = VERSION
__author__ = "Ian Hellen"


# pylint: disable=too-few-public-methods
@dataclass
class _OTXParams(APILookupParams):
    # override APILookupParams to set common defaults
    def __post_init__(self: Self) -> None:
        self.headers = {"X-OTX-API-KEY": "{AuthKey}"}


@export
class OTX(HttpTIProvider):
    """AlientVault OTX Lookup."""

    _BASE_URL: ClassVar[str] = "https://otx.alienvault.com"

    _QUERIES: ClassVar[dict[str, _OTXParams]] = {
        "ipv4": _OTXParams(path="/api/v1/indicators/IPv4/{observable}/general"),
        "ipv6": _OTXParams(path="/api/v1/indicators/IPv6/{observable}/general"),
        "ipv4-passivedns": _OTXParams(
            path="/api/v1/indicators/IPv4/{observable}/passive_dns",
        ),
        "ipv6-passivedns": _OTXParams(
            path="/api/v1/indicators/IPv6/{observable}/passive_dns",
        ),
        "ipv4-geo": _OTXParams(path="/api/v1/indicators/IPv4/{observable}/geo"),
        "ipv6-geo": _OTXParams(path="/api/v1/indicators/IPv6/{observable}/geo"),
        "dns": _OTXParams(path="/api/v1/indicators/domain/{observable}/general"),
        "dns-passivedns": _OTXParams(
            path="/api/v1/indicators/domain/{observable}/passive_dns",
        ),
        "dns-geo": _OTXParams(path="/api/v1/indicators/domain/{observable}/geo"),
        "hostname": _OTXParams(path="/api/v1/indicators/hostname/{observable}/general"),
        "file_hash": _OTXParams(path="/api/v1/indicators/file/{observable}/general"),
        "url": _OTXParams(path="/api/v1/indicators/url/{observable}/general"),
    }

    # aliases
    _QUERIES["md5_hash"] = _QUERIES["file_hash"]
    _QUERIES["sha1_hash"] = _QUERIES["file_hash"]
    _QUERIES["sha256_hash"] = _QUERIES["file_hash"]

    _REQUIRED_PARAMS: ClassVar[list[str]] = ["AuthKey"]

    def __init__(
        self: OTX,
        *,
        timeout: int | None = None,
        ApiID: str | None = None,  # noqa: N803
        AuthKey: str | None = None,  # noqa: N803
    ) -> None:
        """Set OTX specific settings."""
        super().__init__(
            timeout=timeout,
            ApiID=ApiID,
            AuthKey=AuthKey,
        )
        self.require_url_encoding = True

    def parse_results(self: Self, response: dict) -> tuple[bool, ResultSeverity, Any]:
        """
        Return the details of the response.

        Parameters
        ----------
        response : Dict
            The returned data response

        Returns
        -------
        Tuple[bool, ResultSeverity, Any]
            bool = positive or negative hit
            ResultSeverity = enumeration of severity
            Object with match details

        """
        if self._failed_response(response) or not isinstance(
            response["RawResult"],
            dict,
        ):
            return False, ResultSeverity.information, "Not found."
        if "pulse_info" in response["RawResult"]:
            pulses = response["RawResult"]["pulse_info"].get("pulses", {})
            pulse_count = len(pulses)
            if pulse_count == 0:
                severity = ResultSeverity.information
                return (
                    True,
                    severity,
                    {
                        "pulse_count": pulse_count,
                        "sections_available": response["RawResult"]["sections"],
                    },
                )
            severity = (
                ResultSeverity.warning if pulse_count == 1 else ResultSeverity.high
            )
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
        return True, ResultSeverity.information, {}
