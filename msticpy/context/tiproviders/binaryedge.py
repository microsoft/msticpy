# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Binary Edge Provider.

Input can be a single IoC observable or a pandas DataFrame containing
multiple observables. Processing may require an API key and
processing performance may be limited to a specific number of
requests per minute for the account type that you have.

"""
from __future__ import annotations

from typing import Any, ClassVar

from typing_extensions import Self

from ..._version import VERSION
from ...common.utility import export
from ..http_provider import APILookupParams
from .result_severity import ResultSeverity
from .ti_http_provider import HttpTIProvider

__version__ = VERSION
__author__ = "Pete Bryan"


@export
class BinaryEdge(HttpTIProvider):
    """BinaryEdge Lookup."""

    PROVIDER_NAME: ClassVar[str] = "BinaryEdge"

    _BASE_URL: ClassVar[str] = "https://api.binaryedge.io/"

    _QUERIES: ClassVar[dict[str, APILookupParams]] = {
        "ipv4": APILookupParams(
            path="v2/query/ip/{observable}",
            headers={"X-Key": "{AuthKey}"},
        ),
    }

    _REQUIRED_PARAMS: ClassVar[list[str]] = ["AuthKey"]
    # aliases
    _QUERIES["ipv6"] = _QUERIES["ipv4"]

    def parse_results(self: Self, response: dict) -> tuple[bool, ResultSeverity, Any]:
        """Return the details of the response."""
        if self._failed_response(response) or not isinstance(
            response["RawResult"],
            dict,
        ):
            return False, ResultSeverity.information, "Not found."

        data: list = response["RawResult"]["events"]
        result_dict: dict[str, Any] = {"IP": response["RawResult"]["query"]}
        open_ports: list[str] = []
        for data_point in data:
            open_ports.append(data_point["port"])
            service_details: dict[str, Any] = {}
            for result in data_point["results"]:
                if "service" in result["result"]["data"]:
                    service_details["Banner"] = result["result"]["data"]["service"][
                        "banner"
                    ]
                if "cert_info" in result["result"]["data"]:
                    service_details["Cert Info"] = result["result"]["data"]["cert_info"]
            result_dict[data_point["port"]] = service_details
        result_dict["Ports"] = open_ports

        return (True, ResultSeverity.information, result_dict)
