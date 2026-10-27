# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""NetAPI TI provider tests.

The provider looks up domains, hostnames and URLs only. The shared
parametrized harness in test_tiproviders.py looks up IP addresses in bulk,
so this provider is tested here: loading through TILookup, current, past
and never-listed results, the registered-domain match, the `rank` query
subtype and the request parameters.

No network access. The provider's httpx client is replaced throughout.
"""

from __future__ import annotations

import warnings
from pathlib import Path
from typing import Any

import httpx
import pytest
import pytest_check as check

from msticpy.common import pkg_config
from msticpy.context.tilookup import TILookup
from msticpy.context.tiproviders.netapi import NetAPI
from msticpy.context.tiproviders.result_severity import ResultSeverity

from ..unit_test_lib import custom_mp_config, get_test_data_path

__author__ = "Slava Olesik"

pytestmark = pytest.mark.filterwarnings("ignore::DeprecationWarning")

# Answers of the live API on 2026-10-26 and 2026-10-27, trimmed to the fields
# the provider reads.
_LISTED_NOW: dict[str, Any] = {
    "value": "www.000359.xyz",
    "type": "domain",
    "listed_now": True,
    "listed_ever": True,
    "matched_value": "000359.xyz",
    "first_seen": "2023-02-25",
    "last_seen": "2026-10-26 01:05:27 UTC",
    "block_reason": "blackbook",
    "registrar": {
        "id": 1599,
        "name": "Alibaba Cloud Computing Ltd.",
        "brand_name": "Alibaba Cloud",
    },
}
_LISTED_PAST: dict[str, Any] = {
    "value": "0-07.ru",
    "type": "domain",
    "listed_now": False,
    "listed_ever": True,
    "matched_value": "0-07.ru",
    "first_seen": "2019-12-18",
    "last_seen": "2019-12-25 00:00:00 UTC",
    "block_reason": "suspicious",
    "registrar": None,
}
_NOT_LISTED: dict[str, Any] = {
    "value": "github.com",
    "type": "domain",
    "listed_now": False,
    "listed_ever": False,
}
_RANK_IN: dict[str, Any] = {
    "domain": "github.com",
    "in_top_1m": True,
    "rank": 11,
    "snapshot_date": "2026-10-26",
}
_RANK_OUT: dict[str, Any] = {
    "domain": "000359.xyz",
    "in_top_1m": False,
    "rank": None,
    "snapshot_date": "2026-10-26",
}


class _Response(httpx.Response):
    """Mocked httpx response."""

    def __init__(self, json_data: Any, status_code: int = 200) -> None:
        self.json_data = json_data
        self.status_code = status_code
        # str(response) reads the reason phrase from here on a failed lookup.
        self.extensions = {}

    def json(self) -> Any:  # noqa: D102
        return self.json_data


class _Client:
    """Mock httpx client that records every request it is given."""

    def __init__(self, data: Any, status_code: int = 200) -> None:
        self.data = data
        self.status_code = status_code
        self.requests: list[dict[str, Any]] = []

    def get(self, *args, **kwargs) -> _Response:
        """Return the canned response and record the call."""
        self.requests.append(
            {
                "url": kwargs.get("url") or (args[0] if args else ""),
                "params": kwargs.get("params", {}),
                "headers": kwargs.get("headers", {}),
            },
        )
        return _Response(self.data, self.status_code)


def _lookup(
    data: Any,
    ioc: str,
    ioc_type: str,
    query_type: str | None = None,
    status_code: int = 200,
) -> tuple[dict[str, Any], _Client]:
    """Run one lookup against a mock client and return the result row."""
    provider = NetAPI()
    client = _Client(data, status_code)
    provider._httpx_client = client  # type: ignore[assignment]
    result = provider.lookup_ioc(ioc, ioc_type, query_type)
    return result.to_dict(orient="records")[0], client


def test_lookup_through_tilookup():
    """The provider loads from the test config and answers through TILookup."""
    config_path = Path(get_test_data_path()).joinpath(pkg_config._CONFIG_FILE)
    with custom_mp_config(config_path):
        with warnings.catch_warnings():
            warnings.simplefilter("ignore", category=UserWarning)
            ti_lookup = TILookup()
    client = _Client(_LISTED_NOW)
    ti_lookup.loaded_providers["NetAPI"]._httpx_client = client

    result = ti_lookup.lookup_ioc("www.000359.xyz", "hostname", providers=["NetAPI"])
    row = result.to_dict(orient="records")[0]

    check.equal(row["Provider"], "NetAPI")
    check.is_true(row["Result"])
    check.equal(row["Severity"], ResultSeverity.high.name)
    check.equal(len(client.requests), 1)


def test_current_listing_is_high_severity():
    """A host reported within the last 24 hours is a current listing."""
    row, client = _lookup(_LISTED_NOW, "www.000359.xyz", "hostname")

    check.is_true(row["Result"])
    check.equal(row["Severity"], ResultSeverity.high.name)
    check.equal(row["Details"]["Matched Value"], "000359.xyz")
    check.equal(row["Details"]["Reported By"], "blackbook")
    check.equal(row["Details"]["Registrar"], "Alibaba Cloud")
    check.equal(
        client.requests[0]["params"],
        {"method": "compromised-check", "value": "www.000359.xyz"},
    )
    check.equal(client.requests[0]["url"], "https://netapi.com/api-json/")


def test_past_listing_is_reported_but_not_rated():
    """
    A past listing is mostly an incident that is over.

    It is a hit with its dates, but it must not carry the severity of a
    current listing.
    """
    row, _ = _lookup(_LISTED_PAST, "0-07.ru", "dns")

    check.is_true(row["Result"])
    check.equal(row["Severity"], ResultSeverity.information.name)
    check.is_false(row["Details"]["Listed Now"])
    check.equal(row["Details"]["Last Seen"], "2019-12-25 00:00:00 UTC")


def test_never_listed_is_a_negative_result():
    """A value that was never in the feed is not a hit."""
    row, _ = _lookup(_NOT_LISTED, "github.com", "dns")

    check.is_false(row["Result"])
    check.equal(row["Severity"], ResultSeverity.information.name)
    check.equal(row["Details"], "Not listed, now or in the past.")


def test_url_is_sent_to_the_compromised_check():
    """The API reduces a URL to its host itself."""
    _, client = _lookup(_LISTED_NOW, "https://www.000359.xyz/login", "url")

    check.equal(client.requests[0]["params"]["method"], "compromised-check")
    check.is_in("000359.xyz", client.requests[0]["params"]["value"])


@pytest.mark.parametrize("ioc_type", ["dns", "hostname"])
def test_rank_subtype_returns_the_top_1m_position(ioc_type):
    """The `rank` subtype asks domain-rank and reports the position."""
    row, client = _lookup(_RANK_IN, "github.com", ioc_type, "rank")

    check.is_true(row["Result"])
    check.equal(row["Details"]["Rank"], 11)
    check.equal(row["Details"]["Snapshot Date"], "2026-10-26")
    check.equal(
        client.requests[0]["params"],
        {"method": "domain-rank", "domain": "github.com"},
    )


def test_domain_outside_the_top_1m_is_not_a_hit():
    """A domain outside the ranking is a negative rank result."""
    row, _ = _lookup(_RANK_OUT, "000359.xyz", "dns", "rank")

    check.is_false(row["Result"])
    check.is_none(row["Details"]["Rank"])


def test_rate_limit_is_not_a_hit():
    """An API error is reported as a failed lookup, never as a listing."""
    error = {"error": {"code": "rate_limited", "message": "Too many requests."}}
    row, _ = _lookup(error, "github.com", "dns", status_code=429)

    check.is_false(row["Result"])
    check.equal(row["Status"], 429)


def test_no_key_and_own_user_agent():
    """The API is keyless, so no auth header is ever sent."""
    _, client = _lookup(_NOT_LISTED, "github.com", "dns")

    headers = client.requests[0]["headers"]
    check.equal(headers.get("User-Agent"), "netapi-msticpy/1.0")
    check.is_false(any(key.lower() == "authorization" for key in headers))


@pytest.mark.parametrize(
    ("ioc", "ioc_type"),
    [
        ("7657fcb7d772448a6d8504e4b20168b7", "md5_hash"),
        ("185.92.220.35", "ipv4"),
        ("2345:0425:2CA1:0000:0000:0567:5673:23b5", "ipv6"),
    ],
)
def test_hashes_and_ip_addresses_are_not_supported(ioc, ioc_type):
    """Hashes and IP addresses are not looked up and no request is made."""
    row, client = _lookup(_NOT_LISTED, ioc, ioc_type)

    check.is_false(row["Result"])
    check.equal(client.requests, [])


def test_empty_answer_is_not_a_hit():
    """A 200 with an empty body is reported as not found, not as a listing."""
    row, _ = _lookup({}, "github.com", "dns")

    check.is_false(row["Result"])
    check.equal(row["Details"], "Not found.")
