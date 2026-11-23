# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""DarkEye TI provider tests.

No network access: every HTTP request is answered by respx. The provider is
tested through its real httpx client, so the headers, URLs and redirect handling
that are asserted here are what would be sent.
"""

from __future__ import annotations

import json
import logging
import warnings
from pathlib import Path
from typing import Any

import httpx
import pytest
import pytest_check as check
import respx

from msticpy.common.exceptions import MsticpyConfigError
from msticpy.config.mp_config_control import MpConfigControls, get_mpconfig_definitions
from msticpy.context.lookup_result import LookupStatus
from msticpy.context.tilookup import TILookup
from msticpy.context.tiproviders import darkeye
from msticpy.context.tiproviders.darkeye import DarkEye, DarkEyeRecord
from msticpy.context.tiproviders.result_severity import ResultSeverity

from ..unit_test_lib import custom_mp_config, get_test_data_path

__author__ = "Juan Manuel Tejada"

# pylint: disable=protected-access, redefined-outer-name

# Mock secrets. Two different values so that a key sent to the wrong host shows up.
_HIBR_KEY = "[PLACEHOLDER-HIBR]"
_BH_KEY = "[PLACEHOLDER-BH]"

_HIBR_DOMAIN = "https://haveibeenransom.com/api/metadata/domain/example.com"
_HIBR_EMAIL = "https://haveibeenransom.com/api/metadata/email/bob%40example.com"
_BH_SEARCH = "https://breach.house/api/v1/search"

# Fields that exist in real responses but that the provider must never return.
_SECRET_FIELDS: dict[str, str] = {
    "password": "SECRET-PASSWORD",
    "credentials": "SECRET-CREDENTIALS",
    "Identities Found": "SECRET-IDENTITIES",
    "description": "SECRET-DESCRIPTION",
    "meta_description": "SECRET-META",
    "post_url": "http://SECRET-LEAK-SITE.onion/post",
    "download_link": "http://SECRET-DOWNLOAD.onion/file",
    "hibr": {"domain": "SECRET-HIBR-BLOCK"},
}


@pytest.fixture
def api():
    """Return a respx router that answers every request itself."""
    with respx.mock(assert_all_called=False) as router:
        yield router


def _hibr_item(
    item_id: Any = "1", leak_type: str = "traditional_breach", **kwargs
) -> dict[str, Any]:
    """Return a HaveIBeenRansom metadata item."""
    item = {
        "id_source": item_id,
        "leak_type": leak_type,
        "post_title": f"Title {item_id}",
        "website": "example.com",
        "group_name": "group",
        "country": "US",
        "discovered": "2024-05-06 01:02:03",
    }
    item.update(kwargs)
    return item


def _hibr_page(
    items: list[dict[str, Any]], *, has_next: bool = False, documented: bool = False
) -> dict[str, Any]:
    """Return a HaveIBeenRansom page, in the shape the API sends or the documented one."""
    if documented:
        return {"results": items, "pagination": {"has_next": has_next}, "success": True}
    return {"sources": items, "has_next": has_next}


def _bh_item(item_id: Any = "v1", **kwargs) -> dict[str, Any]:
    """Return a Breach House item for example.com."""
    item = {
        "victim_id": item_id,
        "post_title": f"Victim {item_id}",
        "website": "example.com",
        "group_name": "group",
        "country": "IT",
        "discovered": "2023-01-15 10:22:00.000000",
    }
    item.update(kwargs)
    return item


def _bh_body(**sections: list[dict[str, Any]]) -> dict[str, Any]:
    """Return a Breach House search body with a total for each section."""
    return {name: {"results": items, "total": len(items)} for name, items in sections.items()}


def _lookup(provider: DarkEye, ioc: str, ioc_type: str, **kwargs) -> dict[str, Any]:
    """Run a lookup and return the single result row as a dict."""
    return provider.lookup_ioc(ioc=ioc, ioc_type=ioc_type, **kwargs).to_dict(orient="records")[
        0
    ]


@pytest.fixture
def both() -> DarkEye:
    """Return a provider with both keys."""
    return DarkEye(HIBRAuthKey=_HIBR_KEY, BreachHouseAuthKey=_BH_KEY)


def _hosts(api: respx.MockRouter) -> dict[str, list[httpx.Request]]:
    """Return the requests that were made, by host."""
    by_host: dict[str, list[httpx.Request]] = {}
    for call in api.calls:
        by_host.setdefault(call.request.url.host, []).append(call.request)
    return by_host


# --- keys ------------------------------------------------------------------


@pytest.mark.parametrize(
    "keys", [{}, {"HIBRAuthKey": ""}, {"HIBRAuthKey": "  ", "BreachHouseAuthKey": ""}]
)
def test_no_key_cannot_run(keys):
    """With no key the provider is refused at load, before anything can be sent."""
    with pytest.raises(MsticpyConfigError) as err:
        DarkEye(**keys)
    check.is_in("HIBRAuthKey", str(err.value))
    check.is_in("BreachHouseAuthKey", str(err.value))


@pytest.mark.parametrize("bad_key", ["has space", "tab\tkey", "line\nbreak", "ünïcode"])
def test_unusable_key_is_refused_without_echoing_it(bad_key):
    """A key that is not printable ASCII is refused and never repeated."""
    with pytest.raises(MsticpyConfigError) as err:
        DarkEye(HIBRAuthKey=bad_key)
    check.is_not_in(bad_key, str(err.value))
    check.is_in("HIBRAuthKey", str(err.value))


def test_keys_are_stripped():
    """Surrounding whitespace of a key is removed."""
    provider = DarkEye(BreachHouseAuthKey=f"  {_BH_KEY}\n")
    check.equal(provider._breach_house_key, _BH_KEY)
    check.is_none(provider._hibr_key)


# --- both products ---------------------------------------------------------


def test_both_keys_one_combined_result(api, both):
    """Both products are queried and come back as one result with product and category."""
    api.get(_HIBR_DOMAIN).respond(
        json=_hibr_page(
            [_hibr_item("1", "ransomware_leak"), _hibr_item("2", "traditional_breach")]
        )
    )
    api.get(_BH_SEARCH).respond(
        json=_bh_body(
            ransomware=[_bh_item("r1")],
            breaches=[_bh_item("b1")],
            leads=[_bh_item("l1")],
            stealers=[_bh_item("s1")],
        )
    )
    row = _lookup(both, "example.com", "dns")

    check.equal(row["Provider"], "DarkEye")
    check.equal(row["Status"], LookupStatus.OK.value)
    check.is_true(row["Result"])
    check.equal(row["Severity"], ResultSeverity.warning.name)
    details = row["Details"]
    check.equal(details["Count"], 6)
    pairs = [(rec["product"], rec["category"]) for rec in details["Records"]]
    check.equal(
        pairs,
        [
            ("HaveIBeenRansom", "ransomware"),
            ("HaveIBeenRansom", "breach"),
            ("Breach House", "ransomware"),
            ("Breach House", "breach"),
            ("Breach House", "lead"),
            ("Breach House", "stealer"),
        ],
    )
    check.equal(details["Categories"], {"ransomware": 2, "breach": 2, "lead": 1, "stealer": 1})
    check.equal(details["Products"]["HaveIBeenRansom"]["Records"], 2)
    check.equal(details["Products"]["Breach House"]["Records"], 4)
    check.is_false(details["Truncated"])
    check.is_not_in("Errors", details)
    check.is_in("haveibeenransom.com", row["Reference"])
    check.is_in("breach.house", row["Reference"])
    first = details["Records"][0]
    check.equal(
        first,
        {
            "product": "HaveIBeenRansom",
            "category": "ransomware",
            "id": "1",
            "title": "Title 1",
            "website": "example.com",
            "group": "group",
            "country": "US",
            "discovered": "2024-05-06",
        },
    )


def test_each_key_goes_only_to_its_own_host(api, both):
    """The HaveIBeenRansom key is only sent to its host and the Breach House key to its own."""
    api.get(_HIBR_DOMAIN).respond(json=_hibr_page([_hibr_item()]))
    api.get(_BH_SEARCH).respond(json=_bh_body(ransomware=[_bh_item()]))
    _lookup(both, "example.com", "dns")

    by_host = _hosts(api)
    check.equal(set(by_host), {"haveibeenransom.com", "breach.house"})
    for request in by_host["haveibeenransom.com"]:
        check.equal(request.headers["Authorization"], f"Bearer {_HIBR_KEY}")
        check.is_not_in(_BH_KEY, str(request.url))
    for request in by_host["breach.house"]:
        check.equal(request.headers["Authorization"], f"Bearer {_BH_KEY}")
        check.is_not_in(_HIBR_KEY, str(request.url))
    # Keys travel in a header only, never in a URL
    for call in api.calls:
        check.is_not_in("PLACEHOLDER", str(call.request.url))
    bh_request = by_host["breach.house"][0]
    check.equal(bh_request.url.params["q"], "example.com")
    check.equal(bh_request.url.params["limit"], "25")
    check.equal(bh_request.headers["User-Agent"].split("/")[0], "msticpy-darkeye")


def test_only_hibr_key_queries_only_hibr(api):
    """Without a Breach House key that host is not contacted."""
    api.get(_HIBR_DOMAIN).respond(json=_hibr_page([_hibr_item("1", "ransomware_leak")]))
    bh_route = api.get(_BH_SEARCH).respond(json=_bh_body(ransomware=[_bh_item()]))
    row = _lookup(DarkEye(HIBRAuthKey=_HIBR_KEY), "example.com", "dns")

    check.equal(bh_route.call_count, 0)
    check.equal(set(_hosts(api)), {"haveibeenransom.com"})
    check.equal(row["Details"]["Count"], 1)
    check.equal(row["Details"]["Products"]["Breach House"], {"Status": darkeye._NOT_QUERIED})
    check.equal(row["Details"]["Products"]["HaveIBeenRansom"]["Status"], "ok")
    check.is_false(row["Reference"].count("breach.house"))


def test_only_breach_house_key_queries_only_breach_house(api):
    """Without a HaveIBeenRansom key that host is not contacted."""
    hibr_route = api.get(_HIBR_DOMAIN).respond(json=_hibr_page([_hibr_item()]))
    api.get(_BH_SEARCH).respond(json=_bh_body(stealers=[_bh_item("s1")]))
    row = _lookup(DarkEye(BreachHouseAuthKey=_BH_KEY), "example.com", "dns")

    check.equal(hibr_route.call_count, 0)
    check.equal(set(_hosts(api)), {"breach.house"})
    check.equal(row["Details"]["Records"][0]["product"], "Breach House")
    check.equal(row["Details"]["Records"][0]["category"], "stealer")
    check.equal(
        row["Details"]["Products"]["HaveIBeenRansom"], {"Status": darkeye._NOT_QUERIED}
    )


# --- failures --------------------------------------------------------------


_FAILURES = [
    pytest.param(
        {"status_code": 401, "json": {"error": "invalid_token"}}, "HTTP 401", id="401"
    ),
    pytest.param({"status_code": 403}, "HTTP 403", id="403"),
    pytest.param({"status_code": 429}, "query limit", id="429"),
    pytest.param({"status_code": 500}, "HTTP 500", id="500"),
    pytest.param({"status_code": 200, "text": "<html>"}, "invalid JSON", id="not json"),
    pytest.param({"status_code": 200, "json": ["a list"]}, "unexpected response", id="list"),
    pytest.param(
        {"status_code": 200, "json": {"success": False}},
        "unexpected response",
        id="success false",
    ),
    pytest.param(
        {"status_code": 200, "json": {"error": "x"}}, "unexpected response", id="error key"
    ),
    pytest.param({"status_code": 200, "json": {}}, "unexpected response", id="no sections"),
]


@pytest.mark.parametrize(("failure", "message"), _FAILURES)
def test_hibr_fails_breach_house_records_are_returned(api, both, failure, message):
    """If HaveIBeenRansom fails the Breach House records are returned and the failure shown."""
    api.get(_HIBR_DOMAIN).respond(**failure)
    api.get(_BH_SEARCH).respond(json=_bh_body(ransomware=[_bh_item("r1")]))
    row = _lookup(both, "example.com", "dns")

    check.equal(row["Status"], LookupStatus.OK.value)
    check.is_true(row["Result"])
    details = row["Details"]
    check.equal([r["product"] for r in details["Records"]], ["Breach House"])
    check.equal(details["Products"]["HaveIBeenRansom"]["Status"], "error")
    check.equal(details["Products"]["Breach House"]["Status"], "ok")
    check.equal(len(details["Errors"]), 1)
    check.is_true(details["Errors"][0].startswith("HaveIBeenRansom: "))
    check.is_in(message, details["Errors"][0])


@pytest.mark.parametrize(("failure", "message"), _FAILURES)
def test_breach_house_fails_hibr_records_are_returned(api, both, failure, message):
    """If Breach House fails the HaveIBeenRansom records are returned and the failure shown."""
    api.get(_HIBR_DOMAIN).respond(json=_hibr_page([_hibr_item("1", "ransomware_leak")]))
    api.get(_BH_SEARCH).respond(**failure)
    row = _lookup(both, "example.com", "dns")

    check.equal(row["Status"], LookupStatus.OK.value)
    check.is_true(row["Result"])
    details = row["Details"]
    check.equal([r["product"] for r in details["Records"]], ["HaveIBeenRansom"])
    check.equal(details["Products"]["Breach House"]["Status"], "error")
    check.is_true(details["Errors"][0].startswith("Breach House: "))
    check.is_in(message, details["Errors"][0])


def test_transport_errors_do_not_echo_the_key(api, both, caplog):
    """Exception text may carry request details: only its type is reported, and never logged."""
    caplog.set_level(logging.DEBUG)
    api.get(_HIBR_DOMAIN).mock(side_effect=httpx.ConnectError(f"boom Bearer {_HIBR_KEY}"))
    api.get(_BH_SEARCH).mock(side_effect=httpx.ReadTimeout(f"slow Bearer {_BH_KEY}"))
    row = _lookup(both, "example.com", "dns")

    check.equal(row["Status"], LookupStatus.QUERY_FAILED.value)
    check.is_false(row["Result"])
    check.equal(row["Severity"], ResultSeverity.unknown.name)
    check.is_in("HaveIBeenRansom: request failed (ConnectError)", row["Details"])
    check.is_in("Breach House: request timed out", row["Details"])
    for text in (str(row), caplog.text):
        check.is_not_in(_HIBR_KEY, text)
        check.is_not_in(_BH_KEY, text)


def test_all_products_failing_is_a_failure_not_an_empty_result(api, both):
    """When every product fails the result is a failure, which is not 'not found'."""
    api.get(_HIBR_DOMAIN).respond(status_code=401)
    api.get(_BH_SEARCH).respond(status_code=429)
    row = _lookup(both, "example.com", "dns")

    check.equal(row["Status"], LookupStatus.QUERY_FAILED.value)
    check.is_false(row["Result"])
    check.equal(row["Severity"], ResultSeverity.unknown.name)
    check.is_in("HaveIBeenRansom: HTTP 401", row["Details"])
    check.is_in("Breach House: HTTP 429", row["Details"])


def test_single_configured_product_failing_is_a_failure(api):
    """With one key, a failure of that product is a failure of the lookup."""
    api.get(_HIBR_DOMAIN).respond(status_code=403)
    row = _lookup(DarkEye(HIBRAuthKey=_HIBR_KEY), "example.com", "dns")

    check.equal(row["Status"], LookupStatus.QUERY_FAILED.value)
    check.is_false(row["Result"])


def test_hibr_second_page_failing_keeps_first_page_records(api, both):
    """Records read before a failing page are kept and the failure is reported."""
    api.get(_HIBR_DOMAIN).respond(json=_hibr_page([_hibr_item("1")], has_next=True))
    api.get(f"{_HIBR_DOMAIN}/2").respond(status_code=500)
    api.get(_BH_SEARCH).respond(json=_bh_body())
    row = _lookup(both, "example.com", "dns")

    check.equal([r["id"] for r in row["Details"]["Records"]], ["1"])
    check.is_in("HTTP 500", row["Details"]["Errors"][0])


def test_redirect_is_not_followed_and_key_stays_on_its_host(api, both):
    """A redirect must not carry the Authorization header to another host."""
    other = api.get("https://attacker.example/steal").respond(json={})
    api.get(_HIBR_DOMAIN).respond(
        status_code=302, headers={"Location": "https://attacker.example/steal"}
    )
    api.get(_BH_SEARCH).respond(json=_bh_body(ransomware=[_bh_item()]))
    row = _lookup(both, "example.com", "dns")

    check.equal(other.call_count, 0)
    check.is_not_in("attacker.example", set(_hosts(api)))
    check.is_in("HTTP 302", row["Details"]["Errors"][0])
    check.equal(row["Details"]["Count"], 1)


# --- empty results ---------------------------------------------------------


def test_empty_results_are_a_negative_result(api, both):
    """Both products answer with nothing: Result False, Status OK, information."""
    api.get(_HIBR_DOMAIN).respond(json=_hibr_page([]))
    api.get(_BH_SEARCH).respond(
        json=_bh_body(ransomware=[], breaches=[], leads=[], stealers=[])
    )
    row = _lookup(both, "example.com", "dns")

    check.equal(row["Status"], LookupStatus.OK.value)
    check.is_false(row["Result"])
    check.equal(row["Severity"], ResultSeverity.information.name)
    check.equal(row["Details"]["Count"], 0)
    check.equal(row["Details"]["Records"], [])
    check.equal(row["Details"]["Products"]["HaveIBeenRansom"]["Status"], "ok")
    check.equal(row["Details"]["Products"]["Breach House"]["Status"], "ok")


def test_breach_house_records_for_other_domains_are_dropped(api):
    """Free-text hits that are about another domain are not results."""
    api.get(_BH_SEARCH).respond(
        json=_bh_body(ransomware=[_bh_item("x", website="notexample.com")])
    )
    row = _lookup(DarkEye(BreachHouseAuthKey=_BH_KEY), "example.com", "dns")

    check.is_false(row["Result"])
    check.equal(row["Status"], LookupStatus.OK.value)
    check.equal(row["Details"]["Count"], 0)


# --- hostname matching of Breach House -------------------------------------


def test_breach_house_hostname_boundary(api):
    """Breach House is free text: only the host, its subdomains or a standalone title hit."""
    items = [
        _bh_item("a", website="example.com"),
        _bh_item("b", website="mail.example.com"),
        _bh_item("c", website="notexample.com"),
        _bh_item("d", website="example.com.evil.net"),
        _bh_item("e", website="", post_title="Leak of example.com data"),
        _bh_item("f", website="", post_title="notexample.com only"),
        _bh_item("g", website="", post_title="see example.com.other.org"),
        _bh_item("h", website="", post_title="mail.example.com leak"),
        _bh_item("i", website="", post_title=""),
        _bh_item("j", website="WWW.EXAMPLE.COM.", post_title="Upper"),
    ]
    api.get(_BH_SEARCH).respond(json=_bh_body(ransomware=items))
    row = _lookup(DarkEye(BreachHouseAuthKey=_BH_KEY), "example.com", "dns")

    check.equal([r["id"] for r in row["Details"]["Records"]], ["a", "b", "e", "h", "j"])


def test_hostname_is_normalised(api):
    """A hostname is lower-cased before it is sent, and a trailing dot is dropped."""
    check.equal(darkeye._hostname_target(" Example.COM. "), "example.com")
    check.equal(darkeye._hostname_target("bücher.example"), "xn--bcher-kva.example")
    check.is_none(darkeye._hostname_target("a" * 64 + ".com"))
    check.is_none(darkeye._hostname_target("1.2.3.4"))
    api.get(_BH_SEARCH).respond(json=_bh_body(ransomware=[_bh_item("a")]))
    row = _lookup(DarkEye(BreachHouseAuthKey=_BH_KEY), "EXAMPLE.com", "hostname")

    check.equal(api.calls.last.request.url.params["q"], "example.com")
    check.equal(row["Details"]["Count"], 1)


@pytest.mark.parametrize("ioc", ["exa*mple.com", "example.c?m", "-bad-.example.com", "a..com"])
def test_wildcards_and_bad_hostnames_send_nothing(api, both, ioc):
    """Both APIs treat * and ? as wildcards: such values are refused before any request."""
    row = _lookup(both, ioc, "dns")

    check.equal(row["Status"], LookupStatus.BAD_FORMAT.value)
    check.is_false(row["Result"])
    check.equal(len(api.calls), 0)


# --- email -----------------------------------------------------------------


def test_email_is_looked_up_with_exact_match_filter(api, both):
    """HaveIBeenRansom is queried by email and Breach House hits must name the exact address."""
    api.get(_HIBR_EMAIL).respond(
        json=_hibr_page([_hibr_item("7", "ransomware_leak", website="")])
    )
    api.get(_BH_SEARCH).respond(
        json=_bh_body(
            ransomware=[
                _bh_item("a", website="", post_title="bob@example.com in a combo list"),
                _bh_item("b", website="", post_title="xbob@example.com"),
                _bh_item("c", website="", post_title="bob@example.com.au"),
                _bh_item("d", website="", post_title="BOB@EXAMPLE.COM"),
                _bh_item("e", website="", post_title="alice@example.com"),
                _bh_item("f", website="", post_title="bob@example.com-old"),
                _bh_item("g", website="", post_title="", description="mail bob@example.com."),
            ],
            stealers=[_bh_item("s", website="", post_title="", name="bob@example.com")],
        )
    )
    row = _lookup(both, "bob@example.com", "email")

    check.equal(api.calls[0].request.url.raw_path, b"/api/metadata/email/bob%40example.com")
    bh_request = [c.request for c in api.calls if c.request.url.host == "breach.house"][0]
    check.equal(bh_request.url.params["q"], "bob@example.com")
    records = row["Details"]["Records"]
    check.equal(
        [(r["product"], r["id"]) for r in records],
        [
            ("HaveIBeenRansom", "7"),
            ("Breach House", "a"),
            ("Breach House", "d"),
            ("Breach House", "g"),
            ("Breach House", "s"),
        ],
    )
    # The description is only read to match; it is never copied into a record
    check.is_not_in("mail bob", json.dumps(records))


@pytest.mark.parametrize("ioc", ["*@example.com", "bob?@example.com", "bob@*.example.com"])
def test_email_wildcards_send_nothing(api, both, ioc):
    """An email with a wildcard is refused before any request."""
    row = _lookup(both, ioc, "email")

    check.equal(row["Status"], LookupStatus.BAD_FORMAT.value)
    check.equal(len(api.calls), 0)


# --- unsupported -----------------------------------------------------------


@pytest.mark.parametrize(
    ("ioc", "ioc_type"),
    [
        ("185.92.220.35", "ipv4"),
        ("https://example.com/a", "url"),
        ("7657fcb7d772448a6d8504e4b20168b7", "md5_hash"),
    ],
)
def test_unsupported_types_send_nothing(api, both, ioc, ioc_type):
    """Only dns, hostname and email are supported."""
    row = _lookup(both, ioc, ioc_type)

    check.equal(row["Status"], LookupStatus.NOT_SUPPORTED.value)
    check.equal(len(api.calls), 0)
    check.equal(sorted(both.supported_types), ["dns", "email", "hostname"])


def test_query_subtypes_are_not_supported(api, both):
    """DarkEye always combines both products: there are no query subtypes."""
    row = _lookup(both, "example.com", "dns", query_type="quick")

    check.is_false(row["Result"])
    check.is_in("does not support", str(row["Details"]))
    check.equal(len(api.calls), 0)


# --- data rules ------------------------------------------------------------


def test_sensitive_fields_are_never_returned(api, both):
    """Credential, description, leak-site and download fields are never read into a result."""
    api.get(_HIBR_DOMAIN).respond(
        json=_hibr_page([_hibr_item("1", "ransomware_leak", **_SECRET_FIELDS)])
    )
    api.get(_BH_SEARCH).respond(
        json=_bh_body(
            ransomware=[_bh_item("r1", **_SECRET_FIELDS)],
            stealers=[_bh_item("s1", **_SECRET_FIELDS)],
        )
    )
    frame = both.lookup_ioc(ioc="example.com", ioc_type="dns")
    row = frame.to_dict(orient="records")[0]

    everything = json.dumps(row, default=str) + frame.to_json()
    for field, value in _SECRET_FIELDS.items():
        check.is_not_in(field, everything)
        check.is_not_in(str(value), everything)
        if isinstance(value, dict):
            check.is_not_in("SECRET-HIBR-BLOCK", everything)
    for record in row["Details"]["Records"]:
        check.equal(set(record), set(dict(DarkEyeRecord(product="p", category="c"))))
    check.equal(set(row["RawResult"]), {"Records", "Products"})


def test_only_listing_endpoints_are_requested(api, both):
    """No stealer-content endpoint is used: only the metadata and search endpoints."""
    api.get(_HIBR_DOMAIN).respond(json=_hibr_page([_hibr_item()]))
    api.get(_BH_SEARCH).respond(json=_bh_body(stealers=[_bh_item("s1")]))
    _lookup(both, "example.com", "dns")
    api.get(_HIBR_EMAIL).respond(json=_hibr_page([_hibr_item()]))
    _lookup(DarkEye(HIBRAuthKey=_HIBR_KEY), "bob@example.com", "email")

    for call in api.calls:
        url = call.request.url
        if url.host == "haveibeenransom.com":
            check.is_true(
                url.path.startswith(("/api/metadata/domain/", "/api/metadata/email/")),
                url.path,
            )
        else:
            check.equal((url.host, url.path), ("breach.house", "/api/v1/search"))
        check.is_not_in("stealer", url.path)
        check.equal(call.request.method, "GET")


def test_text_is_cleaned_and_bounded(api):
    """Control characters are dropped, lookalike characters normalised, length capped."""
    api.get(_BH_SEARCH).respond(
        json=_bh_body(
            ransomware=[
                _bh_item(
                    "a",
                    post_title="Ｅxample‮\x00  Corp\n" + "x" * 500,
                    group_name="ᴀbc",
                    country=None,
                    discovered="2024-05-06 01:02:03",
                )
            ]
        )
    )
    row = _lookup(DarkEye(BreachHouseAuthKey=_BH_KEY), "example.com", "dns")
    record = row["Details"]["Records"][0]

    check.is_true(record["title"].startswith("Example Corp x"))
    check.is_true(len(record["title"]) <= 256)
    check.is_not_in("\x00", record["title"])
    check.is_not_in("‮", record["title"])
    check.equal(record["country"], "")
    check.equal(record["discovered"], "2024-05-06")


# --- severity --------------------------------------------------------------


@pytest.mark.parametrize(
    ("sections", "expected"),
    [
        ({"ransomware": []}, ResultSeverity.information),
        ({"leads": [_bh_item("l")]}, ResultSeverity.information),
        ({"ransomware": [_bh_item("r")]}, ResultSeverity.warning),
        ({"breaches": [_bh_item("b")]}, ResultSeverity.warning),
        ({"stealers": [_bh_item("s")]}, ResultSeverity.warning),
        ({"leads": [_bh_item("l")], "stealers": [_bh_item("s")]}, ResultSeverity.warning),
    ],
    ids=["none", "lead only", "ransomware", "breach", "stealer", "lead and stealer"],
)
def test_severity_mapping(api, sections, expected):
    """No record or leads only is information, any other record is warning, never high."""
    api.get(_BH_SEARCH).respond(json=_bh_body(**sections))
    row = _lookup(DarkEye(BreachHouseAuthKey=_BH_KEY), "example.com", "dns")

    check.equal(row["Severity"], expected.name)
    check.not_equal(row["Severity"], ResultSeverity.high.name)
    check.equal(row["Result"], any(sections.values()))


def test_hibr_unknown_leak_type_is_a_leak_and_a_warning(api):
    """An unknown HaveIBeenRansom leak_type is reported as a plain leak."""
    api.get(_HIBR_DOMAIN).respond(json=_hibr_page([_hibr_item("1", "something_new")]))
    row = _lookup(DarkEye(HIBRAuthKey=_HIBR_KEY), "example.com", "dns")

    check.equal(row["Details"]["Records"][0]["category"], "leak")
    check.equal(row["Severity"], ResultSeverity.warning.name)


# --- paging and limits -----------------------------------------------------


def test_hibr_pagination_dedup_and_documented_shape(api):
    """Pages are followed while has_next is true, repeated ids are dropped."""
    api.get(_HIBR_DOMAIN).respond(
        json=_hibr_page([_hibr_item("1"), _hibr_item("2", "ransomware_leak")], has_next=True)
    )
    api.get(f"{_HIBR_DOMAIN}/2").respond(
        json=_hibr_page([_hibr_item("2", "ransomware_leak"), _hibr_item("3")], has_next=True)
    )
    # an empty page can still have a next page
    api.get(f"{_HIBR_DOMAIN}/3").respond(json=_hibr_page([], has_next=True))
    api.get(f"{_HIBR_DOMAIN}/4").respond(
        json=_hibr_page([_hibr_item("4")], has_next=False, documented=True)
    )
    row = _lookup(DarkEye(HIBRAuthKey=_HIBR_KEY), "example.com", "dns")

    check.equal([r["id"] for r in row["Details"]["Records"]], ["1", "2", "3", "4"])
    check.equal(len(api.calls), 4)
    check.is_false(row["Details"]["Truncated"])


def test_hibr_max_pages_marks_truncated(api):
    """Reading stops at the page cap and the result says it may be incomplete."""
    for page in range(1, 8):
        url = _HIBR_DOMAIN if page == 1 else f"{_HIBR_DOMAIN}/{page}"
        api.get(url).respond(json=_hibr_page([_hibr_item(str(page))], has_next=True))
    row = _lookup(DarkEye(HIBRAuthKey=_HIBR_KEY), "example.com", "dns")

    check.equal(len(api.calls), darkeye._MAX_PAGES)
    check.equal(row["Details"]["Count"], darkeye._MAX_PAGES)
    check.is_true(row["Details"]["Truncated"])
    check.is_true(row["Details"]["Products"]["HaveIBeenRansom"]["Truncated"])


def test_hibr_record_cap_stops_paging(api):
    """No more than the record cap is kept per product, and no more pages are read."""
    items = [_hibr_item(str(i)) for i in range(darkeye._MAX_RECORDS + 5)]
    api.get(_HIBR_DOMAIN).respond(json=_hibr_page(items, has_next=True))
    row = _lookup(DarkEye(HIBRAuthKey=_HIBR_KEY), "example.com", "dns")

    check.equal(row["Details"]["Count"], darkeye._MAX_RECORDS)
    check.is_true(row["Details"]["Truncated"])
    check.equal(len(api.calls), 1)


def test_breach_house_total_larger_than_page_marks_truncated(api):
    """A Breach House total larger than the results returned means more exist."""
    body = {
        "ransomware": {"results": [_bh_item("a")], "total": 99},
        "breaches": [_bh_item("b")],
    }
    api.get(_BH_SEARCH).respond(json=body)
    row = _lookup(DarkEye(BreachHouseAuthKey=_BH_KEY), "example.com", "dns")

    check.is_true(row["Details"]["Truncated"])
    check.equal(row["Details"]["Count"], 2)


def test_breach_house_full_page_without_total_marks_truncated(api):
    """A section as long as the limit, with no total, may have more."""
    items = [_bh_item(str(i)) for i in range(darkeye._BREACH_HOUSE_LIMIT)]
    api.get(_BH_SEARCH).respond(json={"ransomware": items})
    row = _lookup(DarkEye(BreachHouseAuthKey=_BH_KEY), "example.com", "dns")

    check.is_true(row["Details"]["Truncated"])


def test_breach_house_record_cap_and_dedup(api, monkeypatch):
    """The record cap applies to Breach House too and a repeated id is dropped."""
    monkeypatch.setattr(darkeye, "_MAX_RECORDS", 2)
    items = [_bh_item("a"), _bh_item("a"), _bh_item("b"), _bh_item("c")]
    api.get(_BH_SEARCH).respond(json=_bh_body(ransomware=items))
    row = _lookup(DarkEye(BreachHouseAuthKey=_BH_KEY), "example.com", "dns")

    check.equal([r["id"] for r in row["Details"]["Records"]], ["a", "b"])
    check.is_true(row["Details"]["Truncated"])


def test_same_id_in_two_breach_house_categories_is_kept_twice(api):
    """Deduplication is per category: the same id may be a breach and a lead."""
    api.get(_BH_SEARCH).respond(json=_bh_body(breaches=[_bh_item("1")], leads=[_bh_item("1")]))
    row = _lookup(DarkEye(BreachHouseAuthKey=_BH_KEY), "example.com", "dns")

    check.equal([r["category"] for r in row["Details"]["Records"]], ["breach", "lead"])


@pytest.mark.parametrize(
    "bad_page",
    [
        {"sources": [_hibr_item()]},  # no has_next
        {"sources": "nope", "has_next": False},
        {"sources": [_hibr_item(), "x"], "has_next": False},
        {"sources": [{"leak_type": "traditional_breach"}], "has_next": False},  # no id/title
        {"has_next": False},
    ],
    ids=["no has_next", "sources not list", "item not dict", "empty item", "no sources"],
)
def test_malformed_hibr_responses_are_errors(api, bad_page):
    """A response that does not have the expected shape is an error, not an empty result."""
    api.get(_HIBR_DOMAIN).respond(json=bad_page)
    row = _lookup(DarkEye(HIBRAuthKey=_HIBR_KEY), "example.com", "dns")

    check.equal(row["Status"], LookupStatus.QUERY_FAILED.value)
    check.is_in("unexpected response", row["Details"])


@pytest.mark.parametrize(
    "bad_body",
    [
        {"ransomware": "nope"},
        {"ransomware": {"results": "nope", "total": 1}},
        {"ransomware": [_bh_item(), "x"]},
        {"unrelated": []},
    ],
    ids=["str section", "results not list", "item not dict", "no known section"],
)
def test_malformed_breach_house_responses_are_errors(api, bad_body):
    """A Breach House body that does not have the expected shape is an error."""
    api.get(_BH_SEARCH).respond(json=bad_body)
    row = _lookup(DarkEye(BreachHouseAuthKey=_BH_KEY), "example.com", "dns")

    check.equal(row["Status"], LookupStatus.QUERY_FAILED.value)
    check.is_in("unexpected response", row["Details"])


# --- parse_results ---------------------------------------------------------


def test_parse_results_of_a_built_result(api, both):
    """parse_results reports the same positive and severity from a built result."""
    api.get(_HIBR_DOMAIN).respond(json=_hibr_page([_hibr_item("1", "ransomware_leak")]))
    api.get(_BH_SEARCH).respond(json=_bh_body())
    frame = both.lookup_ioc(ioc="example.com", ioc_type="dns")
    row = frame.to_dict(orient="records")[0]

    positive, severity, details = both.parse_results(row)
    check.is_true(positive)
    check.equal(severity, ResultSeverity.warning)
    check.equal(details["Count"], 1)
    check.equal(
        both.parse_results({"Status": 3}), (False, ResultSeverity.information, "Not found.")
    )


# --- through TILookup and the config ---------------------------------------


def test_loads_from_config_and_looks_up_through_tilookup(api):
    """The provider loads from msticpyconfig, gets both keys and returns one result."""
    config_path = Path(get_test_data_path()).parent.joinpath("msticpyconfig-test.yaml")
    with custom_mp_config(config_path):
        with warnings.catch_warnings():
            warnings.simplefilter("ignore", category=UserWarning)
            ti_lookup = TILookup()
    provider = ti_lookup.loaded_providers["DarkEye"]
    check.is_instance(provider, DarkEye)
    # These only arrive if lookup.py lets the two key names through
    check.equal(provider._hibr_key, "[PLACEHOLDER]")
    check.equal(provider._breach_house_key, "[PLACEHOLDER]")

    api.get(_HIBR_DOMAIN).respond(json=_hibr_page([_hibr_item("1", "ransomware_leak")]))
    api.get(_BH_SEARCH).respond(json=_bh_body(leads=[_bh_item("l1")]))
    result = ti_lookup.lookup_ioc(
        ioc="example.com", ioc_type="dns", providers=["DarkEye"], show_not_supported=True
    )

    check.equal(len(result), 1)
    row = result.to_dict(orient="records")[0]
    check.equal(row["Provider"], "DarkEye")
    check.is_true(row["Result"])
    check.equal(row["Details"]["Count"], 2)
    for call in api.calls:
        check.equal(call.request.headers["Authorization"], "Bearer [PLACEHOLDER]")

    # a mixed batch: unsupported types are left out, supported ones are looked up
    batch = ti_lookup.lookup_iocs(
        data={"example.com": "dns", "185.92.220.35": "ipv4"}, providers=["DarkEye"]
    )
    check.equal(list(batch["Ioc"]), ["example.com"])
    ti_lookup.browse_results(batch, severities=["information", "warning", "high"])


@pytest.mark.parametrize(
    "args",
    [
        {"HIBRAuthKey": "[PLACEHOLDER]", "BreachHouseAuthKey": "[PLACEHOLDER]"},
        {"HIBRAuthKey": "[PLACEHOLDER]"},
        {"BreachHouseAuthKey": {"EnvironmentVar": "DARKEYE_BH_KEY"}},
    ],
    ids=["both keys", "HIBR only", "Breach House only, from environment"],
)
def test_default_config_accepts_each_key_as_optional(args):
    """The shipped config definition accepts either key or both for DarkEye."""
    conf_settings = {
        "TIProviders": {
            "DarkEye": {"Args": args, "Primary": True, "Provider": "DarkEye"},
        }
    }
    controls = MpConfigControls(
        mp_config_def=get_mpconfig_definitions(), mp_config=conf_settings
    )

    results = controls.validate_setting("TIProviders.DarkEye")
    check.equal(results, [])
    check.is_in("DarkEye", get_mpconfig_definitions()["TIProviders"])
