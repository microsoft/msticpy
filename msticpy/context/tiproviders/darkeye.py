# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
DarkEye Provider.

Looks up domains, hostnames and email addresses in DarkEye (https://darkeye.io).
DarkEye has two products with a paid API, and this one provider covers both:

- HaveIBeenRansom (https://haveibeenransom.com) indexes ransomware leaks and
  data breaches.
- Breach House (https://breach.house) lists ransomware victims, data breaches,
  leads and stealer logs.

Each product has its own optional key (``HIBRAuthKey`` and
``BreachHouseAuthKey``). A product without a key is not queried and its host is
never contacted. At least one key is required. A lookup returns one combined
result in which every record states its product and its category
(``ransomware``, ``breach``, ``leak``, ``stealer`` or ``lead``).

Only listing metadata is read (id, title, website, group, country and date).
Nothing else in a response is kept, so no password or credential field is ever
returned, and HaveIBeenRansom's stealer-content endpoint is not used.

Input can be a single IoC observable or a pandas DataFrame containing
multiple observables.

"""

from __future__ import annotations

import json
import logging
import re
import unicodedata
from typing import Any, ClassVar
from urllib.parse import quote

import httpx
from pydantic import BaseModel, Field
from typing_extensions import Self

from ..._version import VERSION
from ...common.exceptions import MsticpyConfigError
from ...common.pkg_config import get_http_timeout
from ..http_provider import APILookupParams
from ..lookup_result import LookupStatus
from .ti_http_provider import HttpTIProvider
from .ti_provider_base import ResultSeverity

__version__ = VERSION
__author__ = "Juan Manuel Tejada"

logger: logging.Logger = logging.getLogger(__name__)

# Product names, as they appear in every record.
HIBR: str = "HaveIBeenRansom"
BREACH_HOUSE: str = "Breach House"

_HIBR_HOST: str = "https://haveibeenransom.com"
_HIBR_METADATA_URL: str = f"{_HIBR_HOST}/api/metadata"
_BREACH_HOUSE_SEARCH_URL: str = "https://breach.house/api/v1/search"

# HaveIBeenRansom `leak_type` values. Anything else is reported as a plain leak.
_HIBR_CATEGORIES: dict[str, str] = {
    "ransomware_leak": "ransomware",
    "traditional_breach": "breach",
}
_UNKNOWN_HIBR_CATEGORY: str = "leak"

# Breach House sections of the cross-source search, and the category of each.
_BREACH_HOUSE_CATEGORIES: dict[str, str] = {
    "ransomware": "ransomware",
    "breaches": "breach",
    "leads": "lead",
    "stealers": "stealer",
}
# Breach House text fields that may mention an email address. They are only used
# to check an exact match and are never copied into a record.
_BREACH_HOUSE_MATCH_FIELDS: tuple[str, ...] = (
    "post_title",
    "name",
    "website",
    "description",
    "meta_description",
)

# Bounds on one lookup. HaveIBeenRansom is paged and every page is one request.
_MAX_PAGES: int = 5
_BREACH_HOUSE_LIMIT: int = 25  # results requested per Breach House section
_MAX_RECORDS: int = 100  # records kept per product
_MAX_TEXT: int = 256
_MAX_MATCH_TEXT: int = 4000
_MAX_EMAIL: int = 320
_MAX_HOSTNAME: int = 253

_HOSTNAME_LABEL = re.compile(r"[a-z0-9](?:[a-z0-9-]{0,61}[a-z0-9])?")

_USER_AGENT: str = f"msticpy-darkeye/{VERSION}"

_NOT_QUERIED: str = "Not queried: no API key configured."


class DarkEyeRecord(BaseModel):
    """Listing metadata of one DarkEye record. Nothing else is kept."""

    product: str
    category: str
    id: str = ""
    title: str = ""
    website: str = ""
    group: str = ""
    country: str = ""
    discovered: str = ""


class _ProductResult(BaseModel):
    """Outcome of the lookup in one product."""

    product: str
    error: str = ""
    truncated: bool = False
    records: list[DarkEyeRecord] = Field(default_factory=list)
    reference: str = ""


class _ProductError(Exception):
    """A product lookup failed. The message is safe to show."""


def _clean_key(name: str, value: str | None) -> str | None:
    """Return the key, or None if it is not set. Raise if it cannot be a key."""
    if not value or not str(value).strip():
        return None
    key: str = str(value).strip()
    if any(ord(char) < 33 or ord(char) > 126 for char in key):
        # The key itself is never put in a message.
        err_msg: str = (
            f"{name} contains whitespace or characters that are not printable ASCII."
        )
        raise MsticpyConfigError(err_msg)
    return key


def _text(value: Any, limit: int = _MAX_TEXT) -> str:
    """Return single-line printable text, or "" for anything but text and numbers."""
    if isinstance(value, bool) or not isinstance(value, str | int):
        return ""
    normalized: str = unicodedata.normalize("NFKC", str(value))
    printable: str = "".join(
        " " if char.isspace() else char
        for char in normalized
        if char.isspace() or unicodedata.category(char)[0] != "C"
    )
    return " ".join(printable.split())[:limit]


def _hostname_target(value: str) -> str | None:
    """Return a plain lower-case ASCII hostname, or None. Wildcards are refused."""
    host: str = value.strip().lower().rstrip(".")
    try:
        host = host.encode("idna").decode("ascii")
    except UnicodeError:
        return None
    labels: list[str] = host.split(".")
    if (
        len(host) > _MAX_HOSTNAME
        or len(labels) < 2
        or labels[-1].isdigit()
        or not all(_HOSTNAME_LABEL.fullmatch(label) for label in labels)
    ):
        return None
    return host


def _email_target(value: str) -> str | None:
    """Return the email address, or None. Wildcards, spaces and controls are refused."""
    email: str = value.strip()
    if (
        not email
        or len(email) > _MAX_EMAIL
        or email.count("@") != 1
        or any(char in "*?" for char in email)
        or any(char.isspace() or unicodedata.category(char)[0] == "C" for char in email)
    ):
        return None
    local, _, domain = email.partition("@")
    if not local or _hostname_target(domain) is None:
        return None
    return email


def _normalize_target(value: str, ioc_type: str) -> str | None:
    """Return the value to send to DarkEye for an observable, or None if not valid."""
    if ioc_type == "email":
        return _email_target(value)
    return _hostname_target(value)


def _mentions_email(texts: list[str], email: str) -> bool:
    """Return True if the exact address appears in one of the texts."""
    # Boundary match: "bob@example.com" must not match "bob@example.com.au" or
    # "xbob@example.com".
    pattern: str = r"(?<![\w.+%@-])" + re.escape(email) + r"(?![\w@+-]|\.\w)"
    return any(re.search(pattern, text, re.IGNORECASE) for text in texts)


def _mentions_host(record: DarkEyeRecord, host: str) -> bool:
    """
    Return True if a Breach House record is about the host, not just near it.

    The search is free text, so a record whose website is another domain only
    mentions the host. With a website it must be the host or one of its
    subdomains. Without one the title is free text and the host must stand alone
    as a hostname: not inside notexample.com or example.com.other.com, but still
    as mail.example.com.
    """
    website: str = record.website.lower().rstrip(".")
    if website:
        return website == host or website.endswith(f".{host}")
    title: str = record.title.lower()
    pattern: str = r"(?<![\w-])" + re.escape(host) + r"(?![\w-]|\.\w)"
    return bool(title) and re.search(pattern, title) is not None


def _is_about(item: dict[str, Any], record: DarkEyeRecord, ioc_type: str, target: str) -> bool:
    """Return True if a Breach House item is about the observable."""
    if ioc_type == "email":
        texts: list[str] = [
            _text(item.get(field), _MAX_MATCH_TEXT) for field in _BREACH_HOUSE_MATCH_FIELDS
        ]
        return _mentions_email(texts, target)
    return _mentions_host(record, target)


def _make_record(product: str, category: str, item: dict[str, Any]) -> DarkEyeRecord:
    """Return the listing metadata of an API item. Nothing else is read."""
    return DarkEyeRecord(
        product=product,
        category=category,
        id=_text(item.get("id_source") or item.get("victim_id") or item.get("id")),
        title=_text(item.get("post_title") or item.get("name")),
        website=_text(item.get("website")),
        group=_text(item.get("group_name")),
        country=_text(item.get("country")),
        discovered=_text(item.get("discovered"))[:10],
    )


def _record_identity(record: DarkEyeRecord) -> str:
    """Return a key that identifies a record within its product and category."""
    return f"{record.category}|{record.id or json.dumps(dict(record), sort_keys=True)}"


class DarkEye(HttpTIProvider):
    """DarkEye Lookup (HaveIBeenRansom and Breach House)."""

    # `_QUERIES` declares the supported observable types and the HaveIBeenRansom
    # path of each. Breach House has one search endpoint for every type, see
    # `_BREACH_HOUSE_SEARCH_URL`. There are no query subtypes: both products are
    # always combined into one result.
    _BASE_URL: ClassVar[str] = _HIBR_HOST
    _QUERIES: ClassVar[dict[str, APILookupParams]] = {
        "dns": APILookupParams(path="/api/metadata/domain/{observable}"),
        "hostname": APILookupParams(path="/api/metadata/domain/{observable}"),
        "email": APILookupParams(path="/api/metadata/email/{observable}"),
    }

    def __init__(
        self: Self,
        *,
        timeout: int | None = None,
        HIBRAuthKey: str | None = None,  # noqa: N803
        BreachHouseAuthKey: str | None = None,  # noqa: N803
    ) -> None:
        """
        Initialize the provider.

        Parameters
        ----------
        timeout : int, optional
            Timeout of each HTTP request, by default None
        HIBRAuthKey : str, optional
            HaveIBeenRansom API key. Without it HaveIBeenRansom is not queried.
        BreachHouseAuthKey : str, optional
            Breach House API token. Without it Breach House is not queried.

        Raises
        ------
        MsticpyConfigError
            If neither key is set, or a key contains characters that cannot be
            sent in an HTTP header.

        """
        super().__init__(timeout=timeout)
        self._hibr_key: str | None = _clean_key("HIBRAuthKey", HIBRAuthKey)
        self._breach_house_key: str | None = _clean_key(
            "BreachHouseAuthKey", BreachHouseAuthKey
        )
        if not (self._hibr_key or self._breach_house_key):
            err_msg: str = (
                "DarkEye needs at least one API key: set HIBRAuthKey "
                "(HaveIBeenRansom) and/or BreachHouseAuthKey (Breach House)."
            )
            raise MsticpyConfigError(err_msg)

    def _run_ti_lookup_query(
        self: Self,
        result: dict[str, Any],
        *,
        query_type: str | None = None,
        timeout: int | None = None,
    ) -> dict[str, Any]:
        """
        Look up an observable in every product that has a key.

        The base implementation issues one request to one host. DarkEye has two
        hosts, each with its own key, and the answer is one combined result, so
        the lookup is overridden. Each response is reduced to listing metadata as it
        is read, and nothing else from it is kept.

        Parameters
        ----------
        result : dict
            Dict result with resolved type and pre-processed item.
            Status is non-zero on failure.
        query_type : str, optional
            Not supported. DarkEye has no query subtypes.
        timeout : int, optional
            Timeout to use for lookups

        Returns
        -------
        dict[str, Any]
            The lookup result.

        Raises
        ------
        LookupError
            If a query subtype is requested.

        """
        if result["Status"] != LookupStatus.OK.value or result["SafeIoc"] is None:
            # Not a supported type, or the pre-processing rejected the value
            return result
        ioc_type: str = result["IocType"]
        if query_type:
            err_msg: str = f"Provider does not support this type {ioc_type}-{query_type}."
            raise LookupError(err_msg)

        target: str | None = _normalize_target(result["SafeIoc"], ioc_type)
        if target is None:
            result["Status"] = LookupStatus.BAD_FORMAT.value
            result["Details"] = f"Observable is not a valid {ioc_type} for DarkEye."
            return result

        products: list[_ProductResult] = []
        if self._hibr_key:
            product = _ProductResult(product=HIBR)
            self._query_hibr(product, ioc_type, target, timeout)
            products.append(product)
        if self._breach_house_key:
            product = _ProductResult(product=BREACH_HOUSE)
            self._query_breach_house(product, ioc_type, target, timeout)
            products.append(product)
        self._fill_result(result, products)
        return result

    def _get_json(
        self: Self,
        url: str,
        key: str,
        *,
        params: dict[str, Any] | None = None,
        timeout: int | None = None,
    ) -> dict[str, Any]:
        """Return the JSON object of a GET request, or raise _ProductError."""
        try:
            response: httpx.Response = self._httpx_client.get(
                url=url,
                params=params,
                # The key goes in this request's header only and the URL is always
                # on the key's own host. Redirects are refused so that a redirect
                # cannot carry the header to another host.
                headers={
                    "Authorization": f"Bearer {key}",
                    "Accept": "application/json",
                    "User-Agent": _USER_AGENT,
                },
                follow_redirects=False,
                timeout=get_http_timeout(timeout=timeout),
            )
        except httpx.TimeoutException:
            raise _ProductError("request timed out") from None
        except httpx.HTTPError as err:
            # The exception text can carry request details: report only its type.
            err_msg: str = f"request failed ({type(err).__name__})"
            raise _ProductError(err_msg) from None

        status: int = response.status_code
        if status in (httpx.codes.UNAUTHORIZED, httpx.codes.FORBIDDEN):
            err_msg = f"HTTP {status} (check the API key and plan)"
            raise _ProductError(err_msg)
        if status == httpx.codes.TOO_MANY_REQUESTS:
            err_msg = f"HTTP {status} (query limit reached)"
            raise _ProductError(err_msg)
        if status != httpx.codes.OK:
            err_msg = f"HTTP {status}"
            raise _ProductError(err_msg)
        try:
            data: Any = response.json()
        except ValueError:
            raise _ProductError("invalid JSON response") from None
        if not isinstance(data, dict) or data.get("success") is False or "error" in data:
            raise _ProductError("unexpected response")
        return data

    def _query_hibr(
        self: Self,
        product: _ProductResult,
        ioc_type: str,
        target: str,
        timeout: int | None,
    ) -> None:
        """Read HaveIBeenRansom metadata (ransomware leaks and data breaches)."""
        base_url: str = (self._BASE_URL + self._QUERIES[ioc_type].path).format(
            observable=quote(target, safe="")
        )
        product.reference = base_url
        try:
            self._read_hibr_pages(product, base_url, timeout)
        except _ProductError as err:
            product.error = str(err)
            logger.info("DarkEye %s lookup failed: %s", HIBR, product.error)

    def _read_hibr_pages(
        self: Self,
        product: _ProductResult,
        base_url: str,
        timeout: int | None,
    ) -> None:
        """Read the pages of a HaveIBeenRansom lookup into the product result."""
        seen: set[str] = set()
        for page in range(1, _MAX_PAGES + 1):
            data = self._get_json(
                base_url if page == 1 else f"{base_url}/{page}",
                str(self._hibr_key),
                timeout=timeout,
            )
            items: Any = data["results"] if "results" in data else data.get("sources")
            pagination: Any = data.get("pagination")
            has_next: Any = data.get("has_next")
            if has_next is None and isinstance(pagination, dict):
                has_next = pagination.get("has_next")
            if (
                not isinstance(items, list)
                or not isinstance(has_next, bool)
                or any(not isinstance(item, dict) for item in items)
            ):
                raise _ProductError("unexpected response")
            for item in items:
                category: str = _HIBR_CATEGORIES.get(
                    _text(item.get("leak_type")), _UNKNOWN_HIBR_CATEGORY
                )
                record = _make_record(HIBR, category, item)
                if not (record.id or record.title or record.website):
                    raise _ProductError("unexpected response")
                if _record_identity(record) in seen:
                    continue
                seen.add(_record_identity(record))
                if len(product.records) >= _MAX_RECORDS:
                    product.truncated = True
                    return
                product.records.append(record)
            if not has_next:
                return
        product.truncated = True

    def _query_breach_house(
        self: Self,
        product: _ProductResult,
        ioc_type: str,
        target: str,
        timeout: int | None,
    ) -> None:
        """Search Breach House (ransomware, breaches, leads and stealers)."""
        product.reference = f"{_BREACH_HOUSE_SEARCH_URL}?q={quote(target, safe='')}"
        try:
            data = self._get_json(
                _BREACH_HOUSE_SEARCH_URL,
                str(self._breach_house_key),
                params={"q": target, "limit": _BREACH_HOUSE_LIMIT},
                timeout=timeout,
            )
            self._read_breach_house_sections(product, data, ioc_type, target)
        except _ProductError as err:
            product.error = str(err)
            logger.info("DarkEye %s lookup failed: %s", BREACH_HOUSE, product.error)

    @staticmethod
    def _read_breach_house_sections(
        product: _ProductResult,
        data: dict[str, Any],
        ioc_type: str,
        target: str,
    ) -> None:
        """
        Read the sections of a Breach House search into the product result.

        The search is free text over the victim and listing fields, so a record
        is kept only when it is about the observable: the exact address for an
        email, the host or one of its subdomains for a hostname.
        """
        if not any(source in data for source in _BREACH_HOUSE_CATEGORIES):
            raise _ProductError("unexpected response")
        seen: set[str] = set()
        for source, category in _BREACH_HOUSE_CATEGORIES.items():
            section: Any = data.get(source)
            if section is None:
                continue
            total: Any = None
            if isinstance(section, dict):
                total, section = section.get("total"), section.get("results")
            if not isinstance(section, list) or any(not isinstance(i, dict) for i in section):
                raise _ProductError("unexpected response")
            if isinstance(total, int) and not isinstance(total, bool):
                product.truncated |= total > len(section)
            else:
                product.truncated |= len(section) >= _BREACH_HOUSE_LIMIT
            for item in section:
                record = _make_record(BREACH_HOUSE, category, item)
                if not _is_about(item, record, ioc_type, target):
                    continue
                if _record_identity(record) in seen:
                    continue
                seen.add(_record_identity(record))
                if len(product.records) >= _MAX_RECORDS:
                    product.truncated = True
                    return
                product.records.append(record)

    @staticmethod
    def _severity(records: list[DarkEyeRecord]) -> ResultSeverity:
        """
        Return the severity of a set of records.

        `ResultSeverity` has no value for "listed as a victim". A listing means the
        organisation or address appears in leaked or listed data, which is not the
        same as the observable being hostile, so the result never goes above
        `warning` and is never `high`. A record means something was found, so any
        record other than a lead is `warning`. Leads are `information`, and so is
        no record at all: no record only means nothing was found, not that the
        observable is clean.
        """
        if any(record.category != "lead" for record in records):
            return ResultSeverity.warning
        return ResultSeverity.information

    def _fill_result(
        self: Self,
        result: dict[str, Any],
        products: list[_ProductResult],
    ) -> None:
        """Set status, severity, details and raw result from the product results."""
        records: list[DarkEyeRecord] = [rec for prod in products for rec in prod.records]
        errors: list[str] = [
            f"{prod.product}: {prod.error}" for prod in products if prod.error
        ]
        result["Reference"] = ", ".join(prod.reference for prod in products)

        if len(errors) == len(products) and not records:
            # Nothing was answered: an empty result would read as "not found".
            result["Status"] = LookupStatus.QUERY_FAILED.value
            result["Severity"] = ResultSeverity.unknown.name
            result["Result"] = False
            result["Details"] = "; ".join(errors)
            result["RawResult"] = {"Errors": errors}
            return

        categories: dict[str, int] = {}
        for record in records:
            categories[record.category] = categories.get(record.category, 0) + 1
        record_dicts: list[dict[str, str]] = [dict(record) for record in records]
        product_info: dict[str, dict[str, Any]] = {}
        for prod in products:
            info: dict[str, Any] = {
                "Status": "error" if prod.error else "ok",
                "Records": len(prod.records),
                "Truncated": prod.truncated,
            }
            if prod.error:
                info["Error"] = prod.error
            product_info[prod.product] = info
        not_queried: list[str] = [
            name
            for name, key in ((HIBR, self._hibr_key), (BREACH_HOUSE, self._breach_house_key))
            if not key
        ]
        for name in not_queried:
            product_info[name] = {"Status": _NOT_QUERIED}

        details: dict[str, Any] = {
            "Count": len(records),
            "Products": product_info,
            "Categories": categories,
            "Truncated": any(prod.truncated for prod in products),
            "Records": record_dicts,
        }
        if errors:
            details["Errors"] = errors
        result["Status"] = LookupStatus.OK.value
        result["Result"] = bool(records)
        result["Severity"] = self._severity(records).name
        result["Details"] = details
        # Not the raw API response: that can hold fields this provider never reads.
        result["RawResult"] = {"Records": record_dicts, "Products": product_info}

    def parse_results(self: Self, response: dict) -> tuple[bool, ResultSeverity, Any]:
        """
        Return the details of the response.

        Not used by this provider: `_run_ti_lookup_query` reduces each response
        to listing metadata itself and builds the result from both products.
        This reports the same details from a result that has already been built.

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
        raw: Any = response.get("RawResult")
        if response.get("Status") != LookupStatus.OK.value or not isinstance(raw, dict):
            return False, ResultSeverity.information, "Not found."
        records: list[DarkEyeRecord] = [
            DarkEyeRecord(**record) for record in raw.get("Records", [])
        ]
        return (
            bool(records),
            self._severity(records),
            {"Count": len(records), "Products": raw.get("Products", {})},
        )
