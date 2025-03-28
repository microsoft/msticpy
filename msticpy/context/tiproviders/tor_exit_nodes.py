# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Tor Exit Nodes Provider.

Input can be a single IoC observable or a pandas DataFrame containing
multiple observables. Processing may require a an API key and
processing performance may be limited to a specific number of
requests per minute for the account type that you have.

"""
from __future__ import annotations

import contextlib
from datetime import datetime, timezone
from threading import Lock
from typing import Any, ClassVar, Iterable

import httpx
import pandas as pd
from typing_extensions import Self

from ..._version import VERSION
from ...common.pkg_config import get_http_timeout
from ...common.utility import export, mp_ua_header
from ..lookup_result import LookupStatus
from .ti_provider_base import ResultSeverity, TIProvider

__version__ = VERSION
__author__ = "Ian Hellen"


@export
class Tor(TIProvider):
    """Tor Exit Nodes Lookup."""

    _BASE_URL: ClassVar[str] = "https://check.torproject.org/exit-addresses"
    _ALT_URL: ClassVar[str] = (
        "https://raw.githubusercontent.com/SecOps-Institute/Tor-IP-Addresses"
        "/master/tor-exit-nodes.lst"
    )

    _QUERIES: ClassVar[dict[str, Any]] = {"ipv4": None, "ipv6": None}
    _nodelist: ClassVar[dict[str, dict[str, str]]] = {}
    _last_cached: datetime = datetime.min
    _cache_lock = Lock()

    @classmethod
    def _check_and_get_nodelist(cls: type[Self]) -> None:
        """Pull down Tor exit node list and save to internal attribute."""
        if cls._cache_lock.locked():
            return
        now: datetime = datetime.now(timezone.utc)
        if not cls._nodelist or (now - cls._last_cached).days > 1:
            with contextlib.suppress(ConnectionError):
                resp: httpx.Response = httpx.get(
                    cls._BASE_URL,
                    timeout=get_http_timeout(),
                    headers=mp_ua_header(),
                )
                tor_raw_list: str = resp.content.decode()
                with cls._cache_lock:
                    cls._nodelist = dict(cls._tor_splitter(tor_raw_list))
                    cls._last_cached = datetime.now(timezone.utc)
        if not cls._nodelist:
            with contextlib.suppress(ConnectionError):
                resp = httpx.get(
                    cls._ALT_URL,
                    timeout=get_http_timeout(),
                    headers=mp_ua_header(),
                )
                tor_raw_list = resp.content.decode()
                with cls._cache_lock:
                    node_dict: dict[str, Any] = {"ExitNode": True, "LastStatus": now}
                    cls._nodelist = {
                        node: node_dict for node in tor_raw_list.split("\n")
                    }
                    cls._last_cached = datetime.now(timezone.utc)

    @staticmethod
    def _tor_splitter(node_list: str) -> Iterable[tuple[str, dict[str, Any]]]:
        node_dict: dict[str, str | None] = {}
        for line in node_list.split("\n"):
            if not line:
                continue
            fields: list[str] = line.split(" ", 2)
            if fields[0] == "ExitNode":
                # new record so reset dict
                node_dict = {}
            if len(fields) > 1:
                node_dict[fields[0]] = fields[1]
            else:
                node_dict[fields[0]] = None
            if fields[0] == "ExitAddress":
                # yield tuple
                yield fields[1], node_dict

    def lookup_ioc(
        self: Self,
        ioc: str,
        ioc_type: str | None = None,
        query_type: str | None = None,
        *,
        provider_name: str | None = None,
    ) -> pd.DataFrame:
        """
        Lookup a single IoC observable.

        Parameters
        ----------
        ioc : str
            IoC Observable value
        ioc_type : str, optional
            IoC Type, by default None (type will be inferred)
        query_type : str, optional
            Specify the data subtype to be queried, by default None.
            If not specified the default record type for the IoC type
            will be returned.
        provider_name : str, optional
            Name of the provider to query

        Returns
        -------
        pd.DataFrame
            The returned results.

        """
        if not self._nodelist:
            self._check_and_get_nodelist()
        result: dict[str, Any] = self._check_ioc_type(
            ioc=ioc,
            ioc_type=ioc_type,
            query_subtype=query_type,
        )

        result["Provider"] = provider_name or self.__class__.__name__
        result["Result"] = bool(self._nodelist)
        result["Reference"] = self._BASE_URL

        if result["Status"] and not bool(self._nodelist):
            result["Status"] = LookupStatus.QUERY_FAILED.value

        if result["Status"]:
            return pd.DataFrame([result])

        tor_node = self._nodelist.get(ioc)

        if tor_node:
            result["Severity"] = ResultSeverity.warning.name
            result["Details"] = {
                "NodeID": tor_node["ExitNode"],
                "LastStatus": tor_node["LastStatus"],
            }
            result["RawResult"] = tor_node
        else:
            result["Details"] = "Not found."
        return pd.DataFrame([result])

    def parse_results(self: Self, response: dict) -> tuple[bool, ResultSeverity, Any]:
        """
        Return the details of the response.

        Parameters
        ----------
        response : Dict
            The returned data response

        Returns
        -------
        tuple[bool, ResultSeverity, Any]
            bool = positive or negative hit
            ResultSeverity = enumeration of severity
            Object with match details

        """
        del response
        return (True, ResultSeverity.information, None)
