# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
HTTP TI Provider base.

Input can be a single IoC observable or a pandas DataFrame containing
multiple observables. Processing may require a an API key and
processing performance may be limited to a specific number of
requests per minute for the account type that you have.

"""
from __future__ import annotations

from functools import lru_cache
from json import JSONDecodeError
from typing import TYPE_CHECKING, Any

import pandas as pd
from typing_extensions import Self

from ..._version import VERSION
from ...common.pkg_config import get_http_timeout
from ...common.utility import export
from ..http_provider import HttpProvider
from ..lookup_result import LookupStatus
from .result_severity import ResultSeverity
from .ti_provider_base import TIProvider

if TYPE_CHECKING:
    import httpx
__version__ = VERSION
__author__ = "Ian Hellen"


@export
class HttpTIProvider(TIProvider, HttpProvider):
    """HTTP API Lookup provider base class."""

    def __init__(
        self: HttpTIProvider,
        *,
        timeout: int | None = None,
        ApiID: str | None = None,  # noqa: N803
        AuthKey: str | None = None,  # noqa: N803
        Instance: str | None = None,  # noqa: N803
    ) -> None:
        """Init HttpContextProvider."""
        TIProvider.__init__(self)
        HttpProvider.__init__(
            self,
            timeout=timeout,
            ApiID=ApiID,
            AuthKey=AuthKey,
            Instance=Instance,
        )

    def _run_ti_lookup_query(
        self: Self,
        result: dict[str, Any],
        *,
        query_type: str | None = None,
        timeout: int | None = None,
    ) -> dict[str, Any]:
        """
        Lookup from a value.

        Parameters
        ----------
        result : dict
            Dict result with resolved type and pre-processed item.
            Status is none-zero on failure.
        observable_type : str, optional
            The Type of the observable to lookup, by default None (type will be inferred)
        query_type : str, optional
            Specify the data subtype to be queried, by default None.
            If not specified the default record type for the item_value
            will be returned.
        provider_name : str, optional
            Name of the provider to use for the lookup
        timeout : int, optional
            Timeout to use for lookups

        Returns
        -------
        pd.DataFrame
            The lookup result:
            result - Positive/Negative,
            details - Lookup Details (or status if failure),
            raw_result - Raw Response
            reference - URL of the item

        Raises
        ------
        NotImplementedError
            If attempting to use an HTTP method or authentication
            protocol that is not supported.

        Notes
        -----
        Note: this method uses memoization (lru_cache) to cache results
        for a particular item to try avoid repeated network calls for
        the same item.

        """
        verb, req_params = self._substitute_parms(
            result["SafeIoc"],
            result["IocType"],
            query_type,
        )
        if verb == "GET":
            response: httpx.Response = self._httpx_client.get(
                **req_params,
                timeout=get_http_timeout(timeout=timeout),
            )
        elif verb == "POST":
            response = self._httpx_client.post(
                **req_params,
                timeout=get_http_timeout(timeout=timeout),
            )
        else:
            err_msg: str = f"Unsupported verb {verb}"
            raise NotImplementedError(err_msg)

        result["Status"] = response.status_code
        result["Reference"] = req_params["url"]
        if response.is_success:
            try:
                result["RawResult"] = response.json()
                result["Result"], severity, result["Details"] = self.parse_results(
                    result,
                )
            except JSONDecodeError:
                result[
                    "RawResult"
                ] = f"""There was a problem parsing results from this lookup:
                                    {response.text}"""
                result["Result"] = False
                severity = ResultSeverity.information
                result["Details"] = {}
            if isinstance(severity, ResultSeverity):
                result["Severity"] = severity.name
            result["Severity"] = ResultSeverity.parse(severity).name
            result["Status"] = LookupStatus.OK.value
        else:
            result["RawResult"] = str(response)
            result["Result"] = False
            result["Details"] = self._response_message(result["Status"])
        return result

    @lru_cache(maxsize=256)
    def lookup_ioc(  # noqa: PLR0913
        self: Self,
        ioc: str,
        ioc_type: str | None = None,
        query_type: str | None = None,
        *,
        provider_name: str | None = None,
        timeout: int = 120,
    ) -> pd.DataFrame:
        """
        Lookup from a value.

        Parameters
        ----------
        ioc : str
            ioc to lookup
        ioc_type : str, optional
            The Type of the ioc to lookup, by default None (type will be inferred)
        query_type : str, optional
            Specify the data subtype to be queried, by default None.
            If not specified the default record type for the ioc
            will be returned.
        provider_name : str, optional
            Name of the provider to query for the lookup
        timeout : str, optional
            Timeout for lookup queries

        Returns
        -------
        pd.DataFrame
            The lookup result:
            result - Positive/Negative,
            details - Lookup Details (or status if failure),
            raw_result - Raw Response
            reference - URL of the item

        Raises
        ------
        NotImplementedError
            If attempting to use an HTTP method or authentication
            protocol that is not supported.

        Notes
        -----
        Note: this method uses memoization (lru_cache) to cache results
        for a particular item to try avoid repeated network calls for
        the same item.

        """
        result: dict[str, Any] = self._check_ioc_type(
            ioc,
            ioc_type,
            query_subtype=query_type,
        )
        result["Provider"] = provider_name or self.__class__.__name__

        req_params: dict[str, Any] = {}
        try:
            self._run_ti_lookup_query(
                result=result,
                query_type=query_type,
                timeout=timeout,
            )
        except (
            LookupError,
            JSONDecodeError,
            NotImplementedError,
            ConnectionError,
        ) as err:
            self._err_to_results(result, err)
            if not isinstance(err, LookupError):
                url: str | None = req_params.get("url") if req_params else None
                result["Reference"] = url
        return pd.DataFrame([result])
