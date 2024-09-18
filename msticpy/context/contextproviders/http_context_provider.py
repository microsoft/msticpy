# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
HTTP Context Provider base.

HTTP implementation of a ContextProvider.
It is used to interface with HTTP API providing additional contexts.
It inherits from ContextProvider and HttpProvider

"""
from __future__ import annotations

from functools import lru_cache
from json import JSONDecodeError
from typing import TYPE_CHECKING, Any

import pandas as pd
from typing_extensions import Self

from ..._version import VERSION
from ...common.pkg_config import get_http_timeout
from ..http_provider import APILookupParams, HttpProvider
from ..lookup_result import LookupStatus
from .context_provider_base import ContextProvider

if TYPE_CHECKING:
    import httpx

__version__ = VERSION
__author__ = "Florian Bracq"


# Create alias for simplicity
ContextLookupParams = APILookupParams


class HttpContextProvider(ContextProvider, HttpProvider):
    """HTTP Context Provider base class."""

    def __init__(
        self: HttpContextProvider,
        timeout: int | None = None,
        ApiID: str | None = None,  # noqa: N803
        AuthKey: str | None = None,  # noqa: N803
        Instance: str | None = None,  # noqa: N803
    ) -> None:
        """Init HttpContextProvider."""
        ContextProvider.__init__(self)
        HttpProvider.__init__(
            self,
            timeout=timeout,
            ApiID=ApiID,
            AuthKey=AuthKey,
            Instance=Instance,
        )

    def _run_context_lookup_query(
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
            result["SafeObservable"],
            result["ObservableType"],
            query_type,
        )
        if verb == "GET":
            response: httpx.Response = self._httpx_client.get(
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
                result["RawResult"] = response.json().copy()
                result["Result"], result["Details"] = self.parse_results(result)
            except JSONDecodeError:
                result[
                    "RawResult"
                ] = f"""There was a problem parsing results from this lookup:
                                    {response.text}"""
                result["Result"] = False
                result["Details"] = {}
            result["Status"] = LookupStatus.OK.value
        else:
            result["RawResult"] = str(response.text)
            result["Result"] = False
            result["Details"] = self._response_message(result["Status"])
        return result

    @lru_cache(maxsize=256)
    def lookup_observable(  # noqa:PLR0913
        self: Self,
        observable: str,
        observable_type: str | None = None,
        query_type: str | None = None,
        *,
        provider_name: str | None = None,
        timeout: int | None = None,
    ) -> pd.DataFrame:
        """
        Lookup from a value.

        Parameters
        ----------
        observable : str
            observable to lookup
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
        result: dict[str, Any] = self._check_observable_type(
            observable,
            observable_type,
            query_subtype=query_type,
        )

        result["Provider"] = provider_name or self.__class__.__name__

        req_params: dict[str, Any] = {}
        try:
            result = self._run_context_lookup_query(
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
