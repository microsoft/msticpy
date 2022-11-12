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
from functools import lru_cache
from json import JSONDecodeError
from typing import Any, Dict

import pandas as pd

from ..._version import VERSION
from ...common.pkg_config import get_http_timeout
from ..http_provider import APILookupParams, HttpProvider
from ..lookup_result import LookupStatus
from .context_provider_base import ContextProvider

__version__ = VERSION
__author__ = "Florian Bracq"


# Create alias for simplicity
ContextLookupParams = APILookupParams


class HttpContextProvider(ContextProvider, HttpProvider):
    """HTTP Context Provider base class."""

    @lru_cache(maxsize=256)
    def lookup_observable(
        self,
        observable: str,
        observable_type: str = None,
        query_type: str = None,
        **kwargs,
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
        result = self._check_observable_type(
            observable, observable_type, query_subtype=query_type
        )

        result["Provider"] = kwargs.get("provider_name", self.__class__.__name__)

        req_params: Dict[str, Any] = {}
        try:
            verb, req_params = self._substitute_parms(
                result["SafeObservable"], result["ObservableType"], query_type
            )
            if verb == "GET":
                response = self._httpx_client.get(
                    **req_params, timeout=get_http_timeout(**kwargs)
                )
            else:
                raise NotImplementedError(f"Unsupported verb {verb}")

            result["Status"] = response.status_code
            result["Reference"] = req_params["url"]
            if result["Status"] == 200:
                try:
                    result["RawResult"] = response.json()
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
        except (
            LookupError,
            JSONDecodeError,
            NotImplementedError,
            ConnectionError,
        ) as err:
            self._err_to_results(result, err)
            if not isinstance(err, LookupError):
                url = req_params.get("url", None) if req_params else None
                result["Reference"] = url
        return pd.DataFrame([result])
