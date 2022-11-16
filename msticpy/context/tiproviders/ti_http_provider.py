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
from functools import lru_cache
from json import JSONDecodeError
from typing import Any, Dict

import pandas as pd

from ..._version import VERSION
from ...common.pkg_config import get_http_timeout
from ...common.utility import export
from ..http_provider import HttpProvider
from ..lookup_result import LookupStatus
from .result_severity import ResultSeverity
from .ti_provider_base import TIProvider

__version__ = VERSION
__author__ = "Ian Hellen"


@export
class HttpTIProvider(TIProvider, HttpProvider):
    """HTTP API Lookup provider base class."""

    @lru_cache(maxsize=256)
    def lookup_ioc(
        self, ioc: str, ioc_type: str = None, query_type: str = None, **kwargs
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
        result = self._check_ioc_type(ioc, ioc_type, query_subtype=query_type)

        result["Provider"] = kwargs.get("provider_name", self.__class__.__name__)

        req_params: Dict[str, Any] = {}
        try:
            verb, req_params = self._substitute_parms(
                result["SafeIoc"], result["IocType"], query_type
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
                    result["Result"], severity, result["Details"] = self.parse_results(
                        result
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
