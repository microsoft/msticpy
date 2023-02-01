# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Open Page Rank Provider.

Input can be a single IoC observable or a pandas DataFrame containing
multiple observables. Processing may require a an API key and
processing performance may be limited to a specific number of
requests per minute for the account type that you have.

"""
from json import JSONDecodeError
from typing import Any, Dict, Iterable, List, Tuple, Union

import pandas as pd

from ..._version import VERSION
from ...common.utility import export
from ..http_provider import APILookupParams
from ..lookup_result import LookupStatus
from ..provider_base import generate_items
from .ti_http_provider import HttpTIProvider
from .ti_provider_base import ResultSeverity

__version__ = VERSION
__author__ = "Ian Hellen"


@export
class OPR(HttpTIProvider):
    """Open PageRank Lookup."""

    _BASE_URL = "https://openpagerank.com"

    _QUERIES = {
        "dns": APILookupParams(
            path="/api/v1.0/getPageRank",
            params={"domains[0]": "{observable}"},
            headers={"API-OPR": "{API_KEY}"},
        )
    }

    _REQUIRED_PARAMS = ["API_KEY"]

    def __init__(self, **kwargs):
        """Initialize a new instance of the class."""
        super().__init__(**kwargs)

        self._provider_name = self.__class__.__name__
        print(
            "Using Open PageRank.",
            "See https://www.domcop.com/openpagerank/what-is-openpagerank",
        )

    async def lookup_iocs_async(
        self,
        data: Union[pd.DataFrame, Dict[str, str], Iterable[str]],
        ioc_col: str = None,
        ioc_type_col: str = None,
        query_type: str = None,
        **kwargs,
    ) -> pd.DataFrame:
        """Call base async wrapper."""
        return await self._lookup_iocs_async_wrapper(
            data, ioc_col, ioc_type_col, query_type, **kwargs
        )

    def lookup_iocs(
        self,
        data: Union[pd.DataFrame, Dict[str, str], Iterable[str]],
        ioc_col: str = None,
        ioc_type_col: str = None,
        query_type: str = None,
        **kwargs,
    ) -> pd.DataFrame:
        """
        Lookup collection of IoC observables.

        Parameters
        ----------
        data : Union[pd.DataFrame, Dict[str, str], Iterable[str]]
            Data input in one of three formats:
            1. Pandas dataframe (you must supply the column name in
            `obs_col` parameter)
            2. Dict of observable, IoCType
            3. Iterable of observables - IoCTypes will be inferred
        ioc_col : str, optional
            DataFrame column to use for observables, by default None
        ioc_type_col : str, optional
            DataFrame column to use for IoCTypes, by default None
        query_type : str, optional
            Specify the data subtype to be queried, by default None.
            If not specified the default record type for the IoC type
            will be returned.

        Returns
        -------
        pd.DataFrame
            DataFrame of results.

        """
        kwargs.get("provider_name", self.__class__.__name__)

        domain_list = set()
        bad_requests: List[pd.Series] = []
        for ioc, ioc_type in generate_items(data, ioc_col, ioc_type_col):
            if not ioc:
                continue
            result = self._check_ioc_type(
                ioc=ioc, ioc_type=ioc_type, query_subtype=query_type
            )

            if result["Status"] == LookupStatus.OK.value:
                domain_list.add(result["Ioc"])
            else:
                bad_requests.append(pd.Series(result))

        results: List[pd.Series] = []
        if not domain_list:
            return pd.DataFrame()
        results.extend(self._lookup_bulk_request(domain_list))

        all_results = results + bad_requests
        return pd.DataFrame(all_results)

    def parse_results(self, response: Dict) -> Tuple[bool, ResultSeverity, Any]:
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
            response["RawResult"], dict
        ):
            return False, ResultSeverity.information, "Not found."

        severity = ResultSeverity.information
        if "response" in response["RawResult"]:
            dom_records = response["RawResult"]["response"]
            dom_record = dom_records[0]
            return self._parse_one_record(dom_record)
        return True, severity, {}

    def _parse_multi_results(self, response: Dict) -> Iterable[Dict]:
        """Parse details of batch response."""
        if not isinstance(response["RawResult"], dict):
            response.update(
                {
                    "Result": False,
                    "Severity": ResultSeverity.information.value,
                    "Details": "Not found",
                }
            )
            yield response

        elif "response" in response["RawResult"]:
            dom_records = response["RawResult"]["response"]
            for dom_record in dom_records:
                result, sev, details = self._parse_one_record(dom_record)
                domain_name = dom_record["domain"]
                yield {
                    "Ioc": domain_name,
                    "IocType": "dns",
                    "Provider": self._provider_name,
                    "Result": result,
                    "Severity": sev.name,
                    "Details": details,
                    "RawResult": dom_record,
                    "Reference": f"{response['Reference']}?domains[0]={domain_name}",
                }

    @staticmethod
    def _parse_one_record(dom_record: dict):
        record_status = dom_record.get("status_code", 404)
        severity = ResultSeverity.information
        if record_status == 200:
            return (
                True,
                severity,
                {
                    "rank": dom_record.get("rank", "0"),
                    "page_rank": dom_record.get("page_rank_decimal", 0),
                    "error": dom_record.get("error", ""),
                },
            )
        if record_status == 404:
            return (
                True,
                ResultSeverity.warning,
                {
                    "rank": dom_record.get("rank", "0"),
                    "error": dom_record.get("error", ""),
                },
            )
        return False, ResultSeverity.information, {}

    def _lookup_bulk_request(self, ioc_list: Iterable[str]) -> Iterable[Dict]:
        ioc_list = list(ioc_list)
        batch_size = 100

        l_len = len(ioc_list)
        for step in range(0, l_len, batch_size):
            batch_list = ioc_list[step : (step + batch_size)]  # noqa: E203
            yield from self._lookup_batch(batch_list)

    def _lookup_batch(self, ioc_list: list) -> Iterable[Dict]:
        # build the query string manually - of the form domains[N]=domN&domains[N+1]...
        qry_elements = [
            f"domains[{idx}]={dom}" for idx, dom in zip(range(len(ioc_list)), ioc_list)
        ]

        qry_str = "&".join(qry_elements)
        path = self._QUERIES["dns"].path
        req_url = f"{self._BASE_URL}{path}?{qry_str}"

        try:
            _, req_params = self._substitute_parms("dummy", "dns", None)
            response = self._httpx_client.get(
                url=req_url, headers=req_params["headers"]
            )
            if response.status_code == 200:
                result = {
                    "Ioc": ",".join(ioc_list),
                    "IocType": "dns",
                    "Status": LookupStatus.OK.value,
                    "Reference": f"{self._BASE_URL}{path}",
                    "RawResult": response.json(),
                }

                yield from self._parse_multi_results(result)
            else:
                yield {
                    "Ioc": ",".join(ioc_list),
                    "IocType": "dns",
                    "Status": response.status_code,
                    "Reference": req_url,
                    "RawResult": str(response),
                    "Result": False,
                    "Details": "No response from provider.",
                }
        except (
            LookupError,
            JSONDecodeError,
            NotImplementedError,
            ConnectionError,
        ) as err:
            result_dict = {
                "Ioc": ",".join(ioc_list),
                "IocType": "dns",
                "Details": "\n".join(err.args),
            }
            if isinstance(err, LookupError):
                result_dict["Reference"] = req_url
            yield result_dict  # type: ignore
