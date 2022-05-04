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

import attr
import pandas as pd

from ..._version import VERSION
from ...common.utility import export
from .http_provider import HttpTIProvider, IoCLookupParams
from .ti_provider_base import LookupResult, LookupStatus, ResultSeverity, generate_items

__version__ = VERSION
__author__ = "Ian Hellen"


@export
class OPR(HttpTIProvider):
    """Open PageRank Lookup."""

    _BASE_URL = "https://openpagerank.com"

    _IOC_QUERIES = {
        "dns": IoCLookupParams(
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
        obs_col: str = None,
        ioc_type_col: str = None,
        query_type: str = None,
        **kwargs,
    ) -> pd.DataFrame:
        """Passthrough async wrapper to original method."""
        return self.lookup_iocs(data, obs_col, ioc_type_col, query_type, **kwargs)

    # pylint: disable=duplicate-code
    def lookup_iocs(
        self,
        data: Union[pd.DataFrame, Dict[str, str], Iterable[str]],
        obs_col: str = None,
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
        obs_col : str, optional
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
        for ioc, ioc_type in generate_items(data, obs_col, ioc_type_col):
            if not ioc:
                continue
            result = self._check_ioc_type(
                ioc=ioc, ioc_type=ioc_type, query_subtype=query_type
            )

            if result.status == LookupStatus.ok.value:
                domain_list.add(result.ioc)
            else:
                bad_requests.append(pd.Series(attr.asdict(result)))

        results: List[pd.Series] = []
        if not domain_list:
            return pd.DataFrame(columns=LookupResult.column_map())
        results.extend(
            pd.Series(attr.asdict(item_result))
            for item_result in self._lookup_bulk_request(domain_list)
        )

        all_results = results + bad_requests
        return pd.DataFrame(data=all_results).rename(columns=LookupResult.column_map())

    def parse_results(self, response: LookupResult) -> Tuple[bool, ResultSeverity, Any]:
        """
        Return the details of the response.

        Parameters
        ----------
        response : LookupResult
            The returned data response

        Returns
        -------
        Tuple[bool, ResultSeverity, Any]
            bool = positive or negative hit
            ResultSeverity = enumeration of severity
            Object with match details

        """
        if self._failed_response(response) or not isinstance(response.raw_result, dict):
            return False, ResultSeverity.information, "Not found."

        severity = ResultSeverity.information
        if "response" in response.raw_result:
            dom_records = response.raw_result["response"]
            dom_record = dom_records[0]
            return self._parse_one_record(dom_record)
        return True, severity, {}

    def _parse_multi_results(self, response: LookupResult) -> Iterable[LookupResult]:
        """Parse details of batch response."""
        if not isinstance(response.raw_result, dict):
            yield LookupResult(
                **attr.asdict(response),
                result=False,
                severity=ResultSeverity.information.value,
                details="Not found",
            )

        elif "response" in response.raw_result:
            dom_records = response.raw_result["response"]
            for dom_record in dom_records:
                result, sev, details = self._parse_one_record(dom_record)
                domain_name = dom_record["domain"]
                yield LookupResult(
                    ioc=domain_name,
                    ioc_type="dns",
                    provider=self._provider_name,
                    result=result,
                    severity=sev.value,
                    details=details,
                    raw_result=dom_record,
                    reference=f"{response.reference}?domains[0]={domain_name}",
                )

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

    def _lookup_bulk_request(self, ioc_list: Iterable[str]) -> Iterable[LookupResult]:
        ioc_list = list(ioc_list)
        batch_size = 100

        l_len = len(ioc_list)
        for step in range(0, l_len, batch_size):
            batch_list = ioc_list[step : (step + batch_size)]  # noqa: E203
            yield from self._lookup_batch(batch_list)

    # pylint: disable=duplicate-code
    def _lookup_batch(self, ioc_list: list) -> Iterable[LookupResult]:
        # build the query string manually - of the form domains[N]=domN&domains[N+1]...
        qry_elements = [
            f"domains[{idx}]={dom}" for idx, dom in zip(range(len(ioc_list)), ioc_list)
        ]

        qry_str = "&".join(qry_elements)
        path = self._IOC_QUERIES["dns"].path
        req_url = f"{self._BASE_URL}{path}?{qry_str}"

        try:
            _, req_params = self._substitute_parms("dummy", "dns", None)
            response = self._httpx_client.get(
                url=req_url, headers=req_params["headers"]
            )
            if response.status_code == 200:
                result = LookupResult(
                    ioc=",".join(ioc_list),
                    ioc_type="dns",
                    status=LookupStatus.ok.value,
                    reference=f"{self._BASE_URL}{path}",
                    raw_result=response.json(),
                )

                yield from self._parse_multi_results(result)
            else:
                yield LookupResult(
                    ioc=",".join(ioc_list),
                    ioc_type="dns",
                    status=response.status_code,
                    reference=req_url,
                    raw_result=str(response),
                    result=False,
                    details="No response from provider.",
                )
        except (
            LookupError,
            JSONDecodeError,
            NotImplementedError,
            ConnectionError,
        ) as err:
            result_dict = {
                "ioc": ",".join(ioc_list),
                "ioc_type": "dns",
                "details": "\n".join(err.args),
            }
            if isinstance(err, LookupError):
                result_dict["reference"] = req_url
            yield LookupResult(**result_dict)  # type: ignore
