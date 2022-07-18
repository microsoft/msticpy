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
import attr
from .ti_provider_base import TIProvider
from ..http_lookup import HttpLookupProvider, APILookupParams
from .ti_lookup_result import TILookupResult, TILookupStatus
from .result_severity import ResultSeverity
from ...common.utility import export
from ..._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


IoCLookupParams = APILookupParams


@export
class HttpTIProvider(HttpLookupProvider, TIProvider):
    """HTTP API Lookup provider base class."""

    def lookup_ioc(
        self,
        ioc: str,
        ioc_type: str = None,
        query_type: str = None,
        **kwargs,
    ) -> TILookupResult:
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

        Returns
        -------
        LookupResult
            The returned results.

        """
        result = self.lookup_item(
            ioc, item_type=ioc_type, query_type=query_type, **kwargs
        )

        item_dict = attr.asdict(result)
        ioc_dict = {
            key.replace("item", "ioc"): value for key, value in item_dict.items()
        }
        ioc_result = TILookupResult(**ioc_dict)

        if ioc_result.status == TILookupStatus.OK.value:
            ioc_result.result, severity, ioc_result.details = self.parse_results(
                ioc_result
            )
        else:
            severity = ResultSeverity.information
        ioc_result.set_severity(severity)

        return ioc_result
