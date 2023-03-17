# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
RiskIQ Threat Intelligence Provider.

Input can be a single IoC observable or a pandas DataFrame containing
multiple observables. Processing may require a an API key and
processing performance may be limited to a specific number of
requests per minute for the account type that you have.

"""
from datetime import datetime
from functools import partial
from typing import Any, Dict, Optional, Tuple, Union

import pandas as pd

from ..._version import VERSION
from ...common.exceptions import MsticpyImportExtraError, MsticpyUserError
from ...common.utility import export
from ..lookup_result import LookupStatus
from .ti_provider_base import ResultSeverity, TIPivotProvider, TIProvider

try:
    from passivetotal import analyzer as ptanalyzer  # isort: skip
except ImportError as imp_err:
    raise MsticpyImportExtraError(
        "Cannot use this feature without passivetotal",
        "package installed.",
        title="Error importing RiskIQ modules.",
        extra="riskiq",
    ) from imp_err

__version__ = VERSION
__author__ = "Mark Kendrick"


@export
class RiskIQ(TIProvider, TIPivotProvider):
    """RiskIQ Threat Intelligence Lookup."""

    _QUERIES: Dict[str, str] = {
        "ipv4": "ALL",
        "ipv4-articles": "articles",
        "ipv4-artifacts": "artifacts",
        "ipv4-certificates": "certificates",
        "ipv4-components": "components",
        "ipv4-cookies": "cookies",
        "ipv4-hostpairchildren": "hostpair_children",
        "ipv4-hostpairparents": "hostpair_parents",
        "ipv4-passivedns": "resolutions",
        "ipv4-projects": "projects",
        "ipv4-malware": "malware",
        "ipv4-rep": "reputation",
        "ipv4-services": "services",
        "ipv4-summary": "summary",
        "ipv4-trackers": "trackers",
        "ipv4-whois": "whois",
        "hostname": "ALL",
        "hostname-articles": "articles",
        "hostname-artifacts": "artifacts",
        "hostname-certificates": "certificates",
        "hostname-components": "components",
        "hostname-cookies": "cookies",
        "hostname-hostpairchildren": "hostpair_children",
        "hostname-hostpairparents": "hostpair_parents",
        "hostname-passivedns": "resolutions",
        "hostname-projects": "projects",
        "hostname-malware": "malware",
        "hostname-rep": "reputation",
        "hostname-summary": "summary",
        "hostname-trackers": "trackers",
        "hostname-whois": "whois",
    }

    # Aliases
    _QUERIES["dns"] = _QUERIES["hostname"]
    _QUERIES["dns-articles"] = _QUERIES["hostname-articles"]
    _QUERIES["dns-artifacts"] = _QUERIES["hostname-artifacts"]
    _QUERIES["dns-certificates"] = _QUERIES["hostname-certificates"]
    _QUERIES["dns-components"] = _QUERIES["hostname-components"]
    _QUERIES["dns-cookies"] = _QUERIES["hostname-cookies"]
    _QUERIES["dns-hostpairchildren"] = _QUERIES["hostname-hostpairchildren"]
    _QUERIES["dns-hostpairparents"] = _QUERIES["hostname-hostpairparents"]
    _QUERIES["dns-passivedns"] = _QUERIES["hostname-passivedns"]
    _QUERIES["dns-projects"] = _QUERIES["hostname-projects"]
    _QUERIES["dns-malware"] = _QUERIES["hostname-malware"]
    _QUERIES["dns-rep"] = _QUERIES["hostname-rep"]
    _QUERIES["dns-summary"] = _QUERIES["hostname-summary"]
    _QUERIES["dns-trackers"] = _QUERIES["hostname-trackers"]
    _QUERIES["dns-whois"] = _QUERIES["hostname-whois"]

    _PIVOT_ENTITIES = {
        prop: {"Dns": "DomainName", "IpAddress": "Address", "Host": "fqdn"}
        for prop in [
            "articles",
            "artifacts",
            "certificates",
            "components",
            "cookies",
            "hostpair_children",
            "hostpair_parents",
            "resolutions",
            "projects",
            "malware",
            "reputation",
            "summary",
            "trackers",
            "whois",
        ]
    }
    _PIVOT_ENTITIES["services"] = {"IpAddress": "Address"}

    _REFERENCE = "https://community.riskiq.com"

    def __init__(self, **kwargs):
        """Instantiate RiskIQ class."""
        super().__init__(**kwargs)
        ptanalyzer.init(username=kwargs.get("ApiID"), api_key=kwargs.get("AuthKey"))
        self._pivot_timespan_start: Optional[datetime] = None
        self._pivot_timespan_end: Optional[datetime] = None
        self._pivot_get_timespan: Any = None

    @property
    def _httpx_client(self):
        """Return the PT Analyzer session."""
        return ptanalyzer.api_clients["Cards"].session

    @_httpx_client.setter
    def _httpx_client(self, session):
        """Set the PT Analyzer session."""
        # pylint: disable=consider-using-dict-items
        for name in ptanalyzer.api_clients:
            ptanalyzer.api_clients[name].session = session

    @staticmethod
    def _severity_rep(classification):
        """Get the severity level for a reputation score classification."""
        return {
            "MALICIOUS": ResultSeverity.high,
            "SUSPICIOUS": ResultSeverity.warning,
            "UNKNOWN": ResultSeverity.information,
            "GOOD": ResultSeverity.information,
        }.get(classification, ResultSeverity.information)

    def lookup_ioc(
        self, ioc: str, ioc_type: str = None, query_type: str = None, **kwargs
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

        Returns
        -------
        pd.DataFrame
            The returned results.

        """
        result = self._check_ioc_type(
            ioc=ioc, ioc_type=ioc_type, query_subtype=query_type
        )

        if result["Status"]:
            return pd.DataFrame([result])

        result["Provider"] = kwargs.get("provider_name", self.__class__.__name__)
        result["Reference"] = self._REFERENCE

        if query_type is None:
            prop = "ALL"
        elif query_type not in [q.split("-", maxsplit=1)[-1] for q in self._QUERIES]:
            result["Result"] = False
            result["Status"] = LookupStatus.QUERY_FAILED.value
            result["Details"] = f"ERROR: unsupported query type {query_type}"
            return pd.DataFrame([result])
        else:
            prop = self._QUERIES.get(f"{result['IocType']}-{query_type}", "ALL")

        try:
            ptanalyzer.set_context("msticpy", "ti", VERSION, prop)
            pt_obj = ptanalyzer.get_object(ioc)
            if prop == "ALL":
                result = self._parse_result_all_props(pt_obj, result)
            else:
                result = self._parse_result_prop(pt_obj, prop, result)
        except ptanalyzer.AnalyzerError as err:
            result["Result"] = False
            result["Status"] = LookupStatus.QUERY_FAILED.value
            result["Details"] = f"ERROR: {err}"
            result["Raw_result"] = err
            result["Severity"] = ResultSeverity.unknown.name

        return pd.DataFrame([result])

    def _parse_result_all_props(self, pt_result, ti_result):
        """Parse results for ALL properties."""
        ti_result["Details"] = {
            "summary": pt_result.summary.as_dict,
            "reputation": pt_result.reputation.as_dict,
        }
        ti_result["RawResult"] = ti_result["Details"]
        ti_result["Result"] = (
            pt_result.summary.total != 0 or pt_result.reputation.score != 0
        )

        rep_severity = self._severity_rep(pt_result.reputation.classification)
        ti_result["Severity"] = rep_severity.name
        if (
            "malware_hashes" in pt_result.summary.available
            or "articles" in pt_result.summary.available
        ):
            ti_result["Severity"] = (max(rep_severity, ResultSeverity.high)).name
        elif "projects" in pt_result.summary.available:
            ti_result["Severity"] = (max(rep_severity, ResultSeverity.warning)).name
        return ti_result

    def _parse_result_prop(self, pt_result, pt_prop, ti_result):
        """Parse result for a specific property."""
        attr = getattr(pt_result, pt_prop)
        if pt_prop == "reputation":
            ti_result["Severity"] = self._severity_rep(attr.classification).name
        elif pt_prop == "malware_hashes" and len(attr) > 0:
            ti_result["Severity"] = ResultSeverity.high.name
        else:
            ti_result["Severity"] = ResultSeverity.information.name
        ti_result["Details"] = ti_result["RawResult"] = attr.as_dict
        ti_result["Result"] = True
        return ti_result

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
        return (True, ResultSeverity.information, None)

    def _set_pivot_timespan(self, **kwargs):
        """
        Set the pivot timespan and track whether it has changed.

        Returns
        -------
        bool
            whether the timespan changed.

        """
        changed = False
        start = kwargs.pop(
            "start",
            self._pivot_get_timespan().start if self._pivot_get_timespan else None,
        )
        end = kwargs.pop(
            "end", self._pivot_get_timespan().end if self._pivot_get_timespan else None
        )
        if (
            start
            and end
            and (start != self._pivot_timespan_start or end != self._pivot_timespan_end)
        ):
            changed = True
            self._pivot_timespan_start = start
            self._pivot_timespan_end = end
            ptanalyzer.set_date_range(start_date=start, end_date=end)
        return changed

    def pivot_value(self, prop, host, **kwargs):
        """Perform a pivot on a single value."""
        ts_changed = self._set_pivot_timespan(**kwargs)
        ptanalyzer.set_context("msticpy", "pivot", VERSION, prop)
        obj = ptanalyzer.get_object(host)
        if ts_changed and prop not in ["reputation", "summary", "whois"]:
            obj.reset(prop)
        try:
            attrib = getattr(obj, prop)
        except ptanalyzer.AnalyzerAPIError as err:
            raise RiskIQAPIUserError(err.message) from err
        except ptanalyzer.AnalyzerError as err:
            raise RiskIQUserError("Analyzer error.") from err
        return attrib.to_dataframe(**kwargs)

    def register_pivots(
        self,
        pivot_reg: "PivotRegistration",  # type: ignore # noqa: F821
        pivot: "Pivot",  # type: ignore # noqa: F821
    ):
        """
        Register pivot functions for the TI Provider.

        Parameters
        ----------
        pivot_reg : PivotRegistration
            Pivot registration settings.
        pivot : Pivot
            Pivot library instance

        """
        self._pivot_get_timespan = pivot.get_timespan
        self._pivot_timespan_start = None
        self._pivot_timespan_end = None
        base_reg = {
            "entity_container_name": "RiskIQ",
            "func_df_param_name": "data",
            "func_df_col_param_name": "host",
            "func_input_value_arg": "host",
            "func_out_column_name": "query",
        }
        for prop, entity_map in self._PIVOT_ENTITIES.items():
            reg = pivot_reg(
                func_new_name=prop,
                func_static_params={"prop": prop},
                input_type="value",
                entity_map=entity_map,
                **base_reg,
            )
            fun = partial(self.pivot_value)
            fun.__doc__ = getattr(
                ptanalyzer.Hostname, prop, getattr(ptanalyzer.IPAddress, prop)
            ).__doc__
            pivot.add_pivot_function(fun, pivot_reg=reg, container="RiskIQ")


class RiskIQUserError(MsticpyUserError):
    """Generic RiskIQ provider exception."""

    def __init__(
        self, *args, help_uri: Union[Tuple[str, str], str, None] = None, **kwargs
    ):
        """
        Create RiskIQ provider exception.

        Parameters
        ----------
        help_uri : Union[Tuple[str, str], str, None], optional
            Override the default help URI.

        """
        kwargs.update(title="error using RiskIQ python library")
        kwargs.update(
            ptlib_uri=(
                "RiskIQ PassiveTotal Python Library",
                "https://passivetotal.readthedocs.io",
            )
        )
        kwargs.update(
            riqinfo_uri=("RiskIQ Support", "https://www.riskiq.com/resources/support/")
        )
        uri = help_uri or self.DEF_HELP_URI
        super().__init__(*args, help_uri=uri, **kwargs)


class RiskIQAPIUserError(RiskIQUserError):
    """RiskIQ API provider exception."""

    def __init__(self, api_exception: ptanalyzer.AnalyzerAPIError):
        """
        Create RiskIQ API exception.

        Parameters
        ----------
        api_exception : ptanalyzer.AnalyzerAPIError
            Underlying API exception.

        """
        title = f"{api_exception.status_code} {api_exception.message}"
        super().__init__(title, str(api_exception))
