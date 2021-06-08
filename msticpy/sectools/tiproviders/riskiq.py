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
from typing import Tuple, Iterable, Dict, Any

from .ti_provider_base import TIProvider, LookupResult, TISeverity, TILookupStatus
from ...common.utility import export
from ..._version import VERSION

from passivetotal import analyzer as ptanalyzer

__version__ = VERSION
__author__ = "Mark Kendrick"


@export
class RiskIQ(TIProvider):
    """RiskIQ Threat Intelligence Lookup."""

    _IOC_QUERIES: dict = {
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
    _IOC_QUERIES["dns"] = _IOC_QUERIES["hostname"]
    _IOC_QUERIES["dns-articles"] = _IOC_QUERIES["hostname-articles"]
    _IOC_QUERIES["dns-artifacts"] = _IOC_QUERIES["hostname-artifacts"]
    _IOC_QUERIES["dns-certificates"] = _IOC_QUERIES["hostname-certificates"]
    _IOC_QUERIES["dns-components"] = _IOC_QUERIES["hostname-components"]
    _IOC_QUERIES["dns-cookies"] = _IOC_QUERIES["hostname-cookies"]
    _IOC_QUERIES["dns-hostpairchildren"] = _IOC_QUERIES["hostname-hostpairchildren"]
    _IOC_QUERIES["dns-hostpairparents"] = _IOC_QUERIES["hostname-hostpairparents"]
    _IOC_QUERIES["dns-passivedns"] = _IOC_QUERIES["hostname-passivedns"]
    _IOC_QUERIES["dns-projects"] = _IOC_QUERIES["hostname-projects"]
    _IOC_QUERIES["dns-malware"] = _IOC_QUERIES["hostname-malware"]
    _IOC_QUERIES["dns-rep"] = _IOC_QUERIES["hostname-rep"]
    _IOC_QUERIES["dns-summary"] = _IOC_QUERIES["hostname-summary"]
    _IOC_QUERIES["dns-trackers"] = _IOC_QUERIES["hostname-trackers"]
    _IOC_QUERIES["dns-whois"] = _IOC_QUERIES["hostname-whois"] 

    def __init__(self, **kwargs):
        """Instantiate RiskIQ class."""
        super().__init__(**kwargs)
        ptanalyzer.init(
            api_username=kwargs.get('ApiID'), 
            api_key=kwargs.get('AuthKey')
        )
    
    def _severity_rep(self, classification):
        """Get the severity level for a reputation score classification."""
        return {
            "MALICIOUS": TISeverity.high,
            "SUSPICIOUS": TISeverity.warning,
            "UNKNOWN": TISeverity.information,
            "GOOD": TISeverity.information
        }.get(classification, TISeverity.information)

    def lookup_ioc(
        self, ioc: str, ioc_type: str = None, query_type: str = None, **kwargs
    ) -> LookupResult:
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
        result = self._check_ioc_type(
            ioc=ioc, ioc_type=ioc_type, query_subtype=query_type
        )

        if result.status:
            return result

        result.provider = kwargs.get("provider_name", self.__class__.__name__)
        
        if query_type is None:
            prop = "ALL"
        elif query_type not in [q.split("-", maxsplit=1)[-1] for q in self._IOC_QUERIES]:
            result.result = False
            result.status = TILookupStatus.query_failed.value
            result.details = 'ERROR: unsupported query type {}'.format(query_type)
            return result
        else:
            prop = self._IOC_QUERIES.get("{0}-{1}".format(result.ioc_type, query_type),"ALL")
        
        try:
            obj = ptanalyzer.get_object(ioc)
            if prop == "ALL":
                details = {
                    "summary": obj.summary.as_dict,
                    "reputation": obj.reputation.as_dict
                }
                if obj.summary.total == 0 and obj.reputation.score == 0:
                    result.result = False
                else:
                    result.result = True
                rep_severity = self._severity_rep(obj.reputation.classification)
                result.set_severity(rep_severity)
                #if 'articles' in obj.summary.available:
                #    result.set_severity(max(rep_severity, TISeverity.high))
                #elif 'projects' in obj.summary.available:
                #    result.set_severity(max(rep_severity, TISeverity.warning))
                #
                # TODO: available prop is broken, fix in core lib, then re-enable
                #
            else:
                attr = getattr(obj, prop)
                details = attr.as_dict
                result.result = True
            result.details = details
            result.raw_result = details
        except ptanalyzer.AnalyzerError as e:
            result.result = False
            result.status = TILookupStatus.query_failed.value
            result.details = "ERROR: {}".format(e.message)
            result.raw_result = e
            result.set_severeity(TISeverity.unknown)
            
        return result

    def parse_results(self, response: LookupResult) -> Tuple[bool, TISeverity, Any]:
        """
        Return the details of the response.

        Parameters
        ----------
        response : LookupResult
            The returned data response

        Returns
        -------
        Tuple[bool, TISeverity, Any]
            bool = positive or negative hit
            TISeverity = enumeration of severity
            Object with match details

        """
        # TODO why is this here? do I need it?
        return (True, TISeverity.information, None)
