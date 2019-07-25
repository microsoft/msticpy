# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Module for TILookup classes.

Input can be a single IoC observable or a pandas DataFrame containing
multiple observables. Processing may require a an API key and
processing performance may be limited to a specific number of
requests per minute for the account type that you have.

"""
import abc
from abc import ABC
import math
import re
from collections import Counter, namedtuple

from functools import singledispatch
from ipaddress import IPv4Address, IPv6Address, ip_address
from typing import Any, Dict, Iterable, List, Optional, Set, Tuple, Union
import socket
from socket import gaierror

import attr
import pandas as pd
from urllib3.exceptions import LocationParseError
from urllib3.util import parse_url

from ..._version import VERSION
from ...nbtools.utility import export
from ..iocextract import IoCExtract, IoCType

__version__ = VERSION
__author__ = "Ian Hellen"


SanitizedObservable = namedtuple("SanitizedObservable", ["observable", "status"])


# pylint: disable=too-few-public-methods
@attr.s(auto_attribs=True)
class LookupResult:
    """Lookup result for IoCs."""

    ioc: str
    ioc_type: str
    query_subtype: str = None
    result: bool = False
    details: Any = None
    raw_result: Optional[str] = None
    reference: Optional[str] = None
    status: int = 0


# Mapping for DataFrame columns
_DF_COLUMNS_MAP: Dict[str, str] = {
    "ioc": "Ioc",
    "ioc_type": "IocType",
    "query_subtype": "QuerySubtype",
    "result": "Result",
    "details": "Details",
    "raw_result": "RawResult",
    "reference": "Reference",
    "status": "Status",
}


@export
class TIProvider(ABC):
    """Abstract base class for Threat Intel providers."""

    _ioc_extract = IoCExtract()

    # pylint: disable=unused-argument
    def __init__(self, **kwargs):
        """Initialize the provider."""
        self._supported_types: Set[IoCType] = set()
        self.description: Optional[str] = None

    @abc.abstractmethod
    def lookup_ioc(
        self, ioc: str, ioc_type: str = None, query_type: str = None
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

    def lookup_iocs(
        self,
        data: Union[pd.DataFrame, Dict[str, str], Iterable[str]],
        obs_col: str = None,
        ioc_type_col: str = None,
        query_type: str = None,
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
        results = []
        for observable, ioc_type in generate_items(data, obs_col, ioc_type_col):
            item_result = self.lookup_ioc(
                ioc=observable, ioc_type=ioc_type, query_type=query_type
            )
            results.append(pd.Series(attr.asdict(item_result)))

        return (
            pd.DataFrame(data=results)
            .rename(columns=_DF_COLUMNS_MAP)
            .drop(columns=["status"])
        )

    @abc.abstractmethod
    def parse_results(self, response: LookupResult) -> Tuple[bool, Any]:
        """
        Return the details of the response.

        Parameters
        ----------
        response : LookupResult
            The returned data response

        Returns
        -------
        Tuple[bool, Any]
            bool = positive or negative hit
            Object with match details

        """

    @property
    def supported_types(self) -> List[str]:
        """
        Return list of supported IoC types for this provider.

        Returns
        -------
        List[str]
            List of supported type names

        """
        return [ioc.name for ioc in self._supported_types]

    @classmethod
    def resolve_ioc_type(cls, observable: str) -> str:
        """
        Return IoCType determined by IoCExtract.

        Parameters
        ----------
        observable : str
            IoC observable string

        Returns
        -------
        str
            IoC Type (or unknown if type could not be determined)

        """
        return cls._ioc_extract.get_ioc_type(observable)

    @classmethod
    def is_known_type(cls, ioc_type: str) -> bool:
        """
        Return True if this a known IoC Type.

        Parameters
        ----------
        ioc_type : str
            IoCType string to test

        Returns
        -------
        bool
            True if known type.

        """
        return ioc_type in IoCType.__members__ and ioc_type != "unknown"

    @classmethod
    @abc.abstractmethod
    def usage(cls):
        """Print usage of provider."""

    def is_supported_type(self, ioc_type: Union[str, IoCType]) -> bool:
        """
        Return True if the passed type is supported.

        Parameters
        ----------
        ioc_type : Union[str, IoCType]
            IoC type name or instance

        Returns
        -------
        bool
            True if supported.

        """
        if isinstance(ioc_type, str):
            ioc_type = IoCType.parse(ioc_type)
        return ioc_type.name in self.supported_types


_IOC_EXTRACT = IoCExtract()
# slightly stricter than normal URL regex to exclude '() from host string
_HTTP_STRICT_REGEX = r"""
    (?P<protocol>(https?|ftp|telnet|ldap|file)://)
    (?P<userinfo>([a-z0-9-._~!$&*+,;=:]|%[0-9A-F]{2})*@)?
    (?P<host>([a-z0-9-._~!$&\*+,;=]|%[0-9A-F]{2})*)
    (:(?P<port>\d*))?
    (/(?P<path>([^?\#| ]|%[0-9A-F]{2})*))?
    (\?(?P<query>([a-z0-9-._~!$&'()*+,;=:/?@]|%[0-9A-F]{2})*))?
    (\#(?P<fragment>([a-z0-9-._~!$&'()*+,;=:/?@]|%[0-9A-F]{2})*))?\b"""

_HTTP_STRICT_RGXC = re.compile(_HTTP_STRICT_REGEX, re.I | re.X | re.M)


# pylint: disable=too-many-return-statements, too-many-branches
def preprocess_observable(observable, ioc_type) -> SanitizedObservable:
    """
    Preprocesses and checks validity of observable against declared IoC type.

        :param observable: the value of the IoC
        :param ioc_type: the IoC type
    """
    observable = observable.strip()
    try:
        validated = _IOC_EXTRACT.validate(observable, ioc_type)
    except KeyError:
        validated = False
    if not validated:
        return SanitizedObservable(
            None, "Observable does not match expected pattern for " + ioc_type
        )
    if ioc_type == "url":
        return _preprocess_url(observable)
    if ioc_type == "ipv4":
        return _preprocess_ip(observable, version=4)
    if ioc_type == "ipv6":
        return _preprocess_ip(observable, version=6)
    if ioc_type in ["dns", "hostname"]:
        return _preprocess_dns(observable)
    if ioc_type in ["md5_hash", "sha1_hash", "sha256_hash", "file_hash"]:
        return _preprocess_hash(observable)
    return SanitizedObservable(observable, "ok")


# Would complicate code with too many branches
# pylint: disable=too-many-return-statements
def _preprocess_url(url: str) -> SanitizedObservable:
    """
    Check that URL can be parsed.

    Parameters
    ----------
    url : str
        the URL to check

    Returns
    -------
    SanitizedObservable
        Pre-processed result

    """
    clean_url, scheme, host = get_schema_and_host(url)

    if scheme is None or host is None:
        return SanitizedObservable(None, f"Could not obtain scheme or host from {url}")
    # get rid of some obvious false positives (localhost, local hostnames)
    try:
        addr = ip_address(host)
        if addr.is_private:
            return SanitizedObservable(None, "Host part of URL is a private IP address")
        if addr.is_loopback:
            return SanitizedObservable(
                None, "Host part of URL is a loopback IP address"
            )
    except ValueError:
        pass

    if "." not in host:
        return SanitizedObservable(None, "Host is unqualified domain name")

    if scheme.lower() in ["file"]:
        return SanitizedObservable(None, f"{scheme} URL scheme is not supported")

    return SanitizedObservable(clean_url, "ok")


def get_schema_and_host(url: str) -> Tuple[Optional[str], Optional[str], Optional[str]]:
    """
    Return URL scheme and host and cleaned URL.

    Parameters
    ----------
    url : str
        Input URL

    Returns
    -------
    Tuple[Optional[str], Optional[str], Optional[str]
        Tuple of URL, scheme, host

    """
    clean_url = None
    scheme = None
    host = None
    try:
        scheme, _, host, _, _, _, _ = parse_url(url)
        clean_url = url
    except LocationParseError:
        # Try to clean URL and re-check
        cleaned_url = _clean_url(url)
        if cleaned_url is not None:
            try:
                scheme, _, host, _, _, _, _ = parse_url(cleaned_url)
                clean_url = cleaned_url
            except LocationParseError:
                pass
    return clean_url, scheme, host


def _clean_url(url: str) -> Optional[str]:
    """
    Clean URL to remove query params and fragments and any trailing stuff.

    Parameters
    ----------
    url : str
        the URL to check

    Returns
    -------
    Optional[str]
        Cleaned URL or None if the input was not a valid URL

    """
    # Try to clean URL and re-check
    match_url = _HTTP_STRICT_RGXC.search(url)
    if (
        match_url.groupdict()["protocol"] is None
        or match_url.groupdict()["host"] is None
    ):
        return None

    # build the URL dropping the query string and fragments
    clean_url = match_url.groupdict()["protocol"]
    if match_url.groupdict()["userinfo"]:
        clean_url += match_url.groupdict()["userinfo"]
    clean_url += match_url.groupdict()["host"]
    if match_url.groupdict()["port"]:
        clean_url += ":" + match_url.groupdict()["port"]
    if match_url.groupdict()["path"]:
        clean_url += "/" + match_url.groupdict()["path"]

    return clean_url


# Would complicate code with too many branches
# pylint: disable=too-many-return-statements
def _preprocess_ip(ipaddress: str, version=4):
    """Ensure Ip address is a valid public IPv4 address."""
    try:
        addr = ip_address(ipaddress)
    except ValueError:
        return SanitizedObservable(None, "IP address is invalid format")

    if version == 4 and not isinstance(addr, IPv4Address):
        return SanitizedObservable(None, "Not an IPv4 address")
    if version == 6 and not isinstance(addr, IPv6Address):
        return SanitizedObservable(None, "Not an IPv6 address")
    if addr.is_global:
        return SanitizedObservable(ipaddress, "ok")

    return SanitizedObservable(None, "IP address is not global")


def _preprocess_dns(domain: str) -> SanitizedObservable:
    """Ensure DNS is a valid-looking domain."""
    if "." not in domain:
        return SanitizedObservable(None, "Domain is unqualified domain name")
    try:
        addr = ip_address(domain)
        del addr
        return SanitizedObservable(None, "Domain is an IP address")
    except ValueError:
        pass
    try:
        socket.gethostbyname(domain)
    except gaierror:
        return SanitizedObservable(None, "Domain not resolvable")

    return SanitizedObservable(domain, "ok")


def _preprocess_hash(hash_str: str) -> SanitizedObservable:
    """Ensure Hash has minimum entropy (rather than a string of 'x')."""
    str_entropy = entropy(hash_str)
    if str_entropy < 3.0:
        return SanitizedObservable(None, "String has too low an entropy to be a hash")
    return SanitizedObservable(hash_str, "ok")


def entropy(input_str: str) -> float:
    """Compute entropy of input string."""
    str_len = float(len(input_str))
    return -sum(
        map(
            lambda a: (a / str_len) * math.log2(a / str_len),
            Counter(input_str).values(),
        )
    )


@singledispatch
def generate_items(
    data: Any, obs_col: Optional[str] = None, ioc_type_col: Optional[str] = None
) -> Tuple[str, str]:
    """
    Generate item pairs from different input types.

    Parameters
    ----------
    data : Any
        DataFrame, dictionary or iterable
    obs_col : Optional[str]
        If `data` is a DataFrame, the column containing the observable value.
    ioc_type_col : Optional[str]
        If `data` is a DataFrame, the column containing the observable type.

    Returns
    -------
    Tuple[str, str] - a tuple of Observable/Type.

    """
    del obs_col, ioc_type_col
    if isinstance(data, Iterable):
        for item in data:
            yield item, None
    yield None, None


@generate_items.register(pd.DataFrame)
def _(data: pd.DataFrame, obs_col: str, ioc_type_col: Optional[str] = None):
    for _, row in data.iterrows():
        if ioc_type_col is None:
            yield row[obs_col], None
        else:
            yield row[obs_col], row[ioc_type_col]


@generate_items.register(dict)  # noqa: F811
def _(data: dict, obs_col: Optional[str] = None, ioc_type_col: Optional[str] = None):
    for obs, ioc_type in data.items():
        yield obs, ioc_type
