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
from collections import namedtuple, Counter
# import json
# from json import JSONDecodeError
import math
import re
from typing import List, Mapping, Any, Tuple, Union, Iterable, Set, Optional

# from collections import namedtuple, Counter, abc
from ipaddress import IPv4Address, ip_address
import warnings

import socket
from socket import gaierror

import pandas as pd

# import requests
from urllib3.exceptions import LocationParseError
from urllib3.util import parse_url

from ..iocextract import IoCExtract, IoCType

from ...nbtools.utility import export
from ..._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


SanitizedObservable = namedtuple("SanitizedObservable", ["observable", "status"])


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


@export
class TIProvider(ABC):
    """Abstract base class for Threat Intel providers."""

    _ioc_extract = IoCExtract()

    # pylint: disable=unused-argument
    def __init__(self, **kwargs):
        """Initialize the provider."""
        self._supported_types: Set[IoCType] = set()

    @abc.abstractmethod
    def lookup_ioc(self, ioc: str, ioc_type: str = None) -> Tuple[bool, str]:
        """Lookup a single IoC observable."""
        pass

    @abc.abstractmethod
    def lookup_iocs(
        self,
        data: Union[pd.DataFrame, Mapping[str, str], Iterable[str]],
        obs_col: str = None,
        ioc_type_col: str = None,
    ) -> pd.DataFrame:
        """
        Lookup collection of IoC observables.

        Parameters
        ----------
        data : Union[pd.DataFrame,Mapping[str, str], Iterable[str]]
            Data input in one of three formats:
            1. Pandas dataframe (you must supply the column name in
            `obs_col` parameter)
            2. Mapping (e.g. a dict) of observable, IoCType
            3. Iterable of observables - IoCTypes will be inferred
        obs_col : str, optional
            DataFrame column to use for observables, by default None
        ioc_type_col : str, optional
            DataFrame column to use for IoCTypes, by default None

        Returns
        -------
        pd.DataFrame
            DataFrame of results.

        """
        pass

    @abc.abstractmethod
    @staticmethod
    def get_result(response: Any) -> bool:
        """
        Return True if the response indicates a hit.

        Parameters
        ----------
        response : Any
            The returned data response

        Returns
        -------
        bool
            True if a positive match

        """
        pass

    @abc.abstractmethod
    @staticmethod
    def get_result_details(response: Any) -> Any:
        """
        Return the details of the response.

        Parameters
        ----------
        response : Any
            The returned data response

        Returns
        -------
        Any
            Object with match details

        """
        pass

    def add_supported_type(self, ioc_type: Union[str, IoCType]):
        """
        Add an IoCType to list of supported types.

        Parameters
        ----------
        ioc_type : Union[str, IoCType]
            IoCType name or IoCType instance

        """
        if isinstance(ioc_type, str):
            parsed_ioc_type = IoCType.parse(ioc_type)
        if parsed_ioc_type != IoCType.unknown:
            self._supported_types.add(parsed_ioc_type)
        else:
            warnings.warn(f"Unknown IoCType {ioc_type}")

    def add_supported_types(self, ioc_types: Iterable[Union[str, IoCType]]):
        """
        Add a collection of supported IoC Types.

        Parameters
        ----------
        ioc_types : Iterable[Union[str, IoCType]]
            Iterable of IoCTypes to add.

        """
        for supp_type in ioc_types:
            self.add_supported_type(supp_type)

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
        return ioc_type in self._supported_types


def preprocess_observable(observable, ioc_type) -> SanitizedObservable:
    """
    Preprocesses and checks validity of observable against declared IoC type.

        :param observable: the value of the IoC
        :param ioc_type: the IoC type
    """
    observable = observable.strip()
    if not _IOC_EXTRACT.validate(observable, ioc_type):
        return SanitizedObservable(
            None, "Observable does not match expected pattern for " + ioc_type
        )
    if ioc_type == "url":
        return _preprocess_url(observable)
    if ioc_type == "ipv4":
        return _preprocess_ip4(observable)
    if ioc_type == "dns":
        return _preprocess_dns(observable)
    if ioc_type in ["md5_hash", "sha1_hash", "sha256_hash"]:
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
            return SanitizedObservable(
                None, "Host part of URL is a private IP address"
            )
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


def get_schema_and_host(
    url: str
) -> Tuple[Optional[str], Optional[str], Optional[str]]:
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
def _preprocess_ip4(ipaddress: str):
    """Ensure Ip address is a valid public IPv4 address."""
    try:
        addr = ip_address(ipaddress)
    except ValueError:
        return SanitizedObservable(None, "IP address is invalid format")

    if not isinstance(addr, IPv4Address):
        return SanitizedObservable(None, "Not an IPv4 address")
    if addr.is_global:
        return SanitizedObservable(ipaddress, "ok")
    if addr.is_private:
        return SanitizedObservable(None, "IP is private address")
    if addr.is_loopback:
        return SanitizedObservable(None, "IP is loopback address")
    if addr.is_reserved:
        return SanitizedObservable(None, "IP is reserved address")
    if addr.is_multicast:
        return SanitizedObservable(None, "IP is multicast address")
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
