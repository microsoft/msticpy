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
import contextlib
import math  # noqa
import re
from collections import Counter
from functools import partial
from ipaddress import IPv4Address, IPv6Address, ip_address
from typing import Callable, List, Optional, Set, Tuple, Union
from urllib.parse import quote_plus

from urllib3.exceptions import LocationParseError
from urllib3.util import parse_url

from .._version import VERSION
from ..common.utility.format import refang_ioc
from ..transform.iocextract import IoCExtract
from .lookup_result import SanitizedObservable

__version__ = VERSION
__author__ = "Ian Hellen"

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


CheckerType = Callable[..., SanitizedObservable]


def _preprocess_url(url: str, **kwargs) -> SanitizedObservable:
    """
    Check that URL can be parsed.

    Parameters
    ----------
    url : str
        The URL to check
    require_url_encoding : bool
        Set to True if url's require encoding before passing to provider

    Returns
    -------
    SanitizedObservable
        Pre-processed result

    """
    require_url_encoding: bool = kwargs.pop("require_url_encoding", False)
    url = refang_ioc(ioc=url, ioc_type="url")
    clean_url, scheme, host = get_schema_and_host(url, require_url_encoding)

    if scheme is None or host is None:
        return SanitizedObservable(None, f"Could not obtain scheme or host from {url}")
    # get rid of some obvious false positives (localhost, local hostnames)
    with contextlib.suppress(ValueError):
        addr = ip_address(host)
        if addr.is_private:
            return SanitizedObservable(None, "Host part of URL is a private IP address")
        if addr.is_loopback:
            return SanitizedObservable(
                None, "Host part of URL is a loopback IP address"
            )
    if "." not in host:
        return SanitizedObservable(None, "Host is unqualified domain name")

    if scheme.lower() in ["file"]:
        return SanitizedObservable(None, f"{scheme} URL scheme is not supported")

    return SanitizedObservable(clean_url, "ok")


def get_schema_and_host(
    url: str, require_url_encoding: bool = False
) -> Tuple[Optional[str], Optional[str], Optional[str]]:
    """
    Return URL scheme and host and cleaned URL.

    Parameters
    ----------
    url : str
        Input URL
    require_url_encoding : bool
        Set to True if url needs encoding. Default is False.

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
            with contextlib.suppress(LocationParseError):
                scheme, _, host, _, _, _, _ = parse_url(cleaned_url)
                clean_url = cleaned_url
    if require_url_encoding and clean_url:
        clean_url = quote_plus(clean_url)
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
        not match_url
        or match_url.groupdict()["protocol"] is None
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


def _preprocess_ip(ipaddress: str, **kwargs):
    """Ensure Ip address is a valid public IPv4 address."""
    version = kwargs.pop("version", 4)
    ipaddress = refang_ioc(ioc=ipaddress, ioc_type="ipv4")
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


def _preprocess_dns(domain: str, **kwargs) -> SanitizedObservable:
    """Ensure DNS is a valid-looking domain."""
    del kwargs
    domain = refang_ioc(ioc=domain, ioc_type="dns")
    if "." not in domain:
        return SanitizedObservable(None, "Domain is unqualified domain name")
    with contextlib.suppress(ValueError):
        addr = ip_address(domain)
        del addr
        return SanitizedObservable(None, "Domain is an IP address")
    return SanitizedObservable(domain, "ok")


def _preprocess_hash(hash_str: str, **kwargs) -> SanitizedObservable:
    """Ensure Hash has minimum entropy (rather than a string of 'x')."""
    del kwargs
    str_entropy = _entropy(hash_str)
    if str_entropy < 3.0:
        return SanitizedObservable(None, "String has too low an entropy to be a hash")
    return SanitizedObservable(hash_str, "ok")


def _validate_ioc_type(observable, ioc_type):
    """Validate type matches IoCExtract regex rules."""
    try:
        validated = _IOC_EXTRACT.validate(observable, ioc_type)
    except KeyError:
        validated = False
    if not validated:
        return SanitizedObservable(
            None, f"Observable does not match expected pattern for {ioc_type}"
        )
    return SanitizedObservable(observable, "ok")


class PreProcessor:
    """Observable pre-processing class."""

    _TYPE_CHECK = "type_check"

    # Default processors
    _DEF_PROCESSORS: List[Tuple[Set[str], List[Union[str, CheckerType]]]] = [
        ({"url"}, [_TYPE_CHECK, _preprocess_url]),
        ({"ipv4"}, [_TYPE_CHECK, _preprocess_ip]),
        ({"ipv6"}, [_TYPE_CHECK, partial(_preprocess_ip, version=6)]),
        ({"dns", "hostname"}, [_TYPE_CHECK, _preprocess_dns]),
        (
            {"md5_hash", "sha1_hash", "sha256_hash", "file_hash"},
            [_TYPE_CHECK, _preprocess_hash],
        ),
    ]

    def __init__(self):
        """Initialize the processor dictionary."""
        self._processors = {
            obs_type: processors
            for types, processors in self._DEF_PROCESSORS
            for obs_type in types
        }

    def check(self, value: str, value_type: str, **kwargs) -> SanitizedObservable:
        """
        Apply processing checks to the input value.

        Parameters
        ----------
        value : str
            The value to be checked.
        value_type : str
            The type of value to be checked.

        Returns
        -------
        SanitizedObservable
            Named tuple with two members:

            1. observable - with the pre-processed result,
               This is set to None if a check fails.
            2. status - this is set to "ok" if the checks completed.
               Otherwise, it has an error message.

        """
        proc_value = value.strip()
        result = SanitizedObservable(proc_value, "ok")
        for processor in self._processors.get(value_type, []):
            if processor == self._TYPE_CHECK:
                result = _validate_ioc_type(proc_value, value_type)
            else:
                result = processor(proc_value, **kwargs)
            proc_value = result.observable
            if proc_value is None:
                break
        return result

    def add_check(self, value_type: str, checker: CheckerType):
        """Add a new checker to the processors."""
        if value_type not in self._processors:
            self._processors[value_type] = [checker]
        else:
            self._processors[value_type].append(checker)


def preprocess_observable(
    observable, ioc_type, require_url_encoding: bool = False
) -> SanitizedObservable:
    """
    Preprocess and check validity of observable against declared IoC type.

    Parameters
    ----------
    observable : _type_
        the value of the observable
    ioc_type : _type_
        The type of observable
    require_url_encoding : bool, optional
        If the observable needs URL-encoding (URL types only), by default False

    Returns
    -------
    SanitizedObservable
        Named tuple with two members:

        1. observable - with the pre-processed result,
            This is set to None if a check fails.
        2. status - this is set to "ok" if the checks completed.
            Otherwise, it has an error message.

    """
    processor = PreProcessor()
    return processor.check(
        value=observable, value_type=ioc_type, require_url_encoding=require_url_encoding
    )


def _entropy(input_str: str) -> float:
    """Compute entropy of input string."""
    str_len = float(len(input_str))
    return -sum(
        map(
            lambda a: (a / str_len) * math.log2(a / str_len),
            Counter(input_str).values(),
        )
    )
