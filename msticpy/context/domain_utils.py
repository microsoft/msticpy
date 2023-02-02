# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Functions to support investigation of a domain or url.

Includes functions to conduct common investigation steps when dealing
with a domain or url, such as getting a screenshot or validating the TLD.

"""
import json
import ssl
import time
from datetime import datetime
from enum import Enum
from typing import Any, Dict, Optional, Tuple
from urllib.error import HTTPError, URLError

import cryptography as crypto
import httpx
import pandas as pd
import tldextract
from cryptography.x509 import Certificate
from dns.exception import DNSException
from dns.resolver import Resolver
from IPython import display
from ipywidgets import IntProgress
from urllib3.exceptions import LocationParseError
from urllib3.util import parse_url

from .._version import VERSION
from ..common import pkg_config as config
from ..common.exceptions import MsticpyUserConfigError
from ..common.utility import export, mp_ua_header

__version__ = VERSION
__author__ = "Pete Bryan"


@export
def screenshot(url: str, api_key: str = None) -> httpx.Response:
    """
    Get a screenshot of a url with Browshot.

    Parameters
    ----------
    url : str
        The url a screenshot is wanted for.
    api_key : str (optional)
        Browshot API key. If not set msticpyconfig checked for this.

    Returns
    -------
    image_data: httpx.Response
        The final screenshot request response data.

    """
    # Get Browshot API key from kwargs or config
    if api_key is not None:
        bs_api_key: Optional[str] = api_key
    else:
        bs_conf = config.settings.get("DataProviders", {}).get(
            "Browshot"
        ) or config.settings.get("Browshot")
        bs_api_key = None
        if bs_conf is not None:
            bs_api_key = bs_conf.get("Args", {}).get("AuthKey")  # type: ignore

    if bs_api_key is None:
        raise MsticpyUserConfigError(
            "No configuration found for Browshot",
            "Please add a section to msticpyconfig.yaml:",
            "DataProviders:",
            "  Browshot:",
            "    Args:",
            "      AuthKey: {your_auth_key}",
            title="Browshot configuration not found",
            browshot_uri=("Get an API key for Browshot", "https://api.browshot.com/"),
        )

    # Request screenshot from Browshot and get request ID
    id_string = (
        f"https://api.browshot.com/api/v1/screenshot/create?url={url}/"
        f"&instance_id=26&size=screen&cache=0&key={bs_api_key}"
    )
    id_data = httpx.get(
        id_string, timeout=config.get_http_timeout(), headers=mp_ua_header()
    )
    bs_id = json.loads(id_data.content)["id"]
    status_string = (
        f"https://api.browshot.com/api/v1/screenshot/info?id={bs_id}&key={bs_api_key}"
    )
    image_string = (
        f"https://api.browshot.com/api/v1/screenshot/thumbnail?id={bs_id}"
        f"&zoom=50&key={bs_api_key}"
    )
    # Wait until the screenshot is ready and keep user updated with progress
    print("Getting screenshot")
    progress = IntProgress(min=0, max=100)
    display.display(progress)
    ready = False
    while not ready and progress.value < 100:
        progress.value += 1
        status_data = httpx.get(
            status_string, timeout=config.get_http_timeout(), headers=mp_ua_header()
        )
        status = json.loads(status_data.content)["status"]
        if status == "finished":
            ready = True
        else:
            time.sleep(0.05)
    progress.value = 100

    # Once ready or timed out get the screenshot
    image_data = httpx.get(image_string, timeout=config.get_http_timeout())

    if image_data.status_code != 200:
        print(
            "There was a problem with the request, please check the status code for details"
        )

    return image_data


# Backward compat with dnspython 1.x
# If v2.x installed use non-deprecated "resolve" method
# otherwise use "query"
_dns_resolver = Resolver()
if hasattr(_dns_resolver, "resolve"):
    _dns_resolve = getattr(_dns_resolver, "resolve")
else:
    _dns_resolve = getattr(_dns_resolver, "query")


@export
class DomainValidator:
    """Assess a domain's validity."""

    _ssl_abuse_list: pd.DataFrame = pd.DataFrame()

    @classmethod
    def _check_and_load_abuselist(cls):
        """Pull IANA TLD list and save to internal attribute."""
        if cls._ssl_abuse_list is None or cls._ssl_abuse_list.empty:
            cls._ssl_abuse_list: pd.DataFrame = cls._get_ssl_abuselist()

    @property
    def ssl_abuse_list(self) -> pd.DataFrame:
        """
        Return the class SSL Blacklist.

        Returns
        -------
        pd.DataFrame
            SSL Blacklist

        """
        self._check_and_load_abuselist()
        return self._ssl_abuse_list

    @staticmethod
    def validate_tld(url_domain: str) -> bool:
        """
        Validate if a domain's TLD is valid.

        Parameters
        ----------
        url_domain : str
            The url or domain to validate.

        Returns
        -------
        result:
            True if valid public TLD, False if not.

        """
        _, _, tld = tldextract.extract(url_domain.lower())
        return bool(tld)

    @staticmethod
    def is_resolvable(url_domain: str) -> bool:
        """
        Validate if a domain or URL be be resolved to an IP address.

        Parameters
        ----------
        url_domain : str
            The url or domain to validate.

        Returns
        -------
        result:
            True if valid resolvable, False if not.

        """
        try:
            _dns_resolve(url_domain, "A")
            return True
        except DNSException:
            return False

    def in_abuse_list(self, url_domain: str) -> Tuple[bool, Optional[Certificate]]:
        """
        Validate if a domain or URL's SSL cert the abuse.ch SSL Abuse List.

        Parameters
        ----------
        url_domain : str
            The url or domain to validate.

        Returns
        -------
        Tuple[bool, Optional[Certificate]]:
            True if valid in the list, False if not.
            Certificate - the certificate loaded from the domain.

        """
        x509: Optional[Certificate]
        try:
            cert = ssl.get_server_certificate((url_domain, 443))
            x509 = crypto.x509.load_pem_x509_certificate(  # type: ignore
                cert.encode("ascii")
            )
            cert_sha1 = x509.fingerprint(
                crypto.hazmat.primitives.hashes.SHA1()  # type: ignore # nosec
            )
            result = bool(
                self.ssl_abuse_list["SHA1"]
                .str.contains(cert_sha1.hex())
                .any()  # type: ignore
            )
        except Exception:  # pylint: disable=broad-except
            result = False
            x509 = None

        return result, x509

    @classmethod
    def _get_ssl_abuselist(cls) -> pd.DataFrame:
        """Download and load abuse.ch SSL Abuse List."""
        try:
            ssl_ab_list = pd.read_csv(
                "https://sslbl.abuse.ch/blacklist/sslblacklist.csv", skiprows=8
            )
        except (ConnectionError, HTTPError, URLError):
            ssl_ab_list = pd.DataFrame({"SHA1": []})

        return ssl_ab_list


def dns_components(domain: str) -> dict:
    """
    Return components of domain as dict.

    Parameters
    ----------
    domain : str
        The domain to extract.

    Returns
    -------
    dict:
        Returns subdomain and TLD components from a domain.

    """
    return tldextract.extract(domain.lower())._asdict()


def url_components(url: str) -> Dict[str, str]:
    """Return parsed Url components as dict."""
    try:
        return parse_url(url)._asdict()
    except LocationParseError:
        return {}


@export
def dns_resolve(url_domain: str, rec_type: str = "A") -> Dict[str, Any]:
    """
    Validate if a domain or URL be be resolved to an IP address.

    Parameters
    ----------
    url_domain : str
        The url or domain to validate.
    rec_type : str
        The DNS record type to query, by default "A"

    Returns
    -------
    Dict[str, Any]:
        Resolver result as dictionary.

    """
    domain = parse_url(url_domain).host
    try:
        return _resolve_resp_to_dict(_dns_resolve(domain, rdtype=rec_type))
    except DNSException as err:
        return {
            "qname": domain,
            "rdtype": rec_type,
            "response": str(err),
        }


@export
def dns_resolve_df(url_domain: str, rec_type: str = "A") -> pd.DataFrame:
    """
    Validate if a domain or URL be be resolved to an IP address.

    Parameters
    ----------
    url_domain : str
        The url or domain to validate.
    rec_type : str
        The DNS record type to query, by default "A"

    Returns
    -------
    pd.DataFrame:
        Resolver result as dataframe with individual resolution
        results as separate rows.

    """
    results = pd.DataFrame([dns_resolve(url_domain, rec_type)])
    if "rrset" in results.columns:
        return results.explode(column="rrset")
    return results


@export
def ip_rev_resolve(ip_address: str) -> Dict[str, Any]:
    """
    Reverse lookup for IP Address.

    Parameters
    ----------
    ip_address : str
        The IP address to query.

    Returns
    -------
    Dict[str, Any]:
        Resolver result as dictionary.

    """
    try:
        return _resolve_resp_to_dict(_dns_resolve(ip_address, raise_on_no_answer=True))
    except DNSException as err:
        return {
            "qname": ip_address,
            "rdtype": "PTR",
            "response": str(err),
        }


@export
def ip_rev_resolve_df(ip_address: str) -> pd.DataFrame:
    """
    Reverse lookup for IP Address.

    Parameters
    ----------
    ip_address : str
        The IP address to query.

    Returns
    -------
    pd.DataFrame:
        Resolver result as dataframe with individual resolution
        results as separate rows.

    """
    results = pd.DataFrame([ip_rev_resolve(ip_address)])
    if "rrset" in results.columns:
        return results.explode(column="rrset")
    return results


@export
def _resolve_resp_to_dict(resolver_resp):
    """Return Dns Python resolver response to dict."""
    rdtype = (
        resolver_resp.rdtype.name
        if isinstance(resolver_resp.rdtype, Enum)
        else str(resolver_resp.rdtype)
    )
    rdclass = (
        resolver_resp.rdclass.name
        if isinstance(resolver_resp.rdclass, Enum)
        else str(resolver_resp.rdclass)
    )

    return {
        "qname": str(resolver_resp.qname),
        "rdtype": rdtype,
        "rdclass": rdclass,
        "response": str(resolver_resp.response),
        "nameserver": getattr(resolver_resp, "nameserver", None),
        "port": getattr(resolver_resp, "port", None),
        "canonical_name": str(resolver_resp.canonical_name),
        "rrset": [str(res) for res in resolver_resp.rrset],
        "expiration": datetime.utcfromtimestamp(resolver_resp.expiration),
    }
