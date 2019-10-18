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
import ssl
import json
import time
import requests
import dns.resolver
import OpenSSL
import pandas as pd
from ipywidgets import IntProgress
from IPython import display
import tldextract
from ..nbtools import pkg_config as config
from .._version import VERSION
from ..nbtools.utility import export

__version__ = VERSION
__author__ = "Pete Bryan"


@export
def screenshot(url: str, api_key: str = None) -> requests.models.Response:
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
    image_data: requests.models.Response
        The final screenshot request response data.

    """
    # Get Broshot API key from kwargs or config
    if api_key is not None:
        bs_api_key = api_key
    elif config.settings.get("Browshot") is not None:
        bs_api_key = config.settings.get("Browshot")["Args"]["AuthKey"]
    else:
        raise AttributeError("No configuration found for Browshot")
    # Request screenshot from Browshot and get request ID
    id_string = (
        f"https://api.browshot.com/api/v1/screenshot/create?url={url}/&instance_id=26&size=screen&cache=0&key={bs_api_key}"
    )
    id_data = requests.get(id_string)
    bs_id = json.loads(id_data.content)["id"]
    status_string = (
        f"https://api.browshot.com/api/v1/screenshot/info?id={bs_id}&key={bs_api_key}"
    )
    image_string = (
        f"https://api.browshot.com/api/v1/screenshot/thumbnail?id={bs_id}&zoom=50&key={bs_api_key}"
    )

    # Wait until the screenshot is ready and keep user updated with progress
    print("Getting screenshot")
    progress = IntProgress(min=0, max=40)
    display.display(progress)
    ready = False
    while ready is False:
        progress.value += 1
        status_data = requests.get(status_string)
        status = json.loads(status_data.content)["status"]
        if status == "finished":
            ready = True
        else:
            time.sleep(0.05)
    progress.value = 40

    # Once ready get the screenshot
    image_data = requests.get(image_string)

    if image_data.status_code != 200:
        print(
            "There was a problem with the request, please check the status code for details"
        )

    return image_data


@export
class DomainValidator():
    """Assess a domain's validity."""

    _TLD_URL = "http://data.iana.org/TLD/tlds-alpha-by-domain.txt"
    _SSLBL_URL = "https://sslbl.abuse.ch/blacklist/sslblacklist.csv"

    def __init__(self):
        """Pull IANA TLD list and save to internal attribute."""
        try:
            resp = requests.get(self._TLD_URL)
            self._tld_list = resp.content.decode().split('\n')[1:]
        except ConnectionError:
            self._tld_list = []

        try:
            self._ssl_bl = pd.read_csv(self._SSLBL_URL, skiprows=8)
        except ConnectionError:
            self._ssl_bl = pd.DataFrame({"SHA1": []})

    def validate_tld(self, url_domain: str):
        """
        Validate if a domain's TLD is public.

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
        if "." in tld:
            ttld = tld.split('.')[1].upper()
        else:
            ttld = tld.upper()

        if not tld or ttld not in self._tld_list:
            result = False
        else:
            result = True

        return result

    def is_resolvable(self, url_domain: str):
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
            dns.resolver.query(url_domain, 'A')
            result = True
        except:
            result = False

        return result

    def ssl_blacklisted(self, url_domain: str):
        """
        Validate if a domain or URL's SSL cert has been blacklisted.

        Parameters
        ----------
        url_domain : str
            The url or domain to validate.

        Returns
        -------
        result:
            True if valid blacklisted, False if not.

        """
        try:
            cert = ssl.get_server_certificate(url_domain, 443)
            x509 = OpenSSL.crypto.load_certificate(OpenSSL.crypto.FILETYPE_PEM, cert)
            cert_sha1 = x509.digest("sha1")
            if self._ssl_bl['SHA1'].str.contains(cert_sha1).any():
                result = True
            else:
                result = False
        except:
            result = False
            x509 = None

        return result, x509
