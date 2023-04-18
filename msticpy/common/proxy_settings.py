# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Get proxy settings from config.

Settings format

.. code-block:: yaml

    msticpy:
        Proxies:
            http:
                Url: proxy_url
                UserName: user
                Password: [PLACEHOLDER]
            https:
                Url: proxy_url
                UserName: user
                Password: [PLACEHOLDER]

The entries for the username and password can be specified
as a string, an dictionary of EnvironmentVar: ENV_VAR_NAME
or a dictionary of one of the following:
- KeyVault: None
- KeyVault: secret_name
- KeyVault: vault_name/secret_name

"""
from typing import Dict, Optional

from .pkg_config import get_config
from .provider_settings import get_protected_setting


def get_http_proxies() -> Optional[Dict[str, str]]:
    """Return proxy settings from config."""
    proxy_config = get_config("msticpy.Proxies", None)
    if not proxy_config:
        return None
    proxy_dict = {}
    for protocol, settings in proxy_config.items():
        if "Url" not in settings:
            continue
        if "//" in settings["Url"]:
            prot_str, host = settings["Url"].split("//", maxsplit=1)
        else:
            prot_str, host = protocol, settings["Url"]
        user = (
            get_protected_setting("http.proxies.{protocol}", "UserName")
            if "UserName" in settings
            else None
        )
        password = (
            get_protected_setting("http.proxies.{protocol}", "Password")
            if "Password" in settings
            else None
        )
        auth_str = f"{user}:{password}@" if user else ""

        proxy_dict[protocol.casefold()] = f"{prot_str}//{auth_str}{host}"
    return proxy_dict
