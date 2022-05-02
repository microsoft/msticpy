# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""TI Providers sub-package."""
import importlib

from ..._version import VERSION

# from .alienvault_otx import OTX  # noqa:F401
# from .azure_sent_byoti import AzSTI  # noqa:F401
# from .greynoise import GreyNoise  # noqa:F401
from .http_base import HttpProvider  # noqa:F401

# from .ibm_xforce import XForce  # noqa:F401
# from .intsights import IntSights  # noqa:F401
# from .open_page_rank import OPR  # noqa:F401
from .ti_provider_base import (  # noqa:F401
    LookupResult,
    TIProvider,
    preprocess_observable,
)

# from .tor_exit_nodes import Tor  # noqa:F401
# from .virustotal import VirusTotal  # noqa:F401

try:
    from .riskiq import RiskIQ  # noqa:F401
except ImportError:
    pass

__version__ = VERSION

TI_PROVIDERS = {
    "OTX": ("alienvault_otx", "OTX"),
    "AzSTI": ("azure_sent_byoti", "AzSTI"),
    "GreyNoise": ("greynoise", "GreyNoise"),
    "HttpProvider": ("http_base", "HttpProvider"),
    "XForce": ("ibm_xforce", "XForce"),
    "IntSights": ("intsights", "IntSights"),
    "OPR": ("open_page_rank", "OPR"),
    "Tor": ("tor_exit_nodes", "Tor"),
    "VirusTotal": ("virustotal", "VirusTotal"),
    "RiskIQ": ("riskiq", "RiskIQ"),
}


def import_provider(provider: str) -> TIProvider:
    """Import provider class."""
    mod_name, cls_name = TI_PROVIDERS.get(provider, (None, None))

    if not (mod_name and cls_name):
        raise LookupError(
            f"No driver available for environment {provider}.",
            "Possible values are:",
            ", ".join(list(TI_PROVIDERS)),
        )

    imp_module = importlib.import_module(
        f"msticpy.context.tiproviders.{mod_name}", package="msticpy"
    )
    return getattr(imp_module, cls_name)
