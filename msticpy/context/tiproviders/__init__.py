# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""TI Providers sub-package."""
import importlib

from ..._version import VERSION
from .http_provider import HttpTIProvider  # noqa:F401
from .lookup_result import LookupResult  # noqa:F401
from .preprocess_observable import preprocess_observable  # noqa:F401
from .ti_provider_base import TIProvider  # noqa:F401

__version__ = VERSION

TI_PROVIDERS = {
    "OTX": ("alienvault_otx", "OTX"),
    "AzSTI": ("azure_sent_byoti", "AzSTI"),
    "GreyNoise": ("greynoise", "GreyNoise"),
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
