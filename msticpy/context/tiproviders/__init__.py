# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""TI Providers sub-package."""
from typing import Dict, Tuple

from ..._version import VERSION
from ..preprocess_observable import preprocess_observable  # noqa:F401
from .ti_http_provider import HttpTIProvider  # noqa:F401
from .ti_provider_base import TIProvider  # noqa:F401

__version__ = VERSION

TI_PROVIDERS: Dict[str, Tuple[str, str]] = {
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
