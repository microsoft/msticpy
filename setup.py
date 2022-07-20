# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Setup script for msticpy."""
import re

import setuptools

with open("msticpy/_version.py", "r", encoding="utf-8") as fd:
    v_match = re.search(r'^VERSION\s*=\s*[\'"]([^\'"]*)[\'"]', fd.read(), re.MULTILINE)
    __version__ = v_match.group(1) if v_match else "no version"

with open("requirements.txt", "r", encoding="utf-8") as fh:
    INSTALL_REQUIRES = fh.readlines()

with open("requirements-dev.txt", "r", encoding="utf-8") as fh:
    INSTALL_DEV_REQUIRES = fh.readlines()


def _combine_extras(extras: list) -> list:
    return list(
        {pkg for name, pkgs in EXTRAS.items() for pkg in pkgs if name in extras}
    )


# Extras definitions
EXTRAS = {
    "dev": INSTALL_DEV_REQUIRES,
    "vt3": ["vt-py>=0.6.1", "vt-graph-api>=2.0", "nest_asyncio>=1.4.0"],
    "splunk": ["splunk-sdk>=1.6.0"],
    "sumologic": ["sumologic-sdk>=0.1.11", "openpyxl>=3.0"],
    "kql": ["KqlmagicCustom[jupyter-extended]>=0.1.114"],
    "_azure_core": [
        "azure-mgmt-compute>=4.6.2",
        "azure-mgmt-core>=1.2.1",
        "azure-mgmt-monitor>=2.0.0",
        "azure-mgmt-network>=2.7.0",
        "azure-mgmt-resource>=16.1.0",
        "azure-storage-blob>=12.5.0",
        "azure-mgmt-resourcegraph>=8.0.0",
    ],
    "keyvault": [
        "azure-keyvault-secrets>=4.0.0",
        "azure-mgmt-keyvault>=2.0.0",
        "keyring>=13.2.1",  # needed by Key Vault package
    ],
    "ml": ["scikit-learn>=0.20.2", "scipy>=1.1.0", "statsmodels>=0.11.1"],
    "sql2kql": ["moz_sql_parser>=4.5.0,<=4.40.21126"],
    "riskiq": ["passivetotal>=2.5.3"],
}
extras_all = [
    extra for name, extras in EXTRAS.items() for extra in extras if name != "dev"
]
EXTRAS["all"] = extras_all

# Create combination extras
EXTRAS["all"] = sorted(
    _combine_extras(list({name for name in EXTRAS if name != "dev"}))
)

EXTRAS["azure"] = sorted(_combine_extras(["_azure_core", "keyvault"]))
EXTRAS["test"] = sorted(_combine_extras(["all", "dev"]))
EXTRAS["azsentinel"] = sorted(_combine_extras(["azure", "kql", "keyvault"]))
EXTRAS["azuresentinel"] = sorted(_combine_extras(["azure", "kql", "keyvault"]))
EXTRAS["sentinel"] = sorted(_combine_extras(["azure", "kql", "keyvault"]))


if __name__ == "__main__":
    setuptools.setup(
        install_requires=INSTALL_REQUIRES, extras_require=EXTRAS, version=__version__
    )
