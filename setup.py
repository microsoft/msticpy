# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Setup script for msticpy."""
import os
import re
import setuptools


def install_requires_rtd(install_list: list) -> list:
    """Return modified install list if installing for ReadtheDocs."""
    rtd_exceptions = [
        "Kqlmagic",
        "azure-cli-core",
        "matplotlib",
        "statsmodels",
        "scipy",
        "splunk-sdk",
        "seaborn",
    ]
    return [
        pkg
        for pkg in install_list
        if not any(excl_pkg for excl_pkg in rtd_exceptions if excl_pkg in pkg)
    ]


with open("README.md", "r") as fh:
    LONG_DESC = fh.read()

# pylint: disable=locally-disabled, invalid-name
with open("msticpy/_version.py", "r") as fd:
    v_match = re.search(r'^VERSION\s*=\s*[\'"]([^\'"]*)[\'"]', fd.read(), re.MULTILINE)
    __version__ = v_match.group(1) if v_match else "no version"
# pylint: enable=locally-disabled, invalid-name

with open("requirements.txt", "r") as fh:
    INSTALL_REQUIRES = fh.readlines()

with open("requirements-dev.txt", "r") as fh:
    INSTALL_DEV_REQUIRES = fh.readlines()


def _combine_extras(extras: list) -> list:
    return list(
        {pkg for name, pkgs in EXTRAS.items() for pkg in pkgs if name in extras}
    )


# Extras definitions
EXTRAS = {
    "dev": INSTALL_DEV_REQUIRES,
    "vt3": ["vt-py>=0.6.1", "vt-graph-api>=1.0.1", "nest_asyncio>=1.4.0"],
    "splunk": ["splunk-sdk>=1.6.0"],
    "sumologic": ["sumologic-sdk>=0.1.11", "openpyxl>=3.0"],
    "kql": ["KqlmagicCustom[jupyter-extended]>=0.1.114.dev25"],
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
    "sql2kql": ["moz_sql_parser>=4.5.0,<=4.11.21016"],
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

# If ReadTheDocs build, remove a couple of problematic packages
# (we ask Sphinx to mock these in the import)
if os.environ.get("MP_RTD_BUILD"):
    INSTALL_REQUIRES = install_requires_rtd(INSTALL_REQUIRES)

setuptools.setup(
    name="msticpy",
    version=__version__,
    author="Ian Hellen",
    author_email="ianhelle@microsoft.com",
    description="MSTIC Security Tools",
    license="MIT License",
    long_description=LONG_DESC,
    long_description_content_type="text/markdown",
    url="https://github.com/microsoft/msticpy",
    project_urls={
        "Documentation": "https://msticpy.readthedocs.io",
        "Code": "https://github.com/microsoft/msticpy",
    },
    python_requires=">=3.6",
    packages=setuptools.find_packages(exclude=["tests", "tests.*", "*.tests.*"]),
    classifiers=[
        "Programming Language :: Python :: 3.6",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent",
        "Development Status :: 5 - Production/Stable",
    ],
    install_requires=INSTALL_REQUIRES,
    extras_require=EXTRAS,
    keywords=[
        "security",
        "azure",
        "sentinel",
        "mstic",
        "cybersec",
        "infosec",
        "cyber",
        "cybersecurity",
        "jupyter",
        "notebooks",
        "SOC",
        "hunting",
    ],
    zip_safe=False,
    include_package_data=True,
)
