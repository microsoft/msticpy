# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Setup script for msticpy."""

import re
import setuptools


INSTALL_REQUIRES = [
    "attrs>=18.2.0",
    "azure-common>=1.1.18",
    "azure-mgmt-resource>=2.2.0",
    "azure-mgmt-subscription>=0.2.0",
    "azure-mgmt-network>=2.7.0",
    "azure-mgmt-monitor>=0.5.2",
    "azure-mgmt-compute>=4.6.2",
    "beautifulsoup4>=4.8.0",
    "bokeh>=1.4.0",
    "cryptography>=2.8",
    "deprecated>=1.2.4",
    "dnspython>=1.16.0",
    "folium>=0.9.0",
    "geoip2>=2.9.0",
    "ipwhois>=1.1.0",
    "ipython>=7.2.0",
    "ipywidgets>=7.4.2",
    "Kqlmagic>=0.1.106",
    "matplotlib>=3.0.2",
    "networkx>=2.2",
    "numpy>=1.15.4",
    "pandas>=0.25.0",
    "pytz>=2019.2",
    "pyyaml>=3.13",
    "requests>=2.21.1",
    "scikit-learn>=0.20.2",
    "seaborn>=0.9.0",
    "setuptools>=40.6.3",
    "tldextract>=2.2.2",
    "tqdm>=4.36.1",
    "typing>=3.6.6",
    "urllib3>=1.24.1",
]


# pylint: disable=locally-disabled, invalid-name
with open("README.md", "r") as fh:
    long_description = fh.read()

with open("msticpy/_version.py", "r") as fd:
    v_match = re.search(r'^VERSION\s*=\s*[\'"]([^\'"]*)[\'"]', fd.read(), re.MULTILINE)
    __version__ = v_match.group(1) if v_match else "no version"
# pylint: enable=locally-disabled, invalid-name

setuptools.setup(
    name="msticpy",
    version=__version__,
    author="Ian Hellen",
    author_email="ianhelle@microsoft.com",
    description="MSTIC Security Tools",
    license="MIT License",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/microsoft/msticpy",
    project_urls={
        "Documentation": "https://msticpy.readthedocs.io",
        "Code": "https://github.com/microsoft/msticpy",
    },
    python_requires=">=3.6",
    packages=setuptools.find_packages(exclude=["*.tests"]),
    classifiers=[
        "Programming Language :: Python :: 3.6",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent",
        "Development Status :: 4 - Beta",
    ],
    install_requires=INSTALL_REQUIRES,
    keywords=["security", "azure", "sentinel"],
    zip_safe=False,
    include_package_data=True,
)
