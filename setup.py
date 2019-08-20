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
    "bokeh>=1.0.2",
    "deprecated>=1.2.4",
    "folium>=0.9.0",
    "ipython>=7.1.1",
    "ipywidgets>=7.4.2",
    "Kqlmagic>=0.1.94",
    "matplotlib>=3.0.0",
    "geoip2>=2.9.0",
    "networkx>=2.2",
    "numpy>=1.12.0",
    "pandas>=0.23.0",
    "requests>=2.20.1",
    "scikit_learn>=0.20.2",
    "setuptools>=40.6.2",
    "typing>=3.6.6",
    "urllib3>=1.24.0",
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
