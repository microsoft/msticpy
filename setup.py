# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Setup script for msticpy."""

import re

import setuptools

INSTALL_REQUIRES = ['matplotlib>=3.0.0',
                    'bokeh>=1.0.2',
                    'setuptools>=40.6.2',
                    'attrs>=18.2.0',
                    'pandas>=0.23.0',
                    'requests>=2.20.1',
                    'networkx>=2.2',
                    'numpy>=1.12.0',
                    'urllib3>=1.24.0',
                    'ipywidgets>=7.4.2',
                    'ipython>=7.1.1',
                    'Kqlmagic>=0.1.90',
                    'scikit_learn>=0.20.2',
                    'maxminddb_geolite2>=2018.0',
                    'typing>=3.6.6']


# pylint: disable=locally-disabled, C0103
with open("Readme.md", "r") as fh:
    long_description = fh.read()
# pylint: enable=locally-disabled, C0103

with open("msticpy/_version.py", "r") as fd:
    __version__ = re.search(r'^VERSION\s*=\s*[\'"]([^\'"]*)[\'"]', fd.read(), re.MULTILINE).group(1)

setuptools.setup(
    name="msticpy",
    version=__version__,
    author="Ian Hellen",
    author_email="ianhelle@microsoft.com",
    description="MSTIC Security Tools",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://https://github.com/ianhelle/msyticpy",
    python_requires='>=3.6',
    packages=setuptools.find_packages(exclude=['*.tests']),
    classifiers=[
        "Programming Language :: Python :: 3.6",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent",
    ],
    install_requires=INSTALL_REQUIRES
)
