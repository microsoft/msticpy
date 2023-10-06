# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Script to create PIP Wheels from tar.gz files."""

import argparse
import os

VERSION = "1.0.0"

__version__ = VERSION
__author__ = "Chris Cianelli"


def build_wheel_from_targz(directory: str):
    """
    Build wheel files from tar.gz files in a directory.

    Parameters
    ----------
    directory: str
        Directory containing tar.gz files

    """
    files = [
        os.path.join(directory, filename)
        for filename in os.listdir(directory)
        if filename.endswith(".tar.gz")
    ]
    for file in files:
        os.system(f"python -m pip wheel {file} -w {directory}")  # nosec
        os.remove(file)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Build wheel files from tar.gz files in a directory"
    )
    parser.add_argument(
        "-d", "--directory", help="Directory for saved zip file", required=True
    )

    args = parser.parse_args()

    build_wheel_from_targz(args.directory)
