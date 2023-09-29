# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Script for installing wheel files to isolated environment."""

import argparse
import os

VERSION = "1.0.0"

__version__ = VERSION
__author__ = "Chris Cianelli"


def install_all_whl_files(directory: str):
    """
    Install all wheel files in a directory.

    Parameters
    ----------
    directory: str
        Directory containing wheel files

    """
    files = [
        os.path.join(directory, filename)
        for filename in os.listdir(directory)
        if filename.endswith(".whl")
    ]
    for file in files:
        os.system(  # nosec
            f"python -m pip install --quiet --no-index --no-deps --find-links . {file} --user"
        )
        print(f"Installed {os.path.split(file)[-1]}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Build wheel files from tar.gz files in a directory"
    )
    parser.add_argument(
        "-d", "--directory", help="Directory for saved zip file", required=True
    )

    args = parser.parse_args()

    install_all_whl_files(args.directory)
