import os
import sys
import argparse

VERSION = "1.0.0"

__version__ = VERSION
__author__ = "Chris Cianelli"


def build_wheel_from_targz(directory: str):
    """
    Build wheel files from tar.gz files in a directory

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
        os.system(f"pip wheel {file} -w {directory}")
        os.remove(file)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Build wheel files from tar.gz files in a directory"
    )
    parser.add_argument(
        "-d", "--directory", help="Directory for saved zip file", required=True
    )

    args = parser.parse_args()

    download_python_package(args.directory)
