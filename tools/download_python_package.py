# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Create MSTICPy install archive using docker."""
import argparse
import os
import subprocess
import time

VERSION = "1.0.0"

__version__ = VERSION
__author__ = "Chris Cianelli"


# pylint: disable=subprocess-run-check


def download_python_package(
    python_version: str, package_name: str, package_version: str, host_directory: str
):
    """
    Download a Python package and its dependencies for local use.

    Parameters
    ----------
    python_version: str
        Python version to use. Ex: "3.8.5" or "3.9"
    package_name: str
        Name of the module to download. Ex: "msticpy"
    package_version: str
        Version of the module to download. Ex: "1.0.0"
    host_directory: str
        Directory containing tar.gz files

    """
    os.makedirs(host_directory, exist_ok=True)
    try:
        # Generate a unique tag based on the current timestamp
        image_tag = f"{python_version}:{int(time.time())}"

        # get base name if module name includes additional dependencies
        module_base_name = package_name.split("[")[0]

        pipstring = (
            f"{package_name}=={package_version}" if package_version else package_name
        )

        # Define Dockerfile content
        dockerfile_content = f"""
            FROM python:{python_version}

            WORKDIR /{python_version}

            RUN apt-get update && \\
                apt-get install -y zip && \\
                rm -rf /var/lib/apt/lists/*

            ENV PACKAGE_NAME="{package_name}"
            ENV PIP_STRING="{pipstring}"

            RUN pip download "$PIP_STRING" -d /{python_version}

            RUN for file in *.tar.gz; do \\
                    if [ -f "$file" ]; then \\
                        pip wheel "$file" -w /{python_version}; \\
                        rm -f "$file"; \\
                    fi; \\
                done


            RUN zip -j /{python_version}/py{python_version}_$PACKAGE_NAME.zip /{python_version}/*.whl

            # Remove the wheel files
            RUN rm -f /{python_version}/*.whl
            RUN rm -f /{python_version}/*.tar.gz

            ENTRYPOINT ["echo", "Docker tasks completed."]
        """

        # Write Dockerfile content to a file
        with open("Dockerfile", "w", encoding="utf-8") as dockerfile:
            dockerfile.write(dockerfile_content)

        # Build Docker image with a unique tag
        docker_build_cmd = ["docker", "build", "-t", image_tag, "."]
        subprocess.run(docker_build_cmd, check=True)  # nosec

        # Run Docker container, copy files to temporary directory, and remove it after it's done
        docker_run_cmd = [
            "docker",
            "run",
            "-v",
            f"./{python_version}:/{python_version}",  # Bind-mount the temporary directory
            "--name",
            f"{module_base_name}",
            image_tag,
        ]
        subprocess.run(docker_run_cmd, check=True)

        print("copying files")

        subprocess.run(  # nosec
            ["docker", "cp", f"{module_base_name}:/{python_version}", host_directory],
            check=True,
        )

        print("removing container")

    finally:
        # Delete the Docker volume
        subprocess.run(["docker", "rm", f"{module_base_name}"])  # nosec

        subprocess.run(["docker", "volume", "rm", f"{python_version}"])  # nosec

        # Delete the Docker image
        subprocess.run(["docker", "rmi", image_tag])  # nosec


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Download Python package for local use"
    )
    parser.add_argument("-v", "--python-version", help="Python version", required=True)
    parser.add_argument(
        "-d", "--directory", help="Directory for saved zip file", required=True
    )
    parser.add_argument("-p", "--package-name", help="Package name", required=True)
    parser.add_argument(
        "-pv", "--package-version", help="Package version", required=False
    )

    args = parser.parse_args()

    download_python_package(
        args.python_version, args.package_name, args.package_version, args.directory
    )
