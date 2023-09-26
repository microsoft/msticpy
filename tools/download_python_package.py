import os
import subprocess
import time
import shutil
import sys
import argparse

VERSION = "1.0.0"

__version__ = VERSION
__author__ = "Chris Cianelli"


def download_python_package(
    python_version: str, module_name: str, module_version: str, host_directory: str
):
    os.makedirs(host_directory, exist_ok=True)
    try:
        # Generate a unique tag based on the current timestamp
        image_tag = f"{python_version}:{int(time.time())}"

        # get base name if module name includes additional dependencies
        module_base_name = module_name.split("[")[0]

        # Define Dockerfile content
        dockerfile_content = f"""
            FROM python:{python_version}

            WORKDIR /{python_version}

            RUN apt-get update && \\
                apt-get install -y zip && \\
                rm -rf /var/lib/apt/lists/*

            ENV MODULE_NAME="{module_name}"
            ENV MODULE_VERSION="{module_version}"

            RUN pip download "${{MODULE_NAME}}==${{MODULE_VERSION}}" -d /{python_version}

            RUN for file in *.tar.gz; do \\
                    if [ -f "$file" ]; then \\
                        pip wheel "$file" -w /{python_version}; \\
                        rm -f "$file"; \\
                    fi; \\
                done
            

            RUN zip -j /{python_version}/$MODULE_NAME.zip /{python_version}/*.whl

            # Remove the wheel files
            RUN rm -f /{python_version}/*.whl
            RUN rm -f /{python_version}/*.tar.gz

            ENTRYPOINT ["echo", "Docker tasks completed."]
        """

        # Write Dockerfile content to a file
        with open("Dockerfile", "w") as dockerfile:
            dockerfile.write(dockerfile_content)

        # Build Docker image with a unique tag
        docker_build_cmd = ["docker", "build", "-t", image_tag, "."]
        subprocess.run(docker_build_cmd, check=True)

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

        subprocess.run(
            ["docker", "cp", f"{module_base_name}:/{python_version}", host_directory],
            check=True,
        )

        print("removing container")

    finally:
        # Delete the Docker volume
        subprocess.run(["docker", "rm", f"{module_base_name}"])

        subprocess.run(["docker", "volume", "rm", f"{python_version}"])

        # Delete the Docker image
        subprocess.run(["docker", "rmi", image_tag])


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Download Python modules for local use"
    )
    parser.add_argument("-v", "--python-version", help="Python version", required=True)
    parser.add_argument("-m", "--module-name", help="Module name", required=True)
    parser.add_argument("-mv", "--module-version", help="Module version", required=True)
    parser.add_argument(
        "-d", "--directory", help="Directory for saved zip file", required=True
    )

    args = parser.parse_args()

    download_python_package(
        args.python_version, args.module_name, args.module_version, args.directory
    )
