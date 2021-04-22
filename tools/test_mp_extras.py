# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Script to test msticpy extras."""
import argparse
from datetime import datetime
from pathlib import Path
import pkg_resources
import subprocess
import sys

__author__ = "Ian Hellen"

base_pkgs = [
    "argon2-cffi",
    "async-generator",
    "atomicwrites",
    "attrs",
    "backcall",
    "bleach",
    "cffi",
    "colorama",
    "decorator",
    "defusedxml",
    "entrypoints",
    "iniconfig",
    "ipykernel",
    "ipython",
    "ipython-genutils",
    "ipywidgets",
    "jedi",
    "Jinja2",
    "jsonschema",
    "jupyter",
    "jupyter-client",
    "jupyter-console",
    "jupyter-core",
    "jupyterlab-pygments",
    "MarkupSafe",
    "mistune",
    "nbclient",
    "nbconvert",
    "nbformat",
    "nest-asyncio",
    "notebook",
    "packaging",
    "pandocfilters",
    "parso",
    "pickleshare",
    "pip",
    "pluggy",
    "prometheus-client",
    "prompt-toolkit",
    "py",
    "pycparser",
    "Pygments",
    "pyparsing",
    "pyrsistent",
    "pytest",
    "python-dateutil",
    "pywin32",
    "pywinpty",
    "pyzmq",
    "qtconsole",
    "QtPy",
    "Send2Trash",
    "setuptools",
    "six",
    "terminado",
    "testpath",
    "toml",
    "tornado",
    "traitlets",
    "wcwidth",
    "webencodings",
    "wheel",
    "widgetsnbextension",
]


# pylint: disable=subprocess-run-check

VERB_ARGS = {"stdout": sys.stdout, "stderr": sys.stderr}


def install_pkg(extra: str, path: str, version: str, verbose: bool):
    """
    Install msticpy with extra from distrib path.

    Parameters
    ----------
    extra : str
        extra to install (default none)
    path : str
        path of the distribution
    version : str
        the version number of the package to install
    verbose : bool
        Emit verbose output for subproceses.


    """
    sp_run = [
        "python",
        "-m",
        "pip",
        "install",
        "-f",
        f"{path}/dist",
        "msticpy{extra_spec}=={ver}".format(
            extra_spec=f"[{extra}]" if extra else "",
            ver=version,
        ),
    ]

    print(f"Installing msticpy from {path}, extra={extra}")
    start = datetime.now()
    print("start", start)
    print(sp_run)
    if verbose:
        print(" ".join(sp_run))
    subprocess.run(sp_run, check=True, **(VERB_ARGS if verbose else {}))  # type: ignore

    end = datetime.now()
    print("end", end)
    print("duration", end - start)


def reset_pkgs(verbose: bool):
    """Reset enviroment - remove all non-core msticpy packages."""
    sp_run = [
        "python",
        "-m",
        "pip",
        "list",
    ]
    inst_pkgs = _get_installed_pkgs()
    remove_pkgs = inst_pkgs - set(base_pkgs)
    # don't deinstall these packages
    remove_pkgs = remove_pkgs - set(("pip", "setuptools", "wheel"))

    if not remove_pkgs:
        print("No packages to uninstall")
        return
    sp_run.remove("list")
    sp_run.extend(["uninstall", "-y", *remove_pkgs])
    print("Removing non-core packages")
    print(sp_run)
    if verbose:
        print(" ".join(sp_run))
    subprocess.run(sp_run, check=True, **(VERB_ARGS if verbose else {}))  # type: ignore


def show_dist(path: str):
    """List current distributions."""
    dist_vers = Path(path).joinpath("dist").glob("*.tar.gz")
    for dist in dist_vers:
        print(str(dist.name).replace(".tar.gz", ""))


def run_tests(path: str, verbose: bool):
    """Run pytest on `path`."""
    sp_run = ["pytest"]
    inst_pkgs = _get_installed_pkgs()
    if "pytest-xdist" in inst_pkgs:
        sp_run.extend(["-n", "auto"])
    sp_run.append(path)
    print("Running tests")
    if verbose:
        print(" ".join(sp_run))
    subprocess.run(sp_run, cwd=path, check=True, **(VERB_ARGS if verbose else {}))  # type: ignore


def _get_installed_pkgs():
    sp_run = [
        "python",
        "-m",
        "pip",
        "list",
    ]
    proc_call = subprocess.run(sp_run, check=True, stdout=subprocess.PIPE)  # type: ignore
    inst_pkgs = proc_call.stdout.decode("utf-8").split("\n")[2:]
    return {pkg.split()[0] for pkg in inst_pkgs if pkg.strip()}


def create_baseline(output=None):
    """Create baseline file for current packages."""
    output = output or "baseline_pkg.txt"
    with open(output, "w") as bl_file:
        bl_file.write("\n".join(sorted(_get_installed_pkgs())))
    print(f"baseline packages written to {output}")


def make_dist(path: str, verbose: bool):
    """Create distrib at `path`."""
    sp_run = [
        "python",
        "setup.py",
        "sdist",
        "bdist_wheel",
    ]
    print("Creating distrib wheel")
    if verbose:
        print(" ".join(sp_run))
    subprocess.run(sp_run, cwd=path, **(VERB_ARGS if verbose else {}))


def _read_base_pkg_list(pkg_file):
    with open(pkg_file, "r") as pkg_fh:
        pkg_lines = pkg_fh.readlines()
    for pkg_line in pkg_lines:
        try:
            req = pkg_resources.Requirement.parse(pkg_line.strip())
        except Exception:  # pylint: disable=broad-except
            pass
        yield req.name


def _add_script_args():
    parser = argparse.ArgumentParser(description="Msticpy extras test script.")
    parser.add_argument(
        "cmd",
        choices=["install", "reset", "test", "makedist", "showdist", "baseline"],
        help="\n".join(
            [
                "Run command: [install | reset | test | makedist | showdist | baseline]",
                (
                    "install - install msticpy from a dist folder (--path) with option extras"
                    " (specified as a string with the --extras argument."
                ),
                (
                    "reset - uninstall all packages apart from the baseline"
                    " (baseline package file is specified with --base-packages argument)."
                ),
                "test - run pytest tests against current install.",
                "makedist - create a setuptools distribution from --path",
                "showdist - list the distributions in the 'dist' folder in --path",
                (
                    "baseline - create a baseline requirements file from current packages"
                    " (specify output file as --output, default is baseline_pkg.txt"
                ),
            ]
        ),
    )
    parser.add_argument(
        "--extra",
        "-e",
        required=False,
        default=None,
        help="Name of extra",
    )
    parser.add_argument(
        "--path",
        "-p",
        required=False,
        default="/src/microsoft/msticpy",
        help="Path to root of msticpy repo",
    )
    parser.add_argument(
        "--version",
        "-n",
        required=False,
        help="Version of msticpy to install",
    )
    parser.add_argument(
        "--base-pkgs",
        "-b",
        required=False,
        default=None,
        help="File with base package list (for reset).",
    )
    parser.add_argument(
        "--output",
        "-o",
        required=False,
        default=None,
        help="Specify file to store base package list (for reset).",
    )
    parser.add_argument(
        "--verbose",
        "-v",
        action="store_true",
        required=False,
        default=False,
        help="Show full output of commands.",
    )
    return parser


# pylint: disable=invalid-name
if __name__ == "__main__":
    arg_parser = _add_script_args()
    args = arg_parser.parse_args()

    if args.cmd.casefold() == "install":
        install_pkg(args.extra, args.path, args.version, args.verbose)

    if args.cmd.casefold() == "baseline":
        create_baseline(args.output)

    if args.cmd.casefold() == "reset":
        if args.base_pkgs:
            base_pkgs = list(_read_base_pkg_list(args.base_pkgs))
        reset_pkgs(args.verbose)

    if args.cmd.casefold() == "test":
        run_tests(args.path, args.verbose)

    if args.cmd.casefold() == "makedist":
        make_dist(args.path, args.verbose)

    if args.cmd.casefold() == "showdist":
        show_dist(args.path)
