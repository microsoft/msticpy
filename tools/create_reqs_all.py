# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Requirements file writer from setup.py extras."""

from __future__ import annotations

import argparse
import difflib
import sys
from importlib import import_module
from pathlib import Path

# Import Requirement with fallbacks for isolated environments (e.g., pre-commit)
try:
    from packaging.requirements import Requirement
except ImportError:
    try:
        from importlib_metadata import Requirement  # type: ignore[assignment]
    except ImportError:
        # Suppress deprecation warning in isolated environments where we have no choice
        import warnings

        warnings.filterwarnings("ignore", ".*pkg_resources.*", DeprecationWarning)
        from pkg_resources import Requirement  # type: ignore[assignment]

from setuptools.config import read_configuration

VERSION = "1.0.0"

__version__ = VERSION
__author__ = "Ian Hellen"


def parse_requirements(requirements: list[str]) -> list[Requirement]:
    """Parse a list of requirement strings into Requirement objects."""
    parsed_reqs = set()
    for req in requirements:
        if not req.strip() or req.strip().startswith("#"):
            continue

        # Remove trailing comments and strip whitespace
        req_clean = req.split("#")[0].strip()
        if req_clean:
            parsed_reqs.add(Requirement(req_clean))

    return list(parsed_reqs)


_PIPFILE_TEMPLATE = """
[[source]]
url = "https://pypi.org/simple"
verify_ssl = true
name = "pypi"

[packages]
{packages}

[dev-packages]
{dev_packages}

[requires]
python_version = "{py_ver}"
"""


def _add_script_args():
    """Define script arguments."""
    parser = argparse.ArgumentParser(description=f"Requirements sync script. v.{VERSION}")
    parser.add_argument(
        "--req-all-path",
        "-r",
        default="./requirements-all.txt",
        required=False,
        help="Path of requirements-all.txt file",
    )
    parser.add_argument(
        "--pipfile",
        "-i",
        default=False,
        action="store_true",
        required=False,
        help="Generate a Pipfile",
    )
    parser.add_argument(
        "--pyver",
        "-y",
        default="3.10",
        required=False,
        help="Python version to use in the generated Pipfile",
    )
    parser.add_argument(
        "--setup-path",
        "-s",
        default="./setup.py",
        required=False,
        help="Path of setup.py to process.",
    )
    parser.add_argument(
        "--diff",
        "-d",
        required=False,
        default=False,
        action="store_true",
        help="Print diffs, don't write file.",
    )
    parser.add_argument(
        "--print",
        "-p",
        required=False,
        action="store_true",
        help="Print new requirements, don't write file.",
    )
    return parser


def _read_reqs_file(file: str) -> list[Requirement]:
    """Return parsed requirements from requirements file."""
    reqs_file = Path(file)
    if reqs_file.is_file():
        reqs_text = reqs_file.read_text(encoding="utf-8")
        return list(
            parse_requirements(
                req.strip()
                for req in sorted(reqs_text.split("\n"))
                if req.strip() and not req.strip().startswith("#")
            )
        )
    return []


def _compare_reqs(new: list[Requirement], current: list[Requirement]) -> list[str]:
    """Return diff of two requirements lists."""
    new_list = [str(req) for req in new]
    curr_list = [str(req) for req in current]
    return list(
        difflib.context_diff(
            sorted(new_list),
            sorted(curr_list),
            fromfile="Updated",
            tofile="Current",
        )
    )


def _write_requirements(file_name: str, requirements: list[Requirement]) -> None:
    """Write requirements file."""
    Path(file_name).write_text(
        "\n".join(str(req) for req in sorted(requirements, key=str)), encoding="utf-8"
    )


def _get_pyver_from_setup(setup_cfg: str = "setup.cfg") -> str:
    """Read the Python version required from setup.cfg."""
    settings = read_configuration(setup_cfg)
    return str(settings["options"]["python_requires"])


def _create_pipfile(reqs: list[Requirement], reqs_dev: list[Requirement], py_ver: str) -> str:
    """Return the text of a Pipfile."""
    packages = [f'{req.name} = "{req.specifier}"' for req in reqs]
    dev_packages = [f'{req.name} = "{req.specifier}"' for req in reqs_dev]
    return _PIPFILE_TEMPLATE.format(
        packages="\n".join(packages),
        dev_packages="\n".join(dev_packages),
        py_ver=py_ver,
    )


def _get_extras_from_setup(
    extra: str = "all",
    include_base: bool = False,
) -> list[Requirement]:
    """
    Return list of extras from setup.py.

    Parameters
    ----------
    extra : str, optiona
        The name of the extra to return, by default "all"
    include_base : bool, optional
        If True include install_requires, by default False

    Returns
    -------
    List[Requirement]
        List of package requirements.

    Notes
    -----
    Duplicated from tools/toollib/import_analyzer.py

    """
    setup_mod = import_module("setup")
    extras = setup_mod.EXTRAS.get(extra)
    if include_base:
        base_install = setup_mod.INSTALL_REQUIRES
        extras.extend([req.strip() for req in base_install if not req.strip().startswith("#")])
    return list(parse_requirements(sorted(set(extras), key=str.casefold)))


# pylint: disable=invalid-name
if __name__ == "__main__":
    arg_parser = _add_script_args()
    args = arg_parser.parse_args()

    all_reqs = _get_extras_from_setup(
        extra="all",
        include_base=True,
    )
    dev_reqs = _read_reqs_file("requirements-dev.txt")

    if args.print:
        print("requirements-all.txt")
        print("--------------------")
        print("\n".join(str(req) for req in all_reqs))
        print(all_reqs)
        if args.pipfile:
            print("\nPipfile")
            print("--------")
            print(_create_pipfile(reqs=all_reqs, reqs_dev=dev_reqs, py_ver=args.pyver))
        sys.exit(0)

    existing_reqs = _read_reqs_file(args.req_all_path)
    diff_reqs = _compare_reqs(new=all_reqs, current=existing_reqs)

    if args.diff:
        # If we just wanted to check for a diff, finish here
        if diff_reqs:
            print(
                "Differences found for setup.py + requirements.txt",
                "vs. requirements-all.txt",
            )
            print("\n".join(diff.strip() for diff in diff_reqs))
            sys.exit(1)
        print("No differences for requirements-all.txt")
        sys.exit(0)

    # If the requirements lists differ
    if diff_reqs:
        _write_requirements(file_name=args.req_all_path, requirements=all_reqs)

    # We may need to create and write a Pipfile
    if args.pipfile and (diff_reqs or not Path("Pipfile").is_file()):
        pipfile_text = _create_pipfile(reqs=all_reqs, reqs_dev=dev_reqs, py_ver=args.pyver)
        Path("Pipfile").write_text(pipfile_text, encoding="utf-8")
    sys.exit(0)
