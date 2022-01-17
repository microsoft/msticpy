# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Requirements file writer from setup.py extras."""
import argparse
import difflib
from importlib import import_module
from pathlib import Path
import sys
from typing import List
import configparser
import re
import toml
import json

VERSION = "1.0.0"

__version__ = VERSION
__author__ = "Ian Hellen"


def _add_script_args():
    parser = argparse.ArgumentParser(
        description=f"Package imports analyer. v.{VERSION}"
    )
    parser.add_argument(
        "--out",
        "-o",
        default="./requirements-all.txt",
        required=False,
        help="Path of output file",
    )
    parser.add_argument(
        "--setup",
        "-s",
        default="./setup.py",
        required=False,
        help="Path of setup.py to process.",
    )
    parser.add_argument(
        "--extra",
        "-e",
        default="all",
        required=False,
        help="Name of extra to use.",
    )
    parser.add_argument(
        "--diff",
        "-d",
        required=False,
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


def _get_current_reqs_all(app_args):
    current_reqs = Path(app_args.out)
    if current_reqs.is_file():
        curr_text = current_reqs.read_text()
        return sorted(
            req
            for req in curr_text.split("\n")
            if req.strip() and not req.strip().startswith("#")
        )
    return []


def _compare_reqs(new, current):
    return list(
        difflib.context_diff(
            sorted(new, key=str.casefold),
            sorted(current, key=str.casefold),
            fromfile="Corrected",
            tofile="Current",
        )
    )


def _write_requirements(app_args, extras: List[str]):
    Path(app_args.out).write_text("\n".join(extras))


def _write_pipfile(extras: List[str]):
    """
    Converts requirements file to Pipfile

    Parameters
    ----------
    extras : List[str]
        List of packages in traditional requirements.txt format

    Notes
    -----
    Reads and writes Pipfile content, used to generate Pipfile.lock
    """

    # Regular expression to extract the required package version and extras
    package_pattern = re.compile('^(?P<name>[\w\d\-_]*)(\[(?P<extras>[\w\d,-_]*)\])' \
        '?(?P<requirements>(?P<condition>[\<\>=]{1,2})(?P<version>[\d\w\.]*))?')

    # Populate a dictionary wheres the key is the package name and value the pinned version
    pipfile_requirements = {}
    for package in extras:
        parsed_package_details = package_pattern.search(package)
        package_name = parsed_package_details['name']

        # Set the required version to latest (*) if no pinned version is found
        required_version = '*'
        if parsed_package_details['condition']:
            required_version = f'{parsed_package_details["condition"]}{parsed_package_details["version"]}'

        # Modify the requirement version if package extras are specified
        if parsed_package_details['extras']:
            package_extras = [extra for extra in parsed_package_details['extras'].split(',')]
            required_version = {'version': required_version, 'extras': package_extras}
        
        pipfile_requirements[package_name] = required_version

    # Save output to Pipfile (toml formatted)
    load_toml = toml.load('Pipfile')
    with open('Pipfile', 'w') as target_pipfile:
        load_toml['packages'] = pipfile_requirements
        target_pipfile.write(toml.dumps(load_toml))

def _get_extras_from_setup(
    package_root: str,
    setup_py: str = "setup.py",
    extra: str = "all",
    include_base: bool = False,
) -> List[str]:
    """
    Return list of extras from setup.py.

    Parameters
    ----------
    package_root : str
        The root folder of the package
    setup_py : str, optional
        The name of the setup file to process, by default "setup.py"
    extra : str, optiona
        The name of the extra to return, by default "all"
    include_base : bool, optional
        If True include install_requires, by default False

    Returns
    -------
    List[str]
        List of package requirements.

    Notes
    -----
    Duplicated from tools/toollib/import_analyzer.py

    """
    setup_py = str(Path(package_root) / setup_py)

    setup_txt = None
    with open(setup_py, "+r") as f_handle:
        setup_txt = f_handle.read()

    srch_txt = "setuptools.setup("
    repl_txt = [
        "def fake_setup(*args, **kwargs):",
        "    pass",
        "",
        "fake_setup(",
    ]
    setup_txt = setup_txt.replace(srch_txt, "\n".join(repl_txt))

    neut_setup_py = Path(package_root) / "neut_setup.py"
    try:
        with open(neut_setup_py, "+w") as f_handle:
            f_handle.writelines(setup_txt)

        setup_mod = import_module("neut_setup")
        extras = getattr(setup_mod, "EXTRAS").get(extra)
        if include_base:
            base_install = getattr(setup_mod, "INSTALL_REQUIRES")
            extras.extend(
                [req.strip() for req in base_install if not req.strip().startswith("#")]
            )
        return sorted(list(set(extras)), key=str.casefold)
    finally:
        neut_setup_py.unlink()


# pylint: disable=invalid-name
if __name__ == "__main__":
    arg_parser = _add_script_args()
    args = arg_parser.parse_args()

    extra_reqs = _get_extras_from_setup(
        package_root=str(Path(args.setup).parent),
        setup_py=str(Path(args.setup).name),
        extra="all",
        include_base=True,
    )

    if args.print:
        print("\n".join(extra_reqs))
        sys.exit(0)

    existing_reqs = _get_current_reqs_all(args)
    diff_reqs = _compare_reqs(new=extra_reqs, current=existing_reqs)
    if diff_reqs:
        if args.diff:
            print("\n".join(diff.strip() for diff in diff_reqs))
        else:
            _write_requirements(app_args=args, extras=extra_reqs)
            _write_pipfile(extra_reqs)
    sys.exit(1)
