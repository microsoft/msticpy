# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Python file import analyzer."""
import argparse

from toollib import VERSION
from toollib.import_analyzer import analyze_imports

__version__ = VERSION
__author__ = "Ian Hellen"


def _add_script_args():
    parser = argparse.ArgumentParser(
        description=f"Package imports analyer. v.{VERSION}"
    )
    parser.add_argument(
        "--path",
        "-p",
        default=".",
        required=True,
        help="Path to folder containing package",
    )
    parser.add_argument(
        "--package", "-k", required=True, help="Name of package (subfolder of --path)"
    )
    parser.add_argument(
        "--req_file",
        "-r",
        default="requirements.txt",
        help="Name of requirements.txt file",
    )
    parser.add_argument(
        "--missing",
        action="store_true",
        default=True,
        help="Show missing imports for modules",
    )
    parser.add_argument(
        "--stdlib",
        action="store_true",
        default=False,
        help="Show standard library imports for modules",
    )
    parser.add_argument(
        "--reqs",
        action="store_true",
        default=False,
        help="Show imports listed in requirements.txt",
    )
    parser.add_argument(
        "--internal",
        action="store_true",
        default=False,
        help="Show missing imports for modules",
    )
    parser.add_argument(
        "--unknown",
        action="store_true",
        default=True,
        help="Show unknown imports for modules",
    )
    parser.add_argument(
        "--modules", action="store_true", default=False, help="Show imports by module."
    )
    return parser


def _print_single_module(mod_name, imps, p_args):
    print(mod_name)
    if p_args.internal:
        print("internal imports:", end=" ")
        print(imps.internal if imps.internal else "none")
    if p_args.stdlib:
        print("std lib imports:", end=" ")
        print(imps.standard if imps.standard else "none")
    if p_args.reqs:
        print(f"external imports listed in {p_args.req_file}:", end=" ")
        print(imps.setup_reqs if imps.setup_reqs else "none")
    if p_args.missing:
        print("missing imports (used but not in requirements):", end=" ")
        print(imps.missing_reqs if imps.missing_reqs else "none")
    if p_args.unknown:
        if imps.unknown:
            print("unknown imports:", end=" ")
            print(imps.unknown if imps.unknown else "none")


def _print_all_imports(mod_imports, p_args):
    if p_args.internal:
        print("internal imports:", end=" ")
        print(sorted({v for s in mod_imports.values() for v in s.internal}))
    if p_args.stdlib:
        print("std lib imports:", end=" ")
        print(sorted({v for s in mod_imports.values() for v in s.standard}))
    if p_args.reqs:
        print(f"external imports listed in {args.req_file}:", end=" ")
        print(sorted({v for s in mod_imports.values() for v in s.setup_reqs}))
    if p_args.missing:
        print("missing imports (used but not in requirements)", end=" ")
        print(sorted({v for s in mod_imports.values() for v in s.missing_reqs}))
    if p_args.unknown:
        print("unknown imports:", end=" ")
        print(sorted({v for s in mod_imports.values() for v in s.unknown}))


# pylint: disable=invalid-name
if __name__ == "__main__":
    arg_parser = _add_script_args()
    args = arg_parser.parse_args()
    if args.version:
        print(f"Version {VERSION}")

    package_imports = analyze_imports(args.path, args.package, req_file=args.req_file)
    if args.modules:
        for mod, imports in package_imports.items():
            _print_single_module(mod, imports, args)
    else:
        _print_all_imports(package_imports, args)
