# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Compare two requirements files."""
import argparse
import re
from packaging.version import parse
from packaging.specifiers import SpecifierSet


def _add_script_args(description):
    parser = argparse.ArgumentParser(
        description=description, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.add_argument("file1", help="First requirements file to compare.")
    parser.add_argument("file2", help="Second requirements file to compare.")
    return parser


def _parse_line(line):
    req_regex = r"(?P<pkg>[^=<>]+)(?P<op>[=<>]*)(?P<ver>.*)"

    match = re.search(req_regex, line)
    if match:
        return match.groups()
    return None, None, None


def _parse_reqs(file):
    with open(file, "r") as file_reqs:
        req_lines = file_reqs.readlines()
    p_lines = [_parse_line(line) for line in req_lines]
    return {pkg: (op, ver) for pkg, op, ver in p_lines}


# pylint: disable=invalid-name
if __name__ == "__main__":
    arg_parser = _add_script_args(description=__doc__)
    args = arg_parser.parse_args()

    f1_dict = _parse_reqs(args.file1)
    f2_dict = _parse_reqs(args.file2)

    f1_only = {pkg for pkg in f1_dict if pkg not in f2_dict}
    f2_only = {pkg for pkg in f2_dict if pkg not in f1_dict}

    both = set(f1_dict) - f1_only

    not_compat = []
    compat = []
    for pkg in both:
        v2 = parse(f2_dict[pkg][1])
        spec1 = SpecifierSet(f1_dict[pkg][0] + f1_dict[pkg][1])
        if v2 in spec1:
            compat.append(f"Compatible: {pkg}, {f1_dict[pkg]}, {f2_dict[pkg]}")
        else:
            not_compat.append(f"Not compatible: {pkg}, {f1_dict[pkg]}, {f2_dict[pkg]}")
    print("\n".join(compat))
    print("\n".join(not_compat))
