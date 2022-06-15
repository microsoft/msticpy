# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Compare two requirements files."""
import argparse
import re

from packaging.specifiers import SpecifierSet
from packaging.version import parse


def _add_script_args(description):
    parser = argparse.ArgumentParser(
        description=description, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.add_argument("source", help="First requirements file to compare.")
    parser.add_argument("target", help="Second requirements file to compare.")
    return parser


def _parse_line(line):
    req_regex = r"(?P<pkg>[^~=<>,]+)(?P<op>[~=<>,]*)(?P<ver>.*)"

    match = re.search(req_regex, line)
    if match:
        return match.groups()
    return None, None, None


def _parse_reqs(file):
    with open(file, "r", encoding="utf-8") as file_reqs:
        req_lines = file_reqs.readlines()
    p_lines = [_parse_line(line) for line in req_lines]
    return {pkg: (op, ver) for pkg, op, ver in p_lines}


# pylint: disable=invalid-name
if __name__ == "__main__":
    arg_parser = _add_script_args(description=__doc__)
    args = arg_parser.parse_args()

    src_dict = _parse_reqs(args.source)
    tgt_dict = _parse_reqs(args.target)

    src_only = {pkg for pkg in src_dict if pkg not in tgt_dict}
    both = set(src_dict) - src_only

    print(f"{len(src_only)} packages missing from target.")
    print("\n".join(sorted(src_only)))
    not_compat = []
    compat = []
    for pkg in sorted(both):
        v2 = parse(tgt_dict[pkg][1])
        spec1 = SpecifierSet(src_dict[pkg][0] + src_dict[pkg][1])
        if v2 in spec1:
            compat.append(f"Compatible: {pkg}, {src_dict[pkg]}, {tgt_dict[pkg]}")
        else:
            not_compat.append(
                f"Not compatible: {pkg}, {src_dict[pkg]}, {tgt_dict[pkg]}"
            )
    print(
        f"Common packages: {len(compat)} compatible, {len(not_compat)} not compatible."
    )
    print("\n".join(compat))
    print("\n".join(not_compat))
