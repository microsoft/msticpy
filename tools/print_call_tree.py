# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Python file import analyzer."""
import argparse

from toollib import VERSION
from toollib.module_tree import analyze_calls, print_call_tree

__version__ = VERSION
__author__ = "Ian Hellen"


def _add_script_args():
    parser = argparse.ArgumentParser(description="Module static call tree analyer.")
    parser.add_argument(
        "--module", "-m", default=".", required=True, help="Path to module to analyze."
    )
    parser.add_argument(
        "--all",
        "-a",
        action="store_true",
        default=False,
        help="Show all functions in module. Default shows only top-level functions",
    )
    parser.add_argument(
        "--external",
        "-e",
        action="store_true",
        default=False,
        help="Show all calls including to external functions.",
    )
    return parser


# pylint: disable=invalid-name
if __name__ == "__main__":
    arg_parser = _add_script_args()
    args = arg_parser.parse_args()

    mod_call_graph = analyze_calls(args.module, all_calls=args.external)
    p_level = "all" if args.all else "top"
    print_call_tree(call_graph=mod_call_graph, level=p_level)
