# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Txt2df core code."""
import argparse
import io
from typing import Dict, Union

import pandas as pd
from pandas.errors import ParserError
from pkg_resources import parse_version

from ..._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"

_PD_VER = parse_version(pd.__version__)


def _add_parser_args():
    parser = argparse.ArgumentParser(
        description="Cell magic to convert cell text to pandas DataFrame",
        prog="%%txt2df",
    )
    parser.add_argument(
        "--sep",
        "-s",
        default=",",
        required=False,
        help="Column separator/delimiter to use.",
    )
    parser.add_argument(
        "--name",
        "-n",
        default=None,
        required=False,
        help="If specified, the DataFrame will be assigned to the named variable.",
    )
    parser.add_argument(
        "--headers",
        "-e",
        action="store_true",
        default=False,
        help="If supplied, the first line is treated as the header row.",
    )
    parser.add_argument(
        "--keepna",
        "-k",
        action="store_true",
        default=False,
        help=(
            "Don't drop columns that are all NA (the default is to drop"
            + " them, which is useful for data with trailing delimiters.)"
        ),
    )
    return parser


def run_txt2df(line, cell, local_ns) -> pd.DataFrame:
    """Convert cell text to pandas DataFrame."""
    arg_parser = _add_parser_args()
    try:
        line_args = line.split(" ") if line else []
        args = arg_parser.parse_args(line_args)
    except argparse.ArgumentError as err:
        raise AttributeError(
            "Invalid argument supplied.", "Use --help to see valid arguments."
        ) from err

    if not cell:
        return pd.DataFrame()
    cell_text = io.StringIO(cell)
    warn_args: Dict[str, Union[str, bool]]
    if _PD_VER < parse_version("1.3.0"):  # type: ignore
        warn_args = {"warn_bad_lines": True}
    else:
        warn_args = {"on_bad_lines": "warn"}
    try:
        parsed_df = pd.read_csv(  # type: ignore
            cell_text,
            header=0 if args.headers else None,
            sep=args.sep,
            skipinitialspace=True,
            skip_blank_lines=True,
            engine="python",
            **warn_args,
        )
    except ParserError:
        # try again without headers
        cell_text = io.StringIO(cell)
        parsed_df = pd.read_csv(  # type: ignore
            cell_text,
            sep=args.sep,
            skipinitialspace=True,
            skip_blank_lines=True,
            engine="python",
            **warn_args,
        )
        print(
            "One or more rows had more columns than specified in first row.",
            "Ignoring header row.",
        )
    if not args.keepna:
        parsed_df = parsed_df.dropna(axis=1, how="all")
    if local_ns is not None and args.name:
        local_ns[args.name] = parsed_df
    return parsed_df
