# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Generate documentation of current queries."""
import argparse
from pathlib import Path

import pandas as pd
from tabulate import tabulate  # type: ignore
import tqdm

from msticpy.data import QueryProvider

__author__ = "Ian Hellen"


PROVIDERS = {
    "MSSentinel": "Microsoft Sentinel",
    "M365D": "Microsoft 365 Defender",
    "Kusto": "Kusto/Azure Data Explorer",
    "SecurityGraph": "Microsoft Graph",
    "Splunk": "Splunk",
    "ResourceGraph": "Azure Resource Graph",
    "Sumologic": "Sumologic",
    "LocalData": "Local Data",
}

_DEF_DOCNAME = "source/data_acquisition/DataQueries.rst"


def get_query_list():
    """Return the query list as a DataFrame."""
    prov_list = QueryProvider.list_data_environments()

    print("Generating documentation for for the following providers")
    print(", ".join(list(PROVIDERS)))
    print("Skipping the following providers")
    print(", ".join(list(set(prov_list) - set(PROVIDERS))))
    env_providers = {prov: QueryProvider(prov) for prov in tqdm.tqdm(PROVIDERS)}

    query_series = []
    for env, env_queries in env_providers.items():
        query_names = env_queries.list_queries()
        for query_name in query_names:
            q_group, q_name = query_name.split(".")
            qry = env_queries.query_store.get_query(q_group + "." + q_name)
            if "table" in qry.default_params:
                q_table = qry.default_params["table"].get("default", "na").split()[0]
            elif "table" in qry.required_params:
                q_table = qry.required_params["table"].get("default", "na").split()[0]
            else:
                q_table = "-"
            q_dict = {
                "Environment": env,
                "QueryGroup": q_group,
                "Query": q_name,
                "Description": qry.description,
                "Req-Params": ", ".join(
                    sorted(
                        [
                            f"{param} ({p_data.get('type')})"
                            for param, p_data in qry.required_params.items()
                        ]
                    )
                ),
                # "OtherParams": ", ".join([f"{param}" for param in qry.default_params]),
                "Table": q_table,
            }
            query_series.append(pd.Series(q_dict))
    print()
    return pd.DataFrame(query_series).sort_values(
        ["Environment", "QueryGroup", "Query"]
    )


def generate_document(query_df):  # sourcery skip: identity-comprehension
    """Generate query list document."""
    doc_lines = [
        "Data Queries Reference",
        "=" * len("Data Queries Reference"),
        "",
        "",
    ]

    # This line fails if re-written as dict(query_df.groupby("Environment"))
    # pylint: disable=unnecessary-comprehension
    group_dict = {name: group for name, group in query_df.groupby("Environment")}
    for name, friendly_name in PROVIDERS.items():
        if name not in group_dict:
            continue
        grp = group_dict[name]
        doc_lines.append(f"Queries for {friendly_name}")
        doc_lines.append("-" * len(f"Queries for {friendly_name}"))
        doc_lines.append(f"\nData Environment identifier: {name}\n")
        tbl_txt = tabulate(
            grp.drop(columns="Environment"),
            headers="keys",
            showindex="never",
            tablefmt="rst",
        )
        tbl_txt = [line.strip() for line in tbl_txt.split("\n")]
        doc_lines.extend(tbl_txt)
        doc_lines.append("\n\n")
    return "\n".join(doc_lines)


def _add_script_args():
    """Create argparse arguments."""
    parser = argparse.ArgumentParser(description="Queries list document generator.")
    parser.add_argument(
        "cmd",
        default="print",
        type=str,
        choices=["print", "doc"],
    )
    parser.add_argument(
        "--file",
        "-f",
        default=_DEF_DOCNAME,
        help="Path to RST document file to write.",
    )
    return parser


# pylint: disable=invalid-name
if __name__ == "__main__":
    arg_parser = _add_script_args()
    args = arg_parser.parse_args()

    doc = generate_document(get_query_list())
    if args.cmd == "print":
        print(doc)
    else:
        Path(args.file).write_text(doc, encoding="utf-8")
