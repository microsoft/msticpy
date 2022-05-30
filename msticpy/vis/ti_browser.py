# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Threat Intel Results Browser."""
import pprint
from typing import List, Union

import pandas as pd
from IPython.display import HTML

from .._version import VERSION
from ..nbwidgets import SelectItem

__version__ = VERSION
__author__ = "Ian Hellen"


def browse_results(
    data: pd.DataFrame, severities: Union[List[str], str, None] = None, **kwargs
) -> SelectItem:
    """
    Return TI Results list browser.

    Parameters
    ----------
    data : pd.DataFrame
        TI Results data from TIProviders
    severities : Union[List[str], str, None], optional
        A list of the severity classes to show or the string 'all'.
        By default these are ['warning', 'high'].

    Other Parameters
    ----------------
    kwargs :
        passed to SelectItem constuctor.

    Returns
    -------
    SelectItem
        SelectItem browser for TI Data.

    """
    if "height" not in kwargs:
        kwargs["height"] = "300px"

    if not isinstance(data, pd.DataFrame) or data.empty:
        raise ValueError("'data' parameter is empty or is not a dataframe.")

    opts = get_ti_select_options(ti_data=data, severities=severities)
    disp_func = ti_details_display(ti_data=data)
    if not opts and not data.empty:
        print("No results at TI Severities 'warning' or 'high'")
        print("Displaying only 'information' severity items.")
        opts = get_ti_select_options(ti_data=data, severities="all")

    return SelectItem(item_dict=opts, action=disp_func, **kwargs)


def get_ti_select_options(
    ti_data: pd.DataFrame, severities: Union[List[str], str, None] = None
):
    """Get SelectItem options for TI data."""
    ti_agg_df = _create_ti_agg_list(ti_data, severities)
    return dict(
        ti_agg_df.reset_index()
        .apply(
            lambda x: (
                f"{x.Ioc:<40} type: {x.IocType:<10}  (sev: {x.Severity})"
                + f"  providers: {x.Providers}",
                (x.Ioc, x.Providers),
            ),
            axis=1,
        )
        .values
    )


def _create_ti_agg_list(
    ti_data: pd.DataFrame, severities: Union[List[str], str, None] = None
):
    """Aggregate ti results on IoC for multiple providers."""
    if not severities:
        severities = ["warning", "high"]
    if severities == "all":
        severities = ["information", "warning", "high"]
    ti_data["Details"] = ti_data.apply(lambda x: _label_col_dict(x, "Details"), axis=1)

    return (
        ti_data[ti_data["Severity"].isin(severities)]
        .groupby(["Ioc", "IocType", "Severity"])
        .agg(
            Providers=pd.NamedAgg(
                column="Provider", aggfunc=lambda x: x.unique().tolist()
            ),
            Details=pd.NamedAgg(column="Details", aggfunc=lambda x: x.tolist()),
            Responses=pd.NamedAgg(column="RawResult", aggfunc=lambda x: x.tolist()),
            References=pd.NamedAgg(
                column="Reference", aggfunc=lambda x: x.unique().tolist()
            ),
        )
        .reset_index()
    )


def _label_col_dict(row: pd.Series, column: str):
    """Add label from the Provider column to the details."""
    if not isinstance(row[column], dict):
        return row[column]
    return (
        {row.Provider: row[column]} if row.Provider not in row[column] else row[column]
    )


def ti_details_display(ti_data):
    """Return TI Details display function."""

    def get_ti_details(ioc_prov):
        """Display TI records from individual TI entry."""
        ioc, provs = ioc_prov
        h2_style = "border: 1px solid;background-color: DarkGray; padding: 6px"
        h3_style = "background-color: SteelBlue; padding: 6px"
        results = [f"<h2 style='{h2_style}'>{ioc}</h2>"]
        for prov in provs:
            ioc_match = ti_data[
                (ti_data["Ioc"] == ioc) & (ti_data["Provider"] == prov)
            ].iloc[0]
            results.extend(
                (
                    f"<h3 style='{h3_style}'>Type: '{ioc_match.IocType}', Provider: {prov}, "
                    + f"severity: {ioc_match.Severity}</h3>",
                    "<h4>Details</h4>",
                )
            )

            results.append(_ti_detail_table(ioc_match.Details))
            results.extend(
                (
                    f"<h4>Reference: </h4><a href='{ioc_match.Reference}'>"
                    + f"{ioc_match.Reference}</a><br>",
                    "<hr>",
                    "<h4>Raw Results</h4>",
                )
            )

            results.append(raw_results(ioc_match.RawResult))
        return HTML("".join(results))

    return get_ti_details


def raw_results(raw_result: str) -> str:
    """Create pre-formatted details for raw results."""
    fmt_details = (
        pprint.pformat(raw_result).replace("\n", "<br>").replace(" ", "&nbsp;")
    )
    return f"""
        <details>
        <summary> <u>Raw results from provider...</u></summary>
        <pre  style="font-size:11px">{fmt_details}</pre>
        </details>
        """


_TI_TABLE_STYLE = """
<style>
    .tb_ti_res {border-collapse: collapse; width: 60%; border: 1px solid #ddd !important;}
    .cell_ti {border: 1px solid #ddd !important;
        text-align: left !important; padding: 5px !important; height: 12px}
    .cell_ti_first {border: 1px solid #ddd !important; width: 25%;
        text-align: left !important; padding: 5px !important; height: 12px}
</style>
"""


def _ti_detail_table(detail_dict: dict) -> str:
    """Return table of ti details."""
    return "".join(
        [
            _TI_TABLE_STYLE,
            "<table class='tb_ti_res'>",
            *_dict_to_html(detail_dict),
            "</table>",
        ]
    )


def _dict_to_html(detail_dict):
    html_txt = []
    if not isinstance(detail_dict, dict):
        return detail_dict
    for key, val in detail_dict.items():
        html_txt.append(f"<tr class='cell_ti'><td class='cell_ti_first'>{key}</td>")
        if not isinstance(val, dict):
            html_txt.append(f"<td class='cell_ti'>{val}</td></tr>")
        else:
            html_txt.extend(_dict_to_html(val))
            html_txt.append("</tr>")
    return html_txt
