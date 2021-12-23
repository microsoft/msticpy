# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""VirusTotal Object browser."""
import pprint

import ipywidgets as widgets
import pandas as pd
import vt
from IPython.display import display

from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"

vt_df: pd.DataFrame = None  # pylint: disable=invalid-name
_NOT_FOUND = "Not found"

border_layout = widgets.Layout(
    **{
        "width": "90%",
        "border": "solid gray 1px",
        "margin": "1pt",
        "padding": "5pt",
    }
)


def _ts_to_pydate(data):
    """Replace Unix timestamps in VT data with Py/pandas Timestamp."""
    for date_col in (col for col in data.columns if col.endswith("_date")):
        data[date_col] = pd.to_datetime(data[date_col], unit="s", utc=True)
    return data


def get_summary(data=None):
    """Return summary of item."""
    data = data if data is not None else vt_df
    def_dict = {"sha256": "", "meaningful_name": "", "names": "", "magic": ""}
    if data is None:
        return def_dict
    if (
        "first_submission_date" not in data.columns
        or data.iloc[0]["first_submission_date"] == _NOT_FOUND
    ):
        def_dict["sha256"] = _NOT_FOUND
        return def_dict
    return data[["sha256", "meaningful_name", "names", "magic"]].iloc[0].to_dict()


def summary_html(title, summary):
    """Return HTML formatted summary."""
    return f"""
    <h3>{title}</h3>
    <table>
    <tr>
        <td>ID</td><td>{summary.get('sha256')}</td>
    </tr>
    <tr>
        <td>Names</td><td>{summary.get('names')}</td>
    </tr>
    <tr>
        <td>File Type</td><td>{summary.get('magic')}</td>
    </tr>
    </table>
    """


def lookup_file_id(btn):
    """Handle lookup button click."""
    del btn
    global vt_df  # pylint: disable=global-statement, invalid-name
    vt_df = vt.get_object(txt_file_id.value, vt_type="file").T
    html_header.value = summary_html(BASIC_TITLE, get_summary(vt_df))
    if (
        "first_submission_date" not in vt_df.columns
        or vt_df.iloc[0]["first_submission_date"] == _NOT_FOUND
    ):
        data_sel.options = []
        return
    vt_df = _ts_to_pydate(vt_df)
    data_sel.options = vt_df.columns


data_sel = widgets.Select(
    description="Attribute", layout=widgets.Layout(height="400px")
)

data_view = widgets.Textarea(
    description="Value", layout=widgets.Layout(height="400px", width="60%")
)


def _display_attribute(change):
    item = change.get("new")
    if item in vt_df.columns:
        data = vt_df.iloc[0][item]
        data_view.value = pprint.pformat(data)
    else:
        data_view.value = ""


data_sel.observe(_display_attribute, names="value")


txt_file_id = widgets.Text(
    description="Enter file ID (hash)",
    layout=widgets.Layout(width="70%"),
    style={"description_width": "150px"},
)

btn_lookup = widgets.Button(description="Lookup")
btn_lookup.on_click(lookup_file_id)

BASIC_TITLE = "VirusTotal File hash lookup"
html_header = widgets.HTML(
    summary_html(BASIC_TITLE, get_summary()), layout=border_layout
)

hb_file_lookup = widgets.HBox([txt_file_id, btn_lookup], layout=border_layout)
hb_vt_attribs = widgets.HBox([data_sel, data_view], layout=border_layout)
display(widgets.VBox([html_header, hb_file_lookup, hb_vt_attribs]))
