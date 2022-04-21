# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""VirusTotal Object browser."""
import pprint
from typing import Dict, Optional

import ipywidgets as widgets
import pandas as pd

from .._version import VERSION
from ..context.vtlookupv3.vtlookupv3 import VTLookupV3, timestamps_to_utcdate
from ..nbwidgets import IPyDisplayMixin

__version__ = VERSION
__author__ = "Ian Hellen"


_NOT_FOUND = "Not found"

_BORDER_LAYOUT = widgets.Layout(
    **{
        "width": "90%",
        "border": "solid gray 1px",
        "margin": "1pt",
        "padding": "5pt",
    }
)


class VTObjectBrowser(IPyDisplayMixin):
    """VirusTotal object attributes browser."""

    _BASIC_TITLE = "VirusTotal File hash lookup"

    def __init__(self, file_id: Optional[str] = None):
        """
        Initialize the VT Browser.

        Parameters
        ----------
        file_id : Optional[str], optional
            File ID (Hash) of file to be retrieved and displayed, by default None

        """
        self._vt = VTLookupV3()
        self._current_data: pd.DataFrame = pd.DataFrame()

        self.data_sel = widgets.Select(
            description="Attribute", layout=widgets.Layout(height="400px")
        )
        self.data_view = widgets.Textarea(
            description="Value", layout=widgets.Layout(height="400px", width="60%")
        )
        self.data_sel.observe(self._display_attribute, names="value")

        self.txt_file_id = widgets.Text(
            description="Enter file ID (hash)",
            layout=widgets.Layout(width="70%"),
            style={"description_width": "150px"},
            value=file_id or "",
        )

        self.btn_lookup = widgets.Button(description="Lookup")
        self.btn_lookup.on_click(self._lookup_file_id)

        self.html_header = widgets.HTML(
            _summary_html(self._BASIC_TITLE, _extract_summary()), layout=_BORDER_LAYOUT
        )

        self.hb_file_lookup = widgets.HBox(
            [self.txt_file_id, self.btn_lookup], layout=_BORDER_LAYOUT
        )
        self.hb_vt_attribs = widgets.HBox(
            [self.data_sel, self.data_view], layout=_BORDER_LAYOUT
        )
        self.layout = widgets.VBox(
            [self.html_header, self.hb_file_lookup, self.hb_vt_attribs]
        )
        if file_id:
            self.btn_lookup.click()

    def _display_attribute(self, change):
        """Display selected attribute in TextArea."""
        item = change.get("new")
        if item in self._current_data.columns:
            data = self._current_data.iloc[0][item]
            self.data_view.value = pprint.pformat(data)
        else:
            self.data_view.value = ""

    def _lookup_file_id(self, btn):
        """Handle lookup button click."""
        del btn

        self._current_data = self._vt.get_object(self.txt_file_id.value, vt_type="file")
        self.html_header.value = _summary_html(
            self._BASIC_TITLE, _extract_summary(self._current_data)
        )
        if (
            "first_submission_date" not in self._current_data.columns
            or self._current_data.iloc[0]["first_submission_date"] == _NOT_FOUND
        ):
            self.data_sel.options = []
            return
        self._current_data = timestamps_to_utcdate(self._current_data)
        self.data_sel.options = self._current_data.columns


def _extract_summary(data: Optional[pd.DataFrame] = None) -> Dict[str, str]:
    """Return summary of item."""
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


def _summary_html(title: str, summary: Dict[str, str]) -> str:
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
