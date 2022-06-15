# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Mordor dataset browser."""
from pprint import pformat
from typing import Any, Dict, Iterable, Optional

import ipywidgets as widgets
import pandas as pd
from IPython.display import HTML, display

from .._version import VERSION
from ..common.exceptions import MsticpyException
from ..common.pkg_config import settings
from ..data.drivers.mordor_driver import (
    MordorDriver,
    MordorEntry,
    download_mdr_file,
    search_mdr_data,
)

__version__ = VERSION
__author__ = "Ian Hellen"


class MordorBrowser:
    """Mordor browser widget."""

    def __init__(self, save_folder: Optional[str] = None, use_cached: bool = True):
        """
        Initialize MordorBrowser control.

        Parameters
        ----------
        save_folder : str, optional
            Folder to save downloaded files, by default reads the value
            from settings or defaults to "."
        use_cached : bool, optional
            If true, downloaded files are not deleted after
            download and are used as a local cache, by default True

        """
        mdr_settings = settings.get("DataProviders", {}).get("Mordor", {})
        self._save_folder = save_folder or mdr_settings.get("save_folder", ".")

        self._use_cached = use_cached
        self.mordor_driver = MordorDriver()
        self.mordor_driver.connect()
        self.mdr_metadata = self.mordor_driver.mordor_data
        self._df_disp = None

        self.layouts = self._create_layouts()
        self.w_style = {
            "description_width": "150px",
            "font_family": "arial, sans-serif",
        }

        self.widgets: Dict[str, Any] = {}
        self._init_field_ctls()
        self._init_select_dataset()
        self._init_filter_ctrls()

        wgt_title = widgets.HTML(
            value="<h2>Mordor dataset browser</h2>", style={"fontsize": "x-large"}
        )

        browse_ctrls = widgets.VBox(
            [wgt_title, self.widgets["ds_select"], self.widgets["filter_grp"]],
            layout=self.layouts["box_layout"],
        )
        fields_ctrls = widgets.VBox(
            list(self.fields.values()), layout=self.layouts["box_layout"]
        )

        self.datasets: Dict[str, pd.DataFrame] = {}
        self.current_dataset: pd.DataFrame = None
        display(widgets.VBox([browse_ctrls, fields_ctrls]))
        self._df_disp = display(HTML("<p>"), display_id=True)

    @property
    def fields(self):
        """Return set of fields widget controls."""
        return self.widgets["fields"]

    @property
    def selected_dset(self):
        """Return the ID of the selected data set."""
        return self.widgets["ds_select"].value

    def _clear_df_display(self):
        if self._df_disp:
            self._df_disp.update(HTML("<p>"))

    @staticmethod
    def _create_layouts():
        """Set up the widget layouts."""
        return {
            "layout_norm": widgets.Layout(width="70%"),
            "layout_high": widgets.Layout(width="70%", height="200px"),
            "box_layout": widgets.Layout(
                width="80%", border="1px solid", margin="5px", padding="10px"
            ),
            "mitre_select_layout": widgets.Layout(width="40%", height="100px"),
        }

    def _init_filter_ctrls(self):
        """Initialize the filter controls."""
        # text_filter
        self.widgets["filter_text"] = widgets.Text(
            description="Filter", layout=self.layouts["layout_norm"], style=self.w_style
        )
        self.widgets["filter_text"].continuous_update = False
        self.widgets["filter_text"].observe(self._update_select_list, "value")
        self.widgets["filter_help"] = widgets.Label(
            value=" comma ORs values, '+' ANDs values"
        )

        # Mitre filters
        self.widgets["sel_techniques"] = widgets.SelectMultiple(
            description="Mitre Techniques",
            options=self._get_mitre_filter_options(
                self.mordor_driver.mdr_idx_tech, self.mordor_driver.mitre_techniques
            ),
            layout=self.layouts["mitre_select_layout"],
            style=self.w_style,
        )

        self.widgets["sel_tactics"] = widgets.SelectMultiple(
            description="Mitre Tactics",
            options=self._get_mitre_filter_options(
                self.mordor_driver.mdr_idx_tact, self.mordor_driver.mitre_tactics
            ),
            layout=self.layouts["mitre_select_layout"],
            style=self.w_style,
        )
        self._reset_filters()
        self.widgets["sel_techniques"].observe(self._update_select_list, names="value")
        self.widgets["sel_tactics"].observe(self._update_select_list, names="value")

        self.widgets["filter_reset"] = widgets.Button(description="Reset filter")
        self.widgets["filter_reset"].on_click(self._reset_filters)
        wgt_filter_grp = widgets.VBox(
            [
                widgets.HBox(
                    [self.widgets["filter_text"], self.widgets["filter_help"]]
                ),
                widgets.HBox(
                    [
                        self.widgets["sel_techniques"],
                        self.widgets["sel_tactics"],
                        self.widgets["filter_reset"],
                    ]
                ),
            ]
        )
        self.widgets["filter_grp"] = widgets.Accordion(children=[wgt_filter_grp])
        self.widgets["filter_grp"].set_title(0, "Filters")
        self.widgets["filter_grp"].selected_index = None

    def _init_select_dataset(self):
        """Initialize the select dataset control."""
        ds_select = widgets.Select(
            description="Data sets",
            options=self._get_md_select_options(),
            layout=self.layouts["layout_norm"],
            style=self.w_style,
        )
        ds_select.observe(self._select_ds_item, names="value")
        self._select_ds_item({"new": next(iter(self.mdr_metadata.keys()))})
        self.widgets["ds_select"] = ds_select

    def _init_field_ctls(self):
        """Initialize the data field controls."""
        fields = {}
        dl_button = None
        for field, field_attrs in MORDOR_FIELDS.items():
            if field == "file_paths":
                dl_button = widgets.Button(description="Download")
                fields[field] = widgets.HBox(
                    [
                        widgets.Select(
                            description=field,
                            layout=self.layouts["layout_norm"],
                            style=self.w_style,
                        ),
                        dl_button,
                    ]
                )
            elif field == "simulation":
                fields[field] = field_attrs["widget"](
                    description=field,
                    layout=self.layouts["layout_high"],
                    style=self.w_style,
                )
            else:
                fields[field] = field_attrs["widget"](
                    description=field,
                    layout=self.layouts["layout_norm"],
                    style=self.w_style,
                )
        self.widgets["fields"] = fields

        if dl_button is not None:
            self.widgets["wgt_dl_button"] = dl_button
            dl_button.on_click(self._download_file)

    def _clear_fields(self):
        """Clear data fields (when nothing is selected)."""
        for field in MORDOR_FIELDS:
            if field == "file_paths":
                self.fields[field].children[0].options = []
            else:
                self.fields[field].value = ""
        self._clear_df_display()

    def _select_ds_item(self, change):  # noqa: MC0001
        """Handle change of dataset selection."""
        item_id = change.get("new")
        mdr_item = self.mdr_metadata.get(item_id)
        if not mdr_item:
            self._clear_fields()
            return

        for field, field_attrs in MORDOR_FIELDS.items():
            if mdr_item and field_attrs["type"] != "cust":
                value = getattr(mdr_item, field)
            else:
                value = None

            if field_attrs["type"] == "text":
                self.fields[field].value = str(value) if value else ""
            elif field_attrs["type"] == "list":
                self.fields[field].value = (
                    ", ".join([str(item) for item in value]) if value else ""
                )
            elif field_attrs["type"] == "raw":
                self.fields[field].value = pformat(value) if value else ""
            elif field == "attacks":
                field_data = mdr_item.get_attacks()
                self.fields[field].value = _format_attacks(field_data)
            elif field == "file_paths":
                file_paths = mdr_item.get_file_paths()
                self.fields[field].children[0].options = _format_files(file_paths)
            elif field == "notebooks":
                self.fields[field].value = _format_notebooks(mdr_item.notebooks)
            elif field == "query name":
                self.fields[field].value = _format_queries(mdr_item)

        self._clear_df_display()

    def _update_select_list(self, event=None):
        """Update the dataset selection list based on filters."""
        del event
        filtered_tech = set()
        for t_id in self.widgets["sel_techniques"].value:
            filtered_tech.update(self.mordor_driver.mdr_idx_tech[t_id])

        filtered_tact = set()
        for t_id in self.widgets["sel_tactics"].value:
            filtered_tact.update(self.mordor_driver.mdr_idx_tact[t_id])

        md_items_filtered = filtered_tech & filtered_tact
        md_ids = search_mdr_data(
            self.mdr_metadata, self.widgets["filter_text"].value, md_items_filtered
        )
        self.widgets["ds_select"].options = self._get_md_select_options(md_ids)

    def _reset_filters(self, event=None):
        """Reset filter controls to default."""
        del event
        self.widgets["sel_techniques"].value = [
            opt[1] for opt in self.widgets["sel_techniques"].options
        ]
        self.widgets["sel_tactics"].value = [
            opt[1] for opt in self.widgets["sel_tactics"].options
        ]
        self.widgets["filter_text"].value = ""
        self._update_select_list()

    def _get_md_select_options(self, subset: Iterable[str] = None):
        """Return current set of datasets for select control."""
        return [
            (f"{mdr.id} {mdr.title} ({mdr.platform})", mdr.id)
            for mdr in self.mdr_metadata.values()
            if subset is None or mdr.id in subset
        ]

    def _download_file(self, event):
        """Handle download file event."""
        del event
        selection = self.fields["file_paths"].children[0].value
        if selection not in self.datasets:
            result_df = None
            try:
                result_df = download_mdr_file(
                    selection,
                    use_cached=self._use_cached,
                    save_folder=self._save_folder,
                )
            except MsticpyException:
                pass
            if not isinstance(result_df, pd.DataFrame) or result_df.empty:
                result_df = HTML("Could not extract data from this file")
            self.datasets[selection] = result_df
        self.current_dataset = self.datasets[selection]
        self._df_disp.update(self.datasets[selection])

    @staticmethod
    def _get_mitre_filter_options(mordor_index: Dict[str, MordorEntry], mitre_data):
        return [
            (f"{m_id} - {mitre_data.loc[m_id].Name}", m_id)
            for m_id in mordor_index
            if m_id in mitre_data.index
        ]


_FMT_STYLE = "border: 1px solid #AAAAAA; padding: 5px"


def _format_attacks(attacks):
    """Format the Mitre Attack data for display."""
    html_text = []
    for attack in attacks:
        sub_tech = f" (sub: {attack.sub_technique}) " if attack.sub_technique else ""
        html_tech = [
            f"<p>Mitre Technique <b>{attack.technique}</b> ",
            sub_tech,
            f": <a href='{attack.technique_uri}' target='_blank'>",
            f"<u>{attack.technique_name}</u></a></p>",
            "Mitre Tactics: ",
        ]

        html_tactics = [
            f"<b>{tactic[0]}</b>: <a href='{tactic[3]}' "
            + f"target='_blank'><u>{tactic[1]}</u></a>"
            for tactic in attack.tactics_full
        ]

        html_tactics = ", ".join(html_tactics)
        html_text.append("".join([*html_tech, html_tactics]))
    content = "<br>".join(html_text)
    return f"<div style='{_FMT_STYLE}'>{content}</span>"


def _format_files(files):
    """Format the Mordor files data for display."""
    return [
        (f"({file['file_type']}) {file['file_path'].split('/')[-1]}", file["file_path"])
        for file in files
    ]


def _format_notebooks(notebooks):
    """Format the Mordor notebooks data for display."""
    if not notebooks:
        return f"<div style='{_FMT_STYLE}'>none</span>"
    nbks = ((nbk["project"], nbk["name"], nbk["link"]) for nbk in notebooks)
    content = "".join(
        [
            f"<p>{nbk[0]} - <a href='{nbk[2]}' target='_blank'><u>{nbk[1]}</u></a></p>"
            for nbk in nbks
        ]
    )
    return f"<div style='{_FMT_STYLE}'>{content}</span>"


def _format_queries(mdr_entry):
    return "\n".join(
        [
            f"{file_path['qry_path']} ({mdr_entry.title}, {mdr_entry.id})"
            for file_path in mdr_entry.get_file_paths()
        ]
    )


MORDOR_FIELDS = {
    "title": {"type": "text", "widget": widgets.Text},
    "id": {"type": "text", "widget": widgets.Text},
    "type": {"type": "text", "widget": widgets.Text},
    "contributors": {"type": "list", "widget": widgets.Text},
    "creation_date": {"type": "text", "widget": widgets.Text},
    "modification_date": {"type": "text", "widget": widgets.Text},
    "platform": {"type": "text", "widget": widgets.Text},
    "description": {"type": "text", "widget": widgets.Textarea},
    "tags": {"type": "list", "widget": widgets.Text},
    "file_paths": {
        "type": "cust",
        "widget": widgets.Select,
        "method": "get_file_paths",
        "formatter": _format_files,
    },
    "attacks": {
        "type": "cust",
        "widget": widgets.HTML,
        "method": "get_attacks",
        "formatter": _format_attacks,
    },
    "notebooks": {"type": "cust", "widget": widgets.HTML},
    "simulation": {"type": "raw", "widget": widgets.Textarea},
    "references": {"type": "raw", "widget": widgets.Textarea},
    "query name": {"type": "cust", "widget": widgets.Textarea},
}
