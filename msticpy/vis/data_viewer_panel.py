# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Dataframe viewer using Panel Tabulator."""
from functools import partial
from pprint import pformat
from textwrap import wrap
from typing import Any, Callable, Dict, Iterable, List, Optional

import pandas as pd
from IPython.display import display

from ..common.exceptions import MsticpyMissingDependencyError

try:
    import panel as pn
except ImportError as err:
    raise MsticpyMissingDependencyError(
        "This component needs the panel package.",
        "Install the conda or pip 'panel' package.",
        packages="panel",
    ) from err


# pylint: disable=too-many-instance-attributes
class DataViewer:
    """Data viewer class."""

    _DEF_HEIGHT = 550
    _DEFAULT_HIDDEN_COLS = ["TenantId"]

    def __init__(self, data: pd.DataFrame, selected_cols: List[str] = None, **kwargs):
        """
        Initialize the DataViewer class.

        Parameters
        ----------
        data : pd.DataFrame
            The DataFrame to view
        selected_cols : List[str], optional
            Initial subset of columns to show, by default None (all cols)

        Other Parameters
        ----------------
        selectable : Union[bool, str], optional
            Whether rows should be selectable, by default "checkbox"
        show_index : bool, optional
            If True show the DataFrame index as a column, by default  True.
        show_tenant_id : bool, optional
            If True show the TenantId column, by default  True.
        max_col_width : int, optional
            Sets the maximum column width to display, by default 500
        detail_cols : List[str]
            List of columns for which details are displayed in collapsible
            field beneath each table row.
        kwargs :
            Other keyword arguments are passed to the panel
            Tabulator control.

        Notes
        -----
        Main attributes:
        value - original dataframe
        selected - indexes of currently selected rows
        selected_dataframe - currently selected rows
        current_view - current dataframe after filtering and sorting
        selection - indexes of currently selected rows

        See Also
        --------
        Tabulator - https://panel.holoviz.org/reference/widgets/Tabulator.html

        """
        if not pn.extension._loaded:
            pn.extension(
                "tabulator",
                sizing_mode="stretch_width",
                css_files=[pn.io.resources.CSS_URLS["font-awesome"]],
            )
        if data.empty:
            raise ValueError("No data available in 'data'")

        # Drop empty columns
        self.data = data.dropna(axis="columns", how="all")
        # Set up hidden columns
        self._hidden_columns = self._default_hidden_cols(selected_cols, **kwargs)
        if kwargs.pop("hide_tenantid", False) and "TenantId" in self._hidden_columns:
            self._hidden_columns.remove("TenantId")
        # Create the tabulator control
        self.data_table = pn.widgets.Tabulator(
            self.data,
            header_filters=kwargs.pop("header_filters", True),
            selectable=kwargs.pop("selectable", "checkbox"),
            show_index=kwargs.pop("show_index", False),
            configuration=self._create_configuration(),
            pagination="local",
            row_content=self._create_row_formatter(kwargs.pop("detail_cols", None)),
            embed_content=False,
            height=kwargs.pop("height", self._DEF_HEIGHT),
            **kwargs,
        )
        # Add the column chooser
        self.column_chooser = DataTableColumnChooser(
            data,
            selected_cols=selected_cols
            or list(set(data.columns) - set(self._hidden_columns)),
        )
        self.column_chooser.apply_button.on_click(self._update_columns)
        self.accordion = pn.layout.Accordion(
            ("Select columns", self.column_chooser.layout)
        )
        self._update_columns(btn=None)
        # set layout for the widget.
        self.layout = pn.layout.Column(self.data_table, self.accordion)

    def __getattr__(self, attrib: str) -> Any:
        """Return attribute from underlying Tabulator control."""
        if hasattr(self.data_table, attrib):
            return getattr(self.data_table, attrib)
        raise AttributeError(f"Attribute {attrib} not found.")

    def display(self):
        """Display the widget."""
        display(self.layout)

    def _ipython_display_(self):
        """Display in IPython."""
        self.display()

    def _update_columns(self, btn):
        """Update the displayed columns."""
        del btn
        hidden_cols = set(self.data.columns) - set(self.column_chooser.selected_columns)
        if self._hidden_columns:
            hidden_cols = hidden_cols | set(self._hidden_columns)
        self.data_table.hidden_columns = list(hidden_cols)
        self.accordion.active = []

    def _create_row_formatter(
        self, detail_columns: Optional[List[str]] = None
    ) -> Optional[Callable]:
        """Build formatter function for row-details."""
        if not detail_columns:
            return None
        row_view_cols = set(detail_columns) & set(self.data.columns)
        return partial(_display_column_details, columns=row_view_cols)

    def _create_configuration(self, **kwargs) -> Dict[str, Any]:
        """Create Tabulator configuration dict to pass to JS Tabulator."""
        return {
            "columnDefaults": {"maxWidth": kwargs.pop("max_col_width", 500)},
            "clipboardCopyRowRange": "selected",
            "clipboardCopyConfig": {
                "columnHeaders": True,
                "columnGroups": False,
                "rowGroups": False,
                "columnCalcs": False,
                "dataTree": False,
                "formatCells": False,
            },
        }

    def _default_hidden_cols(self, selected_cols, **kwargs) -> List[str]:
        """Return list of of columns hidden by default."""
        return [
            hidden_col
            for hidden_col in self._DEFAULT_HIDDEN_COLS
            if (
                hidden_col in self.data.columns
                and selected_cols
                and hidden_col not in selected_cols
            )
        ] + (kwargs.pop("hidden_cols", None) or [])


class DataTableColumnChooser:
    """DataTableColumnChooser class."""

    def __init__(self, data, selected_cols=None):
        """Initialize the DataTableColumnChooser class."""
        self.data = data
        self._all_col_names = list(data.columns)
        self._initial_cols = selected_cols or self._all_col_names
        self._col_select = pn.widgets.MultiSelect(
            name="Select columns to display",
            value=self._initial_cols,
            options=self._all_col_names,
            size=15,
        )
        self.apply_button = pn.widgets.Button(name="Apply columns")
        self.layout = pn.layout.Column(self._col_select, self.apply_button)

    @property
    def selected_columns(self) -> List[str]:
        """Return a list of Bokeh column definitions for the DataFrame."""
        return self._col_select.value

    @property
    def dataframe_columns(self):
        """Return the selected set of DataFrame columns."""
        return self.data[self._reorder_cols(self.selected_columns)]

    def _reorder_cols(self, columns: List[str]) -> List[str]:
        """Return column list in original order."""
        # order the columns as originally specified (or as the DF)
        col_init = [col for col in self._initial_cols if col in columns]
        # If any new columns, add them to the end of the list
        col_init.extend(list(set(columns) - set(col_init)))
        return col_init

    def display(self):
        """Display in IPython."""
        display(self.layout)

    def _ipython_display_(self):
        """Display in IPython."""
        self.display()


# RowFunction display
_ROW_FORMAT_FUNCS = {
    (dict, list): lambda value, width: f"<pre>{pformat(value, width=width)}</pre>",
    str: lambda value, width: "<br>".join(wrap(value, width=width)),
}
# expand
_FORMATTERS = {
    val_type: func
    for val_tuple, func in _ROW_FORMAT_FUNCS.items()
    for val_type in (val_tuple if isinstance(val_tuple, tuple) else (val_tuple,))
}


def _format_value(value: Any, width=150) -> str:
    """Apply format function to column cell."""
    return _FORMATTERS.get(type(value), _FORMATTERS[str])(value, width)


TD_ALIGN = """
<style>
td { text-align:left; vertical-align: top }
</style>
"""


def _display_column_details(
    row: pd.Series, columns: Iterable[str], width: int = 120
) -> pn.pane.HTML:
    """Return HTML pane with formatted row columns."""
    display_text = "".join(
        f"<tr><td><b>{column}</b></td><td>{_format_value(row[column], width=width)}</td></tr>"
        for column in columns
    )
    return pn.pane.HTML(
        f"{TD_ALIGN}<table>{display_text}</table>", sizing_mode="stretch_width"
    )
