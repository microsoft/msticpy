# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Dataframe viewer."""
from collections import namedtuple
from typing import Dict, List, Union

import ipywidgets as widgets
import pandas as pd
from bokeh.io import output_notebook, push_notebook, show
from bokeh.models import (
    BooleanFilter,
    CDSView,
    ColumnDataSource,
    DataTable,
    DateFormatter,
    TableColumn,
)
from IPython.display import display

from .. import nbwidgets
from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


FilterExpr = namedtuple("FilterExpr", "column, inv, operator, expr")


# pylint: disable=too-many-instance-attributes
class DataViewer:
    """Data viewer class."""

    _DEF_HEIGHT = 550

    def __init__(
        self, data: pd.DataFrame, selected_cols: List[str] = None, debug=False
    ):
        """
        Initialize the DataViewer class.

        Parameters
        ----------
        data : pd.DataFrame
            The DataFrame to view
        selected_cols : List[str], optional
            Initial subset of columns to show, by default None (all cols)
        debug : bool
            Output additional debugging info to std out.

        """
        if data.empty:
            raise ValueError("No data available in 'data'")

        output_notebook(hide_banner=True)
        # Drop empty columns
        data = data.dropna(axis="columns", how="all")
        self.cds = ColumnDataSource(data)
        self._columns = _get_cols_from_df(data)
        self._dt_columns = list(self._columns.values())
        self.data = data
        self._debug = debug

        self.nb_handle = None
        self.data_table = DataTable(
            source=self.cds,
            columns=self._dt_columns,
            view=CDSView(source=self.cds),
            height=self._calc_df_height(data),
            width_policy="max",
            auto_edit=True,
            editable=True,
            reorderable=True,
        )

        self.column_chooser = DataTableColumnChooser(data, selected_cols=selected_cols)
        self.data_filter = DataTableFilter(data)
        if selected_cols is not None:
            self._update_columns(btn=None)

        self.column_chooser.apply_button.on_click(self._update_columns)
        self.data_filter.apply_button.on_click(self._apply_filter)

        self.accordion = widgets.Accordion(
            children=[self.column_chooser.layout, self.data_filter.layout]
        )
        self.accordion.set_title(0, "Choose columns")
        self.accordion.set_title(1, "Filter data")
        self.accordion.selected_index = None

        self.layout = self.accordion

    @property
    def filtered_data(self) -> pd.DataFrame:
        """Return filtered dataframe."""
        return self.data_filter.filtered_dataframe[self.column_chooser.selected_columns]

    @property
    def filters(self) -> Dict[str, FilterExpr]:
        """Return current filters as a dict."""
        return self.data_filter.filters

    def import_filters(self, filters: Dict[str, FilterExpr]):
        """
        Import filter set replacing current filters.

        Parameters
        ----------
        filters : Dict[str, FilterExpr]
            dict of filter name, FilterExpr
            FilterExpr is a tuple of:
            column [str], inv [bool], operator [str], expr [str]

        """
        self.data_filter.import_filters(filters)
        self._apply_filter(btn=None)

    def _calc_df_height(self, data):
        df_height = 20 + (len(data) * 20)
        return min(df_height, self._DEF_HEIGHT)

    def show(self):
        """Display the data table control."""
        if self._debug:
            print("_update_data_table")
        self.nb_handle = show(self.data_table, notebook_handle=True)

    def _update_data_table(self):
        if self._debug:
            print("_update_data_table")
            print(self.data_filter.filters)
            print(len(self.filtered_data))
            print(self.filtered_data.iloc[:2])
        if self.nb_handle:
            push_notebook(handle=self.nb_handle)

    def display(self):
        """Display the widget."""
        self.show()
        display(self.layout)

    def _ipython_display_(self):
        """Display in IPython."""
        self.display()

    def _update_columns(self, btn):
        del btn
        self.data_table.columns = self.column_chooser.datatable_columns
        self._update_data_table()

    def _apply_filter(self, btn):
        del btn
        if self._debug:
            print("_apply_filter")
        self.data_table.view = CDSView(
            source=self.cds, filters=[BooleanFilter(self.data_filter.bool_filters)]
        )
        self.data_table.height = self._calc_df_height(
            self.data_filter.filtered_dataframe
        )
        self._update_data_table()


class DataTableColumnChooser:
    """DataTableColumnChooser class."""

    def __init__(self, data, selected_cols=None):
        """Initialize the DataTableColumnChooser class."""
        self.data = data
        self._all_col_names = list(data.columns)
        self._initial_cols = selected_cols or self._all_col_names
        self._col_select = nbwidgets.SelectSubset(
            default_selected=self._initial_cols,
            source_items=self._all_col_names,
            auto_display=False,
        )

        self.apply_button = widgets.Button(description="Apply columns")
        self.layout = widgets.VBox([self._col_select.layout, self.apply_button])

    @property
    def datatable_columns(self):
        """Return a list of Bokeh column definitions for the DataFrame."""
        return list(_get_cols_from_df(self.dataframe_columns).values())

    @property
    def dataframe_columns(self):
        """Return the selected set of DataFrame columns."""
        return self.data[self._reorder_cols(self.selected_columns)]

    def _reorder_cols(self, columns):
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

    @property
    def selected_columns(self):
        """Return the selected columns."""
        return self._reorder_cols(self._col_select.selected_items)


def _layout(width, height=None, desc_width=None, **kwargs):
    """Layout creation for widgets."""
    wgt_dict = {}
    lo_dict = {"width": width}
    if height:
        lo_dict["height"] = height
    border = kwargs.pop("border", None)
    if border:
        lo_dict.update(
            {
                "border": "solid gray 1px",
                "margin": "1pt",
                "padding": "5pt",
            }
        )
    wgt_dict["layout"] = widgets.Layout(**lo_dict)
    style_dict = {}
    if desc_width:
        style_dict["description_width"] = desc_width
    if kwargs:
        style_dict.update(kwargs)
    if style_dict:
        wgt_dict["style"] = style_dict
    return wgt_dict


class DataTableFilter:
    """Data filtering class."""

    _OPERATORS = {
        "string": ["==", "contains", "matches", "in", "between", "query"],
        "other": ["==", ">", "<", ">=", "<=", "in", "between", "query"],
    }

    def __init__(self, data: pd.DataFrame):
        """Initialize the DataTableFilter class."""
        self.all_cols = list(data.columns)
        self.data = data

        # Widgets
        self._add_button = widgets.Button(description="Add filter")
        self._del_button = widgets.Button(description="Delete filter")
        self._upd_button = widgets.Button(description="Update filter")
        self._clear_button = widgets.Button(description="Clear all filters")
        self.apply_button = widgets.Button(description="Apply filter")
        self._col_select = widgets.Dropdown(options=self.all_cols, **(_layout("200px")))
        self._oper_sel = widgets.Dropdown(
            options=self._col_operators(self.current_col), **(_layout("100px"))
        )
        self._not_cb = widgets.Checkbox(
            description="not", value=False, **(_layout("60px", desc_width="initial"))
        )
        self._filter_value = widgets.Textarea(
            description="Filter value", **(_layout("400px"))
        )
        self._curr_filters = widgets.Select(description="Filters", **(_layout("500px")))
        self._oper_label = widgets.Label(" in ")

        self.filters: Dict[str, FilterExpr] = {}

        self._curr_filters.observe(self._select_filter, names="value")
        self._col_select.observe(self._update_operators, names="value")

        self._add_button.on_click(self._add_filter)
        self._upd_button.on_click(self._update_filter)
        self._del_button.on_click(self._del_filter)
        self._clear_button.on_click(self._clear_filters)
        filt_help_lbl = widgets.Label(
            value="Enter multiple values separated by commas. Strings do not need quotes."
        )
        top_row = widgets.VBox(
            [
                filt_help_lbl,
                widgets.HBox(
                    [
                        self._col_select,
                        self._not_cb,
                        self._oper_sel,
                        self._filter_value,
                    ]
                ),
            ]
        )
        mid_row = widgets.HBox(
            [
                self._add_button,
                self._upd_button,
            ]
        )
        curr_filt_lbl = widgets.Label(value="Current filters")
        bottom_row = widgets.VBox(
            [
                curr_filt_lbl,
                widgets.HBox(
                    [
                        self._curr_filters,
                        widgets.VBox([self._del_button, self._clear_button]),
                    ]
                ),
            ],
            **_layout(width="80%", border=True),
        )

        self.layout = widgets.VBox([top_row, mid_row, bottom_row, self.apply_button])

    def display(self):
        """Display in IPython."""
        display(self.layout)

    def _ipython_display_(self):
        """Display in IPython."""
        self.display()

    def import_filters(self, filters: Dict[str, FilterExpr]):
        """
        Replace the current filters with `filters`.

        Parameters
        ----------
        filters : Dict[str, FilterExpr]
            dict of filter name, FilterExpr
            FilterExpr is a tuple of:
            column [str], inv [bool], operator [str], expr [str]

        """
        self.filters = {
            f_name: FilterExpr(*f_expr) for f_name, f_expr in filters.items()
        }
        self._curr_filters.options = list(filters.keys())

    @property
    def bool_filters(self):
        """Return current set of boolean filters."""
        df_filt = None
        for filt in self.filters.values():
            new_filt = self._make_filter(
                filt.column, filt.operator, filt.expr, filt.inv
            )
            new_filt = new_filt.values if isinstance(new_filt, pd.Series) else new_filt
            df_filt = new_filt if df_filt is None else df_filt & new_filt
        return df_filt if df_filt is not None else self.data.index.isin(self.data.index)

    @property
    def filtered_dataframe(self) -> pd.DataFrame:
        """Return current filtered DataFrame."""
        return self.data[self.bool_filters]

    def _select_filter(self, change):
        filter_name = change["new"]
        if not filter_name:
            return
        (
            self._col_select.value,
            self._not_cb.value,
            self._oper_sel.value,
            self._filter_value.value,
        ) = self.filters[filter_name]

    def _update_operators(self, change):
        del change
        self._oper_sel.options = self._col_operators(self._col_select.value)

    def _add_filter(self, btn):
        del btn
        if self._curr_filter_name in self.filters:
            return
        self.filters[self._curr_filter_name] = FilterExpr(
            column=self._col_select.value,
            inv=self._not_cb.value,
            operator=self._oper_sel.value,
            expr=self._filter_value.value,
        )
        curr_opts = list(self._curr_filters.options)
        curr_opts.append(self._curr_filter_name)
        self._curr_filters.options = curr_opts

    def _update_filter(self, btn):
        selected_filter = self._curr_filters.value
        self._add_filter(btn)
        if selected_filter in self.filters:
            del self.filters[selected_filter]
            self._curr_filters.options = list(self.filters.keys())

    def _del_filter(self, btn):
        del btn
        selected_filter = self._curr_filters.value
        if selected_filter in self.filters:
            del self.filters[selected_filter]
            self._curr_filters.options = list(self.filters.keys())

    @property
    def _curr_filter_name(self):
        not_str = " not " if self._not_cb.value else ""
        return (
            f"{self._col_select.value} {not_str}{self._oper_sel.value}"
            f" '{self._filter_value.value}'"
        )

    def _clear_filters(self, btn):
        del btn
        self.filters.clear()
        self._curr_filters.options = []

    @property
    def current_col(self):
        """Return the currently selected column."""
        return self._col_select.value

    def _col_operators(self, col):
        if pd.api.types.is_string_dtype(self.data[col]):
            return self._OPERATORS["string"]
        return self._OPERATORS["other"]

    def _make_filter(self, col, operator, expr, not_true):
        if not_true:
            return ~self._create_filter(col, operator, expr)
        return self._create_filter(col, operator, expr)

    # pylint: disable=too-many-return-statements
    def _create_filter(self, col: str, operator: str, expr: str) -> pd.Series:
        if operator == "query":
            return pd.Series(self.data.index.isin(self.data.query(expr).index))
        if operator in ("in", "between"):
            return self._filter_in_or_between(col, operator, expr)

        test_expr = self._conv_expr_type(col, expr)
        if operator == "==":
            return self.data[col] == test_expr
        if operator == "contains":
            return self.data[col].str.contains(test_expr)
        if operator == "matches":
            return self.data[col].str.match(test_expr)
        if operator == ">":
            return self.data[col] > test_expr
        if operator == ">=":
            return self.data[col] >= test_expr
        if operator == "<":
            return self.data[col] < test_expr
        if operator == "<=":
            return self.data[col] >= test_expr
        raise TypeError(
            f"Unsupported operator for operator {operator} and column {col}"
        )

    def _filter_in_or_between(self, col: str, operator: str, expr: str) -> pd.Series:
        """Return filter for `in` and `between` operators."""
        test_expr: List[Union[str, int, float]]

        if pd.api.types.is_string_dtype(self.data[col]):
            test_expr = [item.strip("\"' ") for item in expr.split(",")]
        elif pd.api.types.is_numeric_dtype(self.data[col]):
            test_expr = [
                int(item) if "." not in item else float(item)
                for item in expr.split(",")
            ]
        elif pd.api.types.is_datetime64_any_dtype(self.data[col]):
            test_expr = [pd.Timestamp(item.strip()) for item in expr.split(",")]
        else:
            raise TypeError(
                f"Unsupported column type {self.data[col].dtype}",
                f"for operator {operator} and column {col}",
            )
        if operator == "in":
            return self.data[col].isin(test_expr)
        if len(test_expr) != 2:
            raise ValueError(
                f"Must have two operands for expression {expr}",
                f"for operator {operator} and column {col}",
            )
        return self.data[col].between(test_expr[0], test_expr[1], inclusive="both")

    def _conv_expr_type(self, col: str, expr: str):
        """Convert string expression to required type."""
        test_expr: Union[str, int, float]
        if pd.api.types.is_numeric_dtype(self.data[col]):
            test_expr = int(expr) if "." not in expr else float(expr)
        elif pd.api.types.is_datetime64_any_dtype(self.data[col]):
            test_expr = pd.Timestamp(expr.strip())
        elif pd.api.types.is_string_dtype(self.data[col]):
            test_expr = expr.strip("\"' ")
        else:
            raise TypeError(
                f"Unsupported column type {self.data[col].dtype}",
                f"for column {col}",
            )
        return test_expr


def _get_col_width(data, col):
    if data[col].iloc[:10].dropna().empty:
        return 8
    if data[col].dtype == "O":
        return int(data[col].iloc[:10].str.len().mean())
    if pd.api.types.is_datetime64_any_dtype(data[col]):
        return 50
    return 8


def _get_cols_from_df(data):
    """Get list of TableColumn columns from DataFrame."""
    # save the existing column order
    col_order = data.columns
    dt_cols = data.select_dtypes("datetime").columns
    columns = {
        col: TableColumn(field=col, title=col, width=_get_col_width(data, col))
        for col in data.columns
        if col not in dt_cols
    }

    date_fmt = "%F %T"
    dt_columns = {
        col: TableColumn(
            field=col,
            title=col,
            formatter=DateFormatter(format=date_fmt),
            width=_get_col_width(data, col),
        )
        for col in dt_cols
    }
    columns.update(dt_columns)
    return {col: columns[col] for col in col_order}
