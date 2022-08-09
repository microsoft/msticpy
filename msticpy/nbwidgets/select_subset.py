# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module for pre-defined widget layouts."""
from typing import Any, Dict, List, Union

import ipywidgets as widgets

from .._version import VERSION
from .core import IPyDisplayMixin

__version__ = VERSION
__author__ = "Ian Hellen"


# pylint: disable=too-many-instance-attributes
class SelectSubset(IPyDisplayMixin):
    """Class to select a subset from an input list."""

    def __init__(
        self,
        source_items: Union[Dict[str, str], List[Any]],
        default_selected: Union[Dict[str, str], List[Any]] = None,
        display_filter: bool = True,
        auto_display: bool = True,
    ):
        """
        Create instance of SelectSubset widget.

        Parameters
        ----------
        source_items : Union[Dict[str, str], List[Any]]
            List of source items - either a dictionary(label, value),
            a simple list or
            a list of (label, value) tuples.
        default_selected : Union[Dict[str, str], List[Any]]
            Populate the selected list with values - either
            a dictionary(label, value),
            a simple list or
            a list of (label, value) tuples.
        display_filter : bool, optional
            Whether to display item filter (the default is True)
        auto_display : bool, optional
            Whether to display on instantiation (the default is True)

        """
        if isinstance(source_items, dict):
            source_items = list(source_items.items())

        self.src_items = sorted(set(source_items))
        if isinstance(self.src_items[0], tuple):
            self._src_dict = {val: (label, val) for label, val in self.src_items}
        else:
            self._src_dict = {}

        w_layout = widgets.Layout(width="40%", height="200px")
        self._source_list = widgets.SelectMultiple(
            options=sorted(set(self.src_items)), layout=w_layout, description="Source: "
        )

        if isinstance(default_selected, dict):
            default_selected = list(default_selected.items())
        if default_selected:
            set_selected = set(default_selected)
            selected_items = sorted(set_selected.intersection(source_items))
        else:
            selected_items = []

        self._select_list = widgets.SelectMultiple(
            options=selected_items, layout=w_layout, description="Selected: "
        )

        self._display_filter = display_filter
        if display_filter:
            self._w_filter = widgets.Text(
                value="", description="Filter:", style={"description_width": "initial"}
            )

            # set up observer callbacks
            self._w_filter.observe(self._update_options, names="value")

        self._b_add_all = widgets.Button(description="Add All \u21fe")
        self._b_add = widgets.Button(description="Add \u21fe")
        self._b_del = widgets.Button(description="\u21fd Remove")
        self._b_del_all = widgets.Button(description="\u21fd Remove All")

        self._b_add.on_click(self._on_btn_add)
        self._b_del.on_click(self._on_btn_del)
        self._b_del_all.on_click(self._on_btn_del_all)
        self._b_add_all.on_click(self._on_btn_add_all)

        v_box = widgets.VBox(
            [self._b_add_all, self._b_add, self._b_del, self._b_del_all]
        )
        self.layout = widgets.HBox([self._source_list, v_box, self._select_list])
        if self._display_filter:
            self.layout = widgets.VBox([self._w_filter, self.layout])
        if auto_display:
            self.display()

    @property
    def value(self) -> List[Any]:
        """Return currently selected value or values."""
        return self.selected_values

    @property
    def selected_items(self) -> List[Any]:
        """
        Return a list of the selected items.

        If the input list is a list of tuples, this returns
        a list of the selected tuples.

        Returns
        -------
        List[Any]
            List of items in the selected list.

        """
        return list(self._select_list.options)

    @property
    def selected_values(self) -> List[Any]:
        """
        Return list of selected values.

        If the input list is a list of tuples, this returns
        a list of values of the items.

        Returns
        -------
        List[Any]
            List of selected item values.

        """
        if self._select_list.options and isinstance(
            self._select_list.options[0], tuple
        ):
            return [item[1] for item in self._select_list.options]
        return self.selected_items

    def _update_options(self, change):
        """Filter the alert list by substring."""
        if change is not None and "new" in change:
            self._source_list.options = sorted(
                {
                    i
                    for i in self.src_items
                    if str(change["new"]).lower() in str(i).lower()
                }
            )

    # pylint: disable=not-an-iterable
    def _on_btn_add(self, button):
        del button
        selected_set = set(self._select_list.options)
        for selected in self._source_list.value:
            if self._src_dict:
                selected_set.add(self._src_dict[selected])
            else:
                selected_set.add(selected)
        self._select_list.options = sorted(list(selected_set))

    def _on_btn_add_all(self, button):
        del button
        self._select_list.options = sorted(list(set(self._source_list.options)))

    def _on_btn_del(self, button):
        del button
        selected_set = set(self._select_list.options)
        # save the current index
        cur_index = max(self._select_list.index)
        if selected_set:
            for selected in self._select_list.value:
                if self._src_dict:
                    selected_set.remove(self._src_dict[selected])
                else:
                    selected_set.remove(selected)
            self._select_list.options = sorted(list(selected_set))
        if not self._select_list.options:
            return
        # try to set the index to the next item in the list
        if cur_index < len(self._select_list.options):
            next_item = cur_index or 0
            self._select_list.index = tuple([next_item])
        else:
            last_item = max(len(self._select_list.options) - 1, 0)
            self._select_list.index = tuple([last_item])

    # pylint: enable=not-an-iterable

    def _on_btn_del_all(self, button):
        del button
        self._select_list.options = []
