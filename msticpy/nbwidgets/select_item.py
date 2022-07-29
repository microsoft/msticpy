# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module for pre-defined widget layouts."""
import random
from typing import Any, Callable, Dict, List, Optional, Tuple, Union

import ipywidgets as widgets
from deprecated.sphinx import deprecated
from IPython.display import HTML, display
from ipywidgets import Layout

from .._version import VERSION
from .core import IPyDisplayMixin

__version__ = VERSION
__author__ = "Ian Hellen"


class SelectItem(IPyDisplayMixin):
    """
    Selection list from list or dict.

    Attributes
    ----------
    item_action : Callable[..., Optional[Tuple]]
        Action to call for each selection.

    """

    # pylint: disable=too-many-arguments, too-few-public-methods
    def __init__(
        self,
        description: str = "Select an item",
        options: Union[List[str], Dict[str, Any]] = None,
        action: Callable[..., Optional[Tuple]] = None,
        value: str = "",
        **kwargs,
    ):
        """
        Select an item from a list or dict.

        Parameters
        ----------
        description : str, optional
            The widget label to display.
            (the default is 'Select an item')
        options : Union[List[str], Dict[str, Any]]
            Either:
            A `list` of items to select from (the default is None)
            A `dict` of items to select from. When using `item_dict`
            the values are displayed as the selectable items and key
            corresponding to the selected value is set as the `value`
            property.
        action : Callable[..., Optional[Tuple[...]]], optional
            function to call when item selected (passed a single
            parameter - the value of the currently selected item)
            (the default is None).
            If the function returns one or a tuple of displayable objects
            these will be displayed.
        value : str, optional
            A default value to pre-populate the filter with.

        Other Parameters
        ----------------
        item_list : List[str], optional
            A `list` of items to select from (the default is None)
        item_dict : Dict[str, str], optional
            A `dict` of items to select from. When using `item_dict`
            the keys are displayed as the selectable items and value
            corresponding to the selected key is set as the `value`
            property.
            (the default is None)
        auto_display : bool, optional
            Whether to display on instantiation (the default is False)
        height : str, optional
            Selection list height (the default is '100px')
        width : str, optional
            Selection list width (the default is '50%')
        display_filter : bool, optional
            Whether to display item filter (the default is True)

        """
        self.options = options or kwargs.pop("item_list", [])
        self.options = self.options or kwargs.pop("item_dict", {})

        if not self.options:
            raise ValueError("No options supplied for SelectItem.")
        auto_display = kwargs.pop("auto_display", False)
        height: str = kwargs.pop("height", "100px")
        width: str = kwargs.pop("width", "50%")
        display_filter: bool = kwargs.pop("display_filter", True)

        self.def_value = value
        if self.def_value not in self.options:
            if isinstance(self.options, list):
                self.def_value = next(iter(self.options))
            else:
                self.def_value = next(iter(self.options.values()))

        self._wgt_select = widgets.Select(
            value=self.def_value,
            options=self._get_filtered_options(),
            description=description,
            layout=Layout(width=width, height=height),
            style={"description_width": "initial"},
        )
        self._w_display_details = widgets.Checkbox(
            value=True,
            description="Display details",
        )
        self._display_filter = display_filter
        if display_filter:
            self._w_filter = widgets.Text(
                value=self.label,
                description="Filter:",
                style={"description_width": "initial"},
            )

            # set up observer callbacks
            self._w_filter.observe(self._filter_options, names="value")
        self._wgt_select.observe(self._select_item, names="value")

        if action:
            self._w_display_details.observe(self._run_action, names="value")
        self.item_action = action

        # setup to use updatable display objects
        rand_id = random.randint(0, 999999)  # nosec
        self._output_id = f"{self.__class__.__name__}_{rand_id}"
        self._disp_elems: List[Any] = []

        if auto_display:
            self.display()

    @property
    def layout(self):
        """Return underlying widget collection."""
        wgt_list = []
        if self._display_filter:
            wgt_list.append(self._w_filter)
        wgt_list.append(self._wgt_select)
        if self.item_action:
            wgt_list.append(self._w_display_details)
        return widgets.VBox(wgt_list)

    @property
    def value(self):
        """Return the currently selected item."""
        return self._wgt_select.value

    @value.setter
    def value(self, value):
        """Set current selected value of widget."""
        if value in self.options:
            self._wgt_select.label = value
        if isinstance(self.options, dict) and value in self.options.values():
            self._wgt_select.value = value

    @property
    def label(self):
        """Return current display item."""
        return self._wgt_select.label

    def display(self):
        """Display the interactive widget."""
        super().display()
        display(HTML("<hr>"))
        self._show_top_item()

    def _select_item(self, selection):
        """Run action if available."""
        del selection
        if self.item_action is not None:
            self._run_action()

    def _filter_options(self, change):
        """Filter the alert list by substring."""
        if change is None or "new" not in change:
            return
        self._wgt_select.options = self._get_filtered_options(change["new"])

    def _get_filtered_options(
        self, substring: str = ""
    ) -> List[Union[str, Tuple[str, str]]]:
        """Return optionally filtered list of option tuples."""
        if self.options is None:
            return []
        if isinstance(self.options, list):
            return [val for val in self.options if substring.casefold() in val.lower()]
        return [
            (lab, val)
            for lab, val in self.options.items()
            if substring.casefold() in lab.lower()
        ]

    def _run_action(self, change=None):
        """Run any action function and display details, if any."""
        del change
        output_objs = None
        if self._w_display_details.value:
            output_objs = self.item_action(self.value)
        if output_objs is None:
            self._clear_display()
            return
        if not isinstance(output_objs, (tuple, list)):
            output_objs = [output_objs]
        display_objs = dict(enumerate(self._disp_elems))
        for idx, out_obj in enumerate(output_objs):
            if idx not in display_objs:
                self._disp_elems.append(
                    display(out_obj, display_id=f"{self._output_id}_{idx}")
                )
            else:
                self._disp_elems[idx].update(out_obj)

    def _clear_display(self):
        """Clear any current details."""
        if not self._disp_elems:
            return
        for disp_obj in self._disp_elems:
            disp_obj.update(HTML(""))

    def _show_top_item(self):
        """Run action on the first item by default."""
        if self.item_action is not None and self.value is not None:
            self._run_action()


@deprecated(
    reason="Superceded by SelectItem. Will be removed in v2.0.0.", version="0.5.2"
)
class SelectString(SelectItem):
    """Selection list from list or dict."""

    # pylint: disable=too-many-arguments, too-few-public-methods
    def __init__(
        self,
        description: str = "Select an item",
        item_list: List[str] = None,
        action: Callable[..., None] = None,
        item_dict: Dict[str, str] = None,
        auto_display: bool = False,
        height: str = "100px",
        width: str = "50%",
        display_filter: bool = True,
    ):
        """
        Select an item from a list or dict.

        Parameters
        ----------
        description : str, optional
            The widget label to display.
            (the default is 'Select an item')
        item_list : List[str], optional
            A `list` of items to select from (the default is None)
        item_dict : Dict[str, str], optional
            A `dict` of items to select from. When using `item_dict`
            the keys are displayed as the selectable items and value
            corresponding to the selected key is set as the `value`
            property.
            (the default is None)
        action : Callable[..., None], optional
            function to call when item selected (passed a single
            parameter - the value of the currently selected item)
            (the default is None)
        auto_display : bool, optional
            Whether to display on instantiation (the default is False)
        height : str, optional
            Selection list height (the default is '100px')
        width : str, optional
            Selection list width (the default is '50%')
        display_filter : bool, optional
            Whether to display item filter (the default is True)

        """
        self._w_output = widgets.Output(layout={"border": "1px solid black"})

        super().__init__(
            description=description,
            item_list=item_list,
            item_dict=item_dict,
            action=action,
            auto_display=auto_display,
            height=height,
            width=width,
            display_filter=display_filter,
        )

    def _run_action(self, change=None):
        del change
        self._w_output.clear_output()
        with self._w_output:
            self.item_action(self.value)

    def display(self):
        """Display the interactive widget."""
        self._show_top_item()
        wgt_list = []
        if self._display_filter:
            wgt_list.append(self._w_filter)
        wgt_list.append(self._wgt_select)
        if self.item_action:
            wgt_list.append(self._w_output)
        display(widgets.VBox(wgt_list))
