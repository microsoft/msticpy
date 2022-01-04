# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Component Edit base and mixin classes."""
from abc import ABC, abstractmethod
from time import sleep
from typing import Any, Dict, List, Optional, Tuple, Union

import ipywidgets as widgets
from IPython.display import display

from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


class CompEditDisplayMixin:
    """Mixin class with common display methods."""

    def _ipython_display_(self):
        layout = getattr(self, "layout", None)
        if layout:
            display(layout)

    @staticmethod
    def border_layout(width="95%"):
        """Return border widget layout."""
        return widgets.Layout(
            **{
                "width": width,
                "border": "solid gray 1px",
                "margin": "1pt",
                "padding": "5pt",
            }
        )

    @staticmethod
    def no_border_layout(width="95%"):
        """Return no-border widget layout."""
        return widgets.Layout(**{"width": width, "margin": "1pt", "padding": "5pt"})


# pylint: disable=too-few-public-methods
class CompEditUtilsMixin:
    """Mixin class with common display methods."""

    @staticmethod
    def _get_settings_path(settings, path):
        path_elems = path.split(".")
        curr_node = settings
        for elem in path_elems:
            curr_node = curr_node.get(elem)
            if curr_node is None:
                break
        return curr_node


class CompEditStatusMixin:
    """Mixin class with with status label."""

    testing = False
    status = widgets.Label(layout=widgets.Layout(width="99%"))

    def set_status(self, status, timeout: float = 3.0):
        """Set the status text."""
        self.status.value = status
        if timeout == 0 or self.testing:
            return
        sleep(timeout)
        self.clear_status()

    def clear_status(self):
        """Clear the status text."""
        self.status.value = ""


class CompEditHelp:
    """Class to add help control."""

    _DEFAULT_URI = {
        "MSTICPy Config": (
            "https://msticpy.readthedocs.io/en/latest/"
            + "getting_started/msticpyconfig.html"
        )
    }
    _HELP_STYLE = "color: blue; text-decoration: underline;"

    def __init__(self, help_text: str = "", help_uri: Dict[str, str] = None):
        """
        Create help sub-component.

        Parameters
        ----------
        help_text : str, optional
            The help string (HTML), by default ""
        help_uri : Dict[str, str], optional
            Dict of named URIs {disp_txt: URI}, by default None

        """
        self.html_help = widgets.HTML()
        self.accdn_help = widgets.Accordion(children=[self.html_help])
        self.accdn_help.set_title(0, "Help")
        self.accdn_help.selected_index = None
        self.set_help(help_text, help_uri)

    def set_help(self, help_text: str = "", help_uri: Dict[str, str] = None):
        """Set the help string (HTML) and URIs."""
        if not help_uri:
            help_uri = self._DEFAULT_URI
        help_uris = "<br>".join(
            f"<a href='{uri}' target='_blank' style='{self._HELP_STYLE}'>Online: {title}</a>"
            for title, uri in help_uri.items()
        )
        help_text = "<br>".join(
            [
                help_text,
                help_uris,
            ]
        )
        self.html_help.value = help_text

    @property
    def layout(self):
        """Return the parent widget."""
        return self.accdn_help


class CompEditFrame(CompEditDisplayMixin, CompEditUtilsMixin, CompEditStatusMixin):
    """Edit frame class for components."""

    def __init__(self, description: str = None):
        """Initialize the class. Set a label with `description` as content."""
        self.inner_frame = widgets.HBox(layout=self.border_layout("99%"))
        self.description = widgets.Label(value=description or "")
        # self.description.style = {"visibility": "visible" if description else "hidden"}
        self.help = CompEditHelp()
        # self.status = widgets.Label(layout=widgets.Layout(width="99%"))
        self.layout = widgets.VBox(
            [self.description, self.inner_frame, self.help.layout, self.status]
        )


class CompEditItemButtons:
    """Component class to add default buttons."""

    def __init__(self):
        """Initialize the class."""
        btn_layout = widgets.Layout(width="100px")
        self.btn_add = widgets.Button(
            description="Add",
            tooltip="Add provider/item in drop-down to your settings.",
            layout=btn_layout,
        )
        self.btn_del = widgets.Button(
            description="Delete",
            tooltip="Remove selected provider/item from your settings.",
            layout=btn_layout,
        )
        self.btn_save = widgets.Button(
            description="Update",
            tooltip="Confirms updates to the settings changes",
            layout=btn_layout,
        )
        self.layout = widgets.HBox([self.btn_add, self.btn_del, self.btn_save])


class CompEditItems(CompEditFrame):
    """Base class for item list and edit controls."""

    def __init__(self, description: str):
        """Initialize the class. Set a label with `description` as content."""
        super().__init__(description=description)
        self.select_item = widgets.Select(
            layout=widgets.Layout(height="200px", width="99%")
        )
        self.edit_frame = widgets.VBox(layout=self.border_layout("99%"))
        self.edit_buttons = CompEditItemButtons()
        self.items_frame = widgets.VBox(
            [
                self.select_item,
                widgets.HBox([self.edit_buttons.btn_add, self.edit_buttons.btn_del]),
            ],
            layout=self.no_border_layout(width="40%"),
        )
        self.inner_frame.children = [
            self.items_frame,
            widgets.VBox(
                [self.edit_frame, self.edit_buttons.btn_save],
                layout=widgets.Layout(width="99%"),
            ),
        ]


class CompEditSimple(CompEditFrame):
    """Base class for simple component with only edit controls."""

    def __init__(self, description: str):
        """Initialize the class. Set a label with `description` as content."""
        super().__init__(description=description)
        self.edit_frame = widgets.VBox(layout=self.border_layout("100%"))
        self.btn_save = widgets.Button(
            description="Update", tooltip="Confirms updates to the settings changes"
        )
        self.container = widgets.VBox(
            [self.edit_frame, self.btn_save], layout=self.no_border_layout("100%")
        )
        self.inner_frame.children = [self.container]


class CEItemsBase(CompEditItems, ABC):
    """Base class for components containing an item list."""

    _DESCRIPTION = "No description"
    _COMP_PATH = ""
    _HELP_TEXT = """"""
    _HELP_URI = {
        "MSTICPy Configuration": (
            "https://msticpy.readthedocs.io/en/latest/"
            + "getting_started/msticpyconfig.html"
        )
    }

    def __init__(self, mp_controls):
        """Initialize the class. Set the controls and retrieve settings."""
        super().__init__(description=self._DESCRIPTION)

        self.mp_controls = mp_controls
        self.comp_defn = self.mp_controls.get_defn(self._COMP_PATH)
        self.settings = self.mp_controls.get_value(self._COMP_PATH)

        self.help.set_help(self._HELP_TEXT, self._HELP_URI)

        self.controls = {}


class SettingsControl(ABC):
    """Abstract base class for settings controls."""

    @property
    @abstractmethod
    def value(self) -> Union[str, Dict[str, Optional[str]]]:
        """Return the current value of the control."""

    @value.setter
    def value(self, value: Union[str, Dict[str, Optional[str]]]):
        """Set value of controls from dict."""


CETabControlDef = Tuple[type, Union[List[Any], Dict[str, Any]]]


class CompEditTabs:
    """Tab class."""

    def __init__(self, tabs: Optional[Dict[str, CETabControlDef]] = None):
        """
        Initialize the CompEditTabs class.

        Parameters
        ----------
        tabs : Optional[Dict[str, Tuple[type, Union[List[Any], Dict[str, Any]]]]], optional
            Tab definitions or contents, by default None.
            Each definition can be a Tuple of class and list of args
            or a Tuple of class and dict of kwargs.

        """
        self.tab = widgets.Tab()
        self.layout = self.tab
        tabs = tabs or {}
        self._tab_state: List[widgets.Widget] = []
        self._tab_lazy_load: Dict[int, CETabControlDef] = {}
        self._tab_names: List[str] = []
        self.controls: Dict[str, Any] = {}
        if tabs:
            for tab_name, tab_ctrl in tabs.items():
                if isinstance(tab_ctrl, CEItemsBase):
                    # if this is an already-instantiated widget, just add the tab
                    self.add_tab(tab_name, tab_ctrl)
                    self._tab_state.append(tab_ctrl.layout)
                elif isinstance(tab_ctrl, tuple):
                    # if we're doing lazy loading, add a lazy-load tab
                    self._add_lazy_tab(tab_name, tab_ctrl)
            if self._tab_lazy_load:
                # Make sure content is loaded for tab 0
                self._load_tab(0)
        self.tab.observe(self._on_select_tab, names="selected_index")

    def _load_tab(self, tab_index):
        """Load any lazily-loaded tab content."""
        if not self._tab_lazy_load:
            return
        if isinstance(self._tab_state[tab_index], widgets.Label):
            wgt_cls, args = self._tab_lazy_load[tab_index]
            # initialize the class object with args or kwargs
            ctrl = wgt_cls(*args) if isinstance(args, list) else wgt_cls(**args)
            # set this control as the current tab state
            self._tab_state[tab_index] = ctrl.layout
            # update the controls dict
            self.controls[self._tab_names[tab_index]] = ctrl
            # update the tab control children with the new state
            self.tab.children = self._tab_state

    def _on_select_tab(self, change):
        """Handle tab select index events."""
        tab_index = change.get("new")
        self._load_tab(tab_index)

    def add_tab(self, tab_name: str, control: CEItemsBase):
        """Add a tab with name `tab_name` and content `control`."""
        self._tab_names.append(tab_name)
        new_idx = len(self._tab_state)
        self._tab_state.append(control.layout)
        self.tab.children = self._tab_state
        self.tab.set_title(new_idx, tab_name)
        self.controls[tab_name] = control

    def _add_lazy_tab(self, tab_name: str, control_def: CETabControlDef):
        """Add a lazily-loaded tab with name `tab_name` and definition `control_def`."""
        self._tab_names.append(tab_name)
        new_idx = len(self._tab_state)
        # add dummy control to tab state and control def to lazy tab dict
        dummy_ctrl = widgets.Label(value="loading...")
        self._tab_state.append(dummy_ctrl)
        self._tab_lazy_load[new_idx] = control_def
        self.controls[tab_name] = dummy_ctrl
        # Refresh tab control children and set title
        self.tab.children = self._tab_state
        self.tab.set_title(new_idx, tab_name)

    def set_tab(self, tab_name: Optional[str], index: int = 0):
        """Programatically set the tab by name or index."""
        if tab_name:
            tab_index = [
                idx
                for idx, tabname in enumerate(self._tab_names)
                if tab_name.casefold() == tabname.casefold()
            ]
            if tab_index:
                self.tab.selected_index = tab_index[0]
            return
        self.tab.selected_index = index

    @property
    def tab_names(self) -> List[str]:
        """Return a list of current tabs."""
        return self._tab_names

    @property
    def tab_controls(self) -> Dict[str, Any]:
        """Return a list of current tab names and controls."""
        return self.controls
