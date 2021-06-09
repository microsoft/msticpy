# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Component Edit base and mixin classes."""
from abc import ABC, abstractproperty
from time import sleep
from typing import Dict, Optional, Union

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


class CompEditTabs:
    """Tab class."""

    def __init__(self, tabs: Optional[Dict[str, widgets.Widget]] = None):
        """Initialize the class."""
        self.tab = widgets.Tab()
        self.layout = self.tab
        self.tab_controls = tabs or {}
        if self.tab_controls:
            for tab_name, tab_ctrl in self.tab_controls.items():
                self.add_tab(tab_name, tab_ctrl)

    def add_tab(self, tab_name: str, control: widgets.Widget):
        """Add a tab with name `tab_name` and content `control`."""
        self.tab_controls[tab_name] = control
        tab_children = list(self.tab.children)
        new_idx = len(tab_children)
        tab_children.append(control.layout)
        self.tab.children = tab_children
        self.tab.set_title(new_idx, tab_name)

    def set_tab(self, tab_name: Optional[str], index: int = 0):
        """Programatically set the tab by name or index."""
        if tab_name:
            tab_index = [
                idx
                for idx, tabname in enumerate(self.tab_controls)
                if tab_name.casefold() == tabname.casefold()
            ]
            if tab_index:
                self.tab.selected_index = tab_index[0]
            return
        self.tab.selected_index = index

    @property
    def tab_names(self):
        """Return a list of current tabs."""
        return list(enumerate(self.tab_controls.keys()))


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
        self.btn_add = widgets.Button(description="Add", layout=btn_layout)
        self.btn_del = widgets.Button(description="Delete", layout=btn_layout)
        self.btn_save = widgets.Button(description="Update", layout=btn_layout)
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
        self.btn_save = widgets.Button(description="Save")
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


class CESimpleBase(CompEditSimple):
    """Base class for components containing no item list."""

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
        self.comp_defn = self._get_settings_path(
            mp_controls.config_defn, self._COMP_PATH
        )
        self.settings = self._get_settings_path(mp_controls.mp_config, self._COMP_PATH)

        self.help.set_help(self._HELP_TEXT, self._HELP_URI)

        self.controls = {}


class SettingsControl(ABC):
    """Abstract base class for settings controls."""

    @abstractproperty
    def value(self) -> Union[str, Dict[str, Optional[str]]]:
        """Return the current value of the control."""

    @value.setter
    def value(self, value: Union[str, Dict[str, Optional[str]]]):
        """Set value of controls from dict."""
