# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module for pre-defined widget layouts."""
import os

import ipywidgets as widgets
from IPython.display import display
from ipywidgets import Layout

from .._version import VERSION
from .core import IPyDisplayMixin, RegisteredWidget

__version__ = VERSION
__author__ = "Ian Hellen"


class GetEnvironmentKey(RegisteredWidget, IPyDisplayMixin):
    """
    GetEnvironmentKey.

    Tries to retrieve an environment variable value. The value
    can be changed/set and optionally saved back to the system
    environment.
    """

    _NB_PARAMS = {"value": "_value"}

    def __init__(
        self,
        env_var: str,
        help_str: str = None,
        description: str = "Enter the value: ",
        auto_display: bool = False,
        **kwargs,
    ):
        """
        Create a new instance of GetEnvironmentKey.

        Parameters
        ----------
        env_var : str
            Name of the environment variable.
        help_str : str, optional
            Help to display if the environment variable is not set. (the default is None)
        description : str, optional
            Prompt to display with the text box.
            (the default is "Enter the value: ")
            "prompt" is a alias for this parameter
        auto_display : bool, optional
            Whether to display on instantiation (the default is False)

        """
        env_val = os.environ.get(env_var)
        self._name = env_var
        self._value = ""
        description = kwargs.pop("prompt", description)

        # Call superclass to register
        super().__init__(id_vals=[env_var, description], val_attrs=["_value"], **kwargs)

        # Use the registed widget "remembered" value but if the environment
        # variable is set override with this value.
        if env_val is not None:
            self._value = env_val

        if not self._value and help_str is not None:
            display(widgets.HTML(value=help_str))

        self._w_text = widgets.Text(
            value=self._value,
            description=description,
            layout=Layout(width="50%"),
            style={"description_width": "initial"},
        )

        self._w_save_button = widgets.Button(
            description="Set",
            layout=Layout(width="10%", disabled=False),
            style={"description_width": "initial"},
        )
        self._w_check_save = widgets.Checkbox(
            value=True, description="Save as environment var", disabled=False
        )
        self._w_save_button.on_click(self._on_save_button_clicked)
        self._hbox = widgets.HBox(
            [self._w_text, self._w_save_button, self._w_check_save]
        )

        if auto_display:
            self.display()

    @property
    def value(self):
        """Get the current value of the key."""
        self._value = self._w_text.value or ""
        return self._value.strip()

    @property
    def name(self):
        """Get the current name of the key."""
        return self._name

    @property
    def layout(self):
        """Return underlying widget collection."""
        return self._hbox

    def _on_save_button_clicked(self, button):
        del button
        self._value = self.value
        if self._w_check_save.value:
            os.environ[self._name] = self.value
