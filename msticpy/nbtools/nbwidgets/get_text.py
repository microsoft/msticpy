# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module for pre-defined widget layouts."""
import ipywidgets as widgets
from ipywidgets import Layout

from ..._version import VERSION
from .core import IPyDisplayMixin, RegisteredWidget

__version__ = VERSION
__author__ = "Ian Hellen"


class GetText(RegisteredWidget, IPyDisplayMixin):
    """
    GetEnvironmentKey.

    Tries to retrieve an environment variable value. The value
    can be changed/set and optionally saved back to the system
    environment.
    """

    def __init__(
        self,
        default: str = None,
        description: str = "Enter the value: ",
        auto_display: bool = False,
        **kwargs,
    ):
        """
        Create a new instance of GetEnvironmentKey.

        Parameters
        ----------
        default : str
            Default value.
        description : str, optional
            Prompt to display with the text box.
            (the default is "Enter the value: ")
            "prompt" is a alias for this parameter
        auto_display : bool, optional
            Whether to display on instantiation (the default is False)

        See Also
        --------
        RegisteredWidget

        """
        self._value = default
        description = kwargs.pop("prompt", description)

        # Call superclass to register
        super().__init__(id_vals=[default, description], val_attrs=["_value"], **kwargs)

        self._w_text = widgets.Text(
            value=self._value,
            description=description,
            layout=Layout(width="50%"),
            style={"description_width": "initial"},
        )

        self._w_text.observe(self._update_value, names="value")
        if auto_display:
            self.display()

    def _update_value(self, change):
        self._value = change.get("new", "")

    @property
    def layout(self):
        """Return underlying widget collection."""
        return self._w_text

    @property
    def value(self):
        """Get the current value of the key."""
        return self._value.strip() if self._value else None

    def display(self):
        """Display the interactive widgets."""
        if self._value:
            self._w_text.value = self._value
        super().display()
