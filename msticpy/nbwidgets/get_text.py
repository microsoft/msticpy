# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module for pre-defined widget layouts."""
from typing import Optional

import ipywidgets as widgets
from ipywidgets import Layout

from .._version import VERSION
from .core import IPyDisplayMixin, RegisteredWidget

__version__ = VERSION
__author__ = "Ian Hellen"


class GetText(RegisteredWidget, IPyDisplayMixin):
    """
    GetText.

    Widget to set text value.
    """

    _NB_PARAMS = {"value": "_value"}

    def __init__(
        self,
        default: Optional[str] = None,
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

        self._w_text = widgets.Text(
            description=description,
            layout=Layout(width=kwargs.get("width", "50%")),
            style={"description_width": kwargs.get("description_width", "initial")},
            value=default or kwargs.get("value", ""),
        )

        # Call superclass to register
        super().__init__(id_vals=[default, description], val_attrs=["value"], **kwargs)

        if auto_display:
            self.display()

    @property
    def layout(self):
        """Return underlying widget collection."""
        return self._w_text

    @property
    def value(self):
        """Get the current value of the widget."""
        return self._w_text.value.strip()

    @value.setter
    def value(self, value: str):  # sourcery skip: remove-unnecessary-cast
        """Set the value of the widget."""
        if not isinstance(value, str):
            value = str(value)
        self._w_text.value = value
