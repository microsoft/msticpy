# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module for pre-defined widget layouts."""
import asyncio
from typing import Any, Iterable, Optional

import ipywidgets as widgets
from IPython.display import display

from .._version import VERSION
from .core import IPyDisplayMixin

__version__ = VERSION
__author__ = "Ian Hellen"


class OptionButtons(IPyDisplayMixin):
    """
    OptionButtons creates a sequence of buttons to choose from.

    The widget can be run in synchronous mode as a simple option
    selector or in async mode with a timeout.
    In the latter mode, after the timeout has expired the widget
    value is set to the default option button value.
    To use the async mode you must call `widget.display_async()` with
    the async keyword.

    Attributes
    ----------
    value : str
        The value of the option selected (case-normalized)

    Example
    -------
    >>> opt = OptionButtons(description="Continue something?",
    ...  buttons=["Maybe", "Yes", "Cancel"], timeout=10)
    >>> await opt.display_async()

    """

    def __init__(
        self,
        description: Optional[str] = "Select an option to continue",
        buttons: Optional[Iterable[str]] = None,
        default: Optional[str] = None,
        timeout: int = 0,
        debug: bool = False,
        **kwargs,
    ):
        """
        Initialize the OptionButton widget.

        Parameters
        ----------
        description : Optional[str], optional
            Description label displayed above the buttons,
            by default "Select an option to continue"
        buttons : Optional[Iterable[str]], optional
            A list of button values, by default None. This
            will default to ["Yes", "No", "Cancel"]
            "options" is an alias for this parameter.
        default : Optional[str], optional
            The default value to use on timeout, by default the
            first value in the `buttons` list
        timeout : int, optional
            Timeout in seconds, by default 0
        debug : bool, optional
            Adds some debug information to an Output controle,
            by default False

        """
        buttons = buttons or kwargs.pop("options", None)
        if buttons is None:
            buttons = ["Yes", "No", "Cancel"]
        self._buttons = [widgets.Button(description=b_item) for b_item in buttons]
        self._desc_label = widgets.Label(value=description)
        self._timer_label = widgets.Label(layout=widgets.Layout(left="10px"))
        self.default = default or next(iter(buttons)).casefold()
        self.value: Optional[str] = None
        self.timeout = timeout

        self._completion: Any = None
        self._fut_val: Any = None
        self._debug = debug
        if self._debug:
            self._out = widgets.Output()
        self._create_button_callbacks(self._buttons)

    @property
    def layout(self):
        """Create layout for buttons."""
        return widgets.VBox(
            [self._desc_label, widgets.HBox([*(self._buttons), self._timer_label])]
        )

    def _debug_out(self, mssg: str):
        if self._debug:
            self._out.append_stdout(mssg)

    def _create_button_callbacks(self, buttons):
        """Set up buttons."""

        def getvalue(change):
            """Button on_click handler."""
            self.value = change.description
            for btn in buttons:
                btn.on_click(getvalue, remove=True)

        for btn in buttons:
            btn.on_click(getvalue)

    async def _await_widget(self):
        """Awaitable coroutine for widget."""
        self._debug_out("await_widget entered\n")
        self._create_button_callbacks(self._buttons)
        self._debug_out("buttons set\n")

        done, _ = await asyncio.wait(
            [self._wait_for_button_change(), self._await_timer(self.timeout)],
            return_when=asyncio.FIRST_COMPLETED,
            timeout=self.timeout + 5,
        )
        self._debug_out("wait returned\n")
        self._completion = done
        self._debug_out(str(done))
        return done

    async def _wait_for_button_change(self):
        """Awaitable for button selection state."""
        self._debug_out("wait_for_button_change entered\n")
        while self.value is None:
            await asyncio.sleep(0.1)
            if self._debug:
                self._debug_out("*")

    async def _await_timer(self, timeout: int = 5):
        timeout = max(timeout, 0)
        while timeout > 0:
            self._timer_label.value = f"Waiting {timeout} sec..."
            if self.value:
                self._timer_label.value = f"Option selected: '{self.value}'"
                return
            await asyncio.sleep(1)
            timeout -= 1
        self.value = self.default
        self._timer_label.value = f"Timed out. Defaulted to '{self.value}'"

    async def display_async(self, reset: bool = False):
        """
        Display widget with timeout.

        Parameters
        ----------
        reset : bool, optional
            Resets any current value to None,
            by default False

        """
        if reset:
            self.value = None
        display(self.layout)
        if self._debug:
            display(self._out)
        self._fut_val = asyncio.ensure_future(self._await_widget())
        self._debug_out("future returned\n")
        self._debug_out(str(self._fut_val) + "\n")
