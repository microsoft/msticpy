# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module for pre-defined widget layouts."""
import ipywidgets as widgets

from .._version import VERSION
from .core import IPyDisplayMixin

__version__ = VERSION
__author__ = "Ian Hellen"


class Progress(IPyDisplayMixin):
    """UI Progress bar."""

    def __init__(self, completed_len: int, visible: bool = True):
        """
        Instantiate new _Progress UI.

        Parameters
        ----------
        completed_len : int
            The expected value that indicates 100% done.
        visible : bool
            If True start the progress UI visible, by default True.

        """
        self._completed = 0
        self._total = completed_len
        self._progress = widgets.IntProgress(
            value=0,
            max=100,
            description="Progress:",
            bar_style="info",
            orientation="horizontal",
        )
        self._done_label = widgets.Label(value="0%")
        if visible:
            self.show()
        else:
            self.hide()
        self.layout = widgets.HBox([self._progress, self._done_label])
        self.display()

    @property
    def value(self) -> int:
        """
        Return the current progress value.

        Returns
        -------
        int
            Progess value

        """
        return self._completed

    @property
    def max(self) -> int:
        """
        Return the current progress maximum value.

        Returns
        -------
        int
            Max value

        """
        return self._total

    def update_progress(self, new_total: int = 0, delta: int = 0):
        """
        Update progress UI by increment or new total.

        Parameters
        ----------
        new_total : int, optional
            New total, by default 0
        delta : int, optional
            Increment to update current total, by default 0

        """
        if new_total:
            self._completed = new_total
        else:
            self._completed += delta
        perc_total = int(100 * self._completed / self._total)
        self._progress.value = perc_total
        self._done_label.value = f"{perc_total}%"

    def show(self):
        """Make the controls visible."""
        self._hide_show("visible")

    def hide(self):
        """Hide the controls."""
        self._hide_show("hidden")

    def _hide_show(self, visibility):
        vis_layout = widgets.Layout(visibility=visibility)
        self._progress.layout = vis_layout
        self._done_label.layout = vis_layout
