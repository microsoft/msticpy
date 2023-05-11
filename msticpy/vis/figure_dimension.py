# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""figure_dimension - helps set the width and height properties of a figure for plotting."""
from functools import wraps
from typing import Any, Callable

from bokeh.plotting import figure

from .._version import VERSION
from ..common.utility import export

__version__ = VERSION
__author__ = "Claudiu Toma"


@export
def set_figure_size(fig: figure, width: int, height: int) -> figure:
    """Set the figure size.

    Args:
        fig (figure): initial figure
        width (int): width dimension
        height (int): height dimension

    Returns
    -------
        figure: figure with correct width and height

    """
    if hasattr(figure(), "height"):
        setattr(fig, "height", height)
    if hasattr(figure(), "plot_height"):
        setattr(fig, "plot_height", height)
    if hasattr(figure(), "width"):
        setattr(fig, "width", width)
    if hasattr(figure(), "plot_width"):
        setattr(fig, "plot_width", width)
    return fig


_BOKEH_3_FIG_PARAMS = {
    "plot_height": "height",
    "plot_width": "width",
}
_BOKEH_2_FIG_PARAMS = {val: key for key, val in _BOKEH_3_FIG_PARAMS.items()}


def bokeh_figure(func) -> Callable[[Any], Any]:
    """
    Wrap bokeh 'figure' function for version-agnostic param naming.

    Parameters
    ----------
    func : Callable
        The function to wrap

    Returns
    -------
    Callable :
        The wrapped figure function.

    """

    @wraps(func)
    def set_figure_size_params(*args, **kwargs):
        """Re-write parameters names for Bokeh version."""
        # pylint: disable=comparison-with-callable
        if func == figure:
            param_mapper = (
                _BOKEH_3_FIG_PARAMS
                if hasattr(func(), "height")
                else _BOKEH_2_FIG_PARAMS
            )

            func_kwargs = {
                param_mapper.get(param, param): value for param, value in kwargs.items()
            }
            return func(*args, **func_kwargs)
        return func(*args, **kwargs)

    return set_figure_size_params
