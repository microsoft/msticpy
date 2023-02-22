# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""figure_dimension - helps set the width and height properties of a figure for plotting."""
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
        setattr(fig, 'height', height)
    if hasattr(figure(), "plot_height"):
        setattr(fig, 'plot_height', height)
    if hasattr(figure(), "width"):
        setattr(fig, 'width', width)
    if hasattr(figure(), "plot_width"):
        setattr(fig, 'plot_width', width)
    return fig
