# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------

from bokeh.plotting import figure

class figure_dimension:
    @staticmethod
    def set_size(fig: figure, width, height) -> figure:
        if hasattr(figure(), "height"):
            setattr(fig, 'height', height)
        if hasattr(figure(), "plot_height"):
            setattr(fig, 'plot_height', height)
        if hasattr(figure(), "width"):
            setattr(fig, 'width', width)
        if hasattr(figure(), "plot_width"):
            setattr(fig, 'plot_width', width)
        return fig