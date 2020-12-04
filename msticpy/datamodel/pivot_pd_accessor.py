# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Pandas DataFrame accessor for Pivot functions."""
import warnings
from typing import Callable, Iterable

import pandas as pd
from IPython import get_ipython
from IPython.display import HTML, display

from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


@pd.api.extensions.register_dataframe_accessor("mp_pivot")
class PivotAccessor:
    """Pandas api extension for Pivot functions."""

    def __init__(self, pandas_obj):
        """Instantiate pivot extension class."""
        self._df = pandas_obj
        self._ip = get_ipython()

    def run(self, func: Callable[..., pd.DataFrame], **kwargs) -> pd.DataFrame:
        """
        Run a pivot function on the current DataFrame.

        Parameters
        ----------
        func : Callable[..., pd.DataFrame]
            Pivot function to run
        kwargs:
            Keyword arguments to pass to `func`.
            A column specification (e.g. column="src_col_name")
            is usually the minimum needed.
            For data queries the `column` keyword must be the
            name of the the query parameter (e.g. host_name = "src_col_name")

        Returns
        -------
        pd.DataFrame
            The output DataFrame from the function.

        Notes
        -----
        You can pass the `join` keyword argument to most
        pivot functions. Values for join are "inner", "left", "right"
        or "outer".

        """
        return self._df.pipe((func, "data"), **kwargs)

    def display(
        self,
        title: str = None,
        cols: Iterable[str] = None,
        query: str = None,
        head: int = None,
    ) -> pd.DataFrame:
        """
        Display the DataFrame in the middle of a pipeline.

        Parameters
        ----------
        title : str, optional
            Title to display for the DataFrame, by default None
        cols : Iterable[str], optional
            List of columns to display, by default None
        query : str, optional
            Query to filter the displayed data, by default None
            This should be a string executable by the DataFrame.query
            function
        head : int, optional
            Limit the displayed output to `head` rows, by default None

        Returns
        -------
        pd.DataFrame
            Passed through input DataFrame.

        """
        if title:
            display(HTML(f"<h3>{title}</h3>"))
        disp_df = self._df
        if cols:
            disp_df = disp_df[cols]
        if query:
            disp_df = disp_df.query(query)  # , parser='python', engine='python')
        if head:
            disp_df = disp_df.head(head)
        display(disp_df)
        return self._df

    def tee(self, var_name: str, clobber: bool = False) -> pd.DataFrame:
        """
        Save current dataframe to `var_name` in the IPython user namespace.

        Parameters
        ----------
        var_name : str
            The name of the DF variable to create.
        clobber : bool, optional
            Whether to overwrite an existing variable of the same name,
            by default False

        Returns
        -------
        pd.DataFrame
            Passed through input DataFrame.

        Notes
        -----
        This function only works in an IPython/Jupyter notebook environment.
        It will attempt to create a variable in the user local namespace
        that references the current state of the DataFrame in the pipeline.

        By default it will not overwrite an existing variable of the same
        name (specify `clobber=True` to overwrite)

        """
        if self._ip and var_name:
            if var_name in self._ip.ns_table["user_local"] and not clobber:
                warnings.warn(f"Did not overwrite existing {var_name} in namespace")
            else:
                self._ip.ns_table["user_local"][var_name] = self._df
        return self._df
