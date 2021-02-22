# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Pandas DataFrame accessor for Pivot functions."""
import re
import warnings
from typing import Callable, Iterable, Set, Union

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

    def tee_exec(self, df_func: str, *args, **kwargs) -> pd.DataFrame:
        """
        Run the dataframe method or accessor function on the dataframe.

        Parameters
        ----------
        df_func : str
            The name of the function to execute. Accessor methods
            must be of the form "accessor.method".
        args : tuple
            Positional arguments to be passed to the function
        kwargs : dict
            Keyword arguments to be passed to the function.

        Returns
        -------
        pd.DataFrame
            Passed through input DataFrame.

        Notes
        -----
        This function runs the DataFrame method or accessor function.
        It does not alter the DataFrame (unless the function does
        any kind of in-place modification). The function is run and
        the original input DataFrame is returned.

        """
        acc_name = func_name = func = None
        if "." in df_func:
            acc_name, func_name = df_func.split(".")
            accessor = getattr(self._df, acc_name, None)
            if accessor:
                func = getattr(accessor, func_name, None)
        else:
            func = getattr(self._df, df_func, None)
        if func:
            # run the function with any additional args
            func(*args, **kwargs)
        return self._df

    def filter_cols(
        self,
        cols: Union[str, Iterable[str]],
        match_case: bool = False,
        sort_cols: bool = False,
    ) -> pd.DataFrame:
        """
        Filter output columns matching names in `cols` expression(s).

        Parameters
        ----------
        cols : Union[str, Iterable[str]]
            Either a string or a list of strings with filter expressions.
            These can be exact matches for column names, wildcard patterns
            ("*" matches multiple chars and "?" matches a single char),
            or regular expressions.
        match_case: bool, optional
            Use case-sensitive matching, by default False
        sort_cols : bool, optional
            Alphabetically sort column names, by default False

        Returns
        -------
        pd.DataFrame
            The input DataFrame with only columns that match the
            filtering expressions.
        """
        curr_cols = self._df.columns
        filt_cols: Set[str] = set()
        if isinstance(cols, str):
            filt_cols.update(self._name_match(curr_cols, cols, match_case))
        elif isinstance(cols, list):
            for col_filter in cols:
                filt_cols.update(self._name_match(curr_cols, col_filter, match_case))
        if not filt_cols:
            if not sort_cols:
                warnings.warn("Column filter expression(s) did not match any columns")
            filt_cols.update(curr_cols)
        if sort_cols:
            out_cols = sorted(filt_cols)
        else:
            # keep the existing order
            out_cols = [col for col in curr_cols if col in filt_cols]
        return self._df[out_cols]

    @staticmethod
    def _name_match(cur_cols: Iterable[str], col_filter, match_case):
        col_filter = re.sub(r"[^.]\*", ".*", col_filter)
        col_filter = re.sub(r"[^.]\?", ".?", col_filter)
        regex_opts = [re.IGNORECASE] if match_case else []
        return {col for col in cur_cols if re.match(col_filter, col, *regex_opts)}
