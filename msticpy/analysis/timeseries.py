# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module for timeseries analysis functions."""
import inspect
from datetime import datetime
from typing import Dict, List, Optional

import pandas as pd

from .._version import VERSION
from ..common.exceptions import MsticpyException, MsticpyImportExtraError
from ..common.timespan import TimeSpan
from ..common.utility import check_kwargs, export
from ..vis.timeseries import display_timeseries_anomalies

try:
    from scipy import stats
    from statsmodels.tsa.seasonal import STL
except ImportError as imp_err:
    raise MsticpyImportExtraError(
        "Cannot use this feature without scipy and statsmodel installed",
        title="Error importing package",
        extra="ml",
    ) from imp_err

__version__ = VERSION
__author__ = "Ashwin Patil"


@pd.api.extensions.register_dataframe_accessor("mp_timeseries")
class MsticpyTimeSeriesAccessor:
    """Msticpy pandas accessor for time series functions."""

    def __init__(self, pandas_obj):
        """Initialize the extension."""
        self._df = pandas_obj

    def analyze(self, **kwargs) -> pd.DataFrame:
        """
        Return anomalies in Timeseries using STL.

        Parameters
        ----------
        time_column : str, optional
            If the input data is not indexed on the time column, use this column
            as the time index
        data_column : str, optional
            Use named column if the input data has more than one column.
        seasonal : int, optional
            Seasonality period of the input data required for STL.
            Must be an odd integer, and should normally be >= 7 (default).
        period: int, optional
            Periodicity of the the input data. by default 24 (Hourly).
        score_threshold : float, optional
            standard deviation threshold value calculated using Z-score used to
            flag anomalies, by default 3

        Returns
        -------
        pd.DataFrame
            Returns a dataframe with additional columns by decomposing time series data
            into residual, trend, seasonal, weights, baseline, score and anomalies.
            The anomalies column will have 0, 1,-1 values based on score_threshold set.

        Notes
        -----
        The decomposition method is STL - Seasonal-Trend Decomposition using LOESS

        """
        return ts_anomalies_stl(self._df, **kwargs)

    def anomaly_periods(self, **kwargs):
        """
        Return list of anomaly period as TimeSpans.

        Parameters
        ----------
        time_column : str, optional
            The name of the time column
        period : str, optional
            pandas-compatible time period designator,
            by default "1H"
        pos_only : bool, optional
            If True only extract positive anomaly periods,
            else extract both positive and negative.
            By default, True
        anomalies_column : str, optional
            The column containing the anomalies flag.

        Returns
        -------
        List[TimeSpan] :
            TimeSpan(start, end)

        """
        return find_anomaly_periods(data=self._df, **kwargs)

    def apply_threshold(self, **kwargs):
        """
        Return DataFrame with anomalies calculated based on new threshold.

        Parameters
        ----------
        threshold : float
            Threshold above (beyond) which values will be marked as
            anomalies. Used as positive and negative threshold
            unless `threshold_low` is specified.
        threshold_low : Optional[float], optional
            The threshold below which values will be reported
            as anomalies, by default None.
        anomalies_column : str, optional
            The column containing the anomalies flag.

        Returns
        -------
        pd.DataFrame
            Output DataFrame with recalculated anomalies.

        """
        return set_new_anomaly_threshold(data=self._df, **kwargs)

    def kql_periods(self, **kwargs):
        """
        Return KQL filter expression for anomaly time periods.

        Parameters
        ----------
        time_column : str, optional
            The name of the time column
        period : str, optional
            pandas-compatible time period designator,
            by default "1H"
        pos_only : bool, optional
            If True only extract positive anomaly periods,
            else extract both positive and negative.
            By default, True

        Returns
        -------
        List[TimeSpan] :
            TimeSpan(start, end)

        """
        anom_periods = extract_anomaly_periods(data=self._df, **kwargs)
        return create_time_period_kqlfilter(anom_periods)

    def plot(self, **kwargs):
        """
        Display time series anomalies visualization.

        Parameters
        ----------
            value_column : str, optional
            Name of column holding numeric values to plot against time series to
            determine anomalies
            (the default is 'Total')
        y : str, optional
            alias for "value_column"
        time_column : str, optional
            Name of the timestamp column
            (the default is 'TimeGenerated')
        anomalies_column : str, optional
            Name of the column holding binary status(1/0) for anomaly/benign
            (the default is 'anomalies')
        period : int, optional
            Period of the dataset for hourly-no of days, for daily-no of weeks.
            This is used to correctly calculate the plot height.
            (the default is 30)

        Other Parameters
        ----------------
        ref_time : datetime, optional
            Input reference line to display (the default is None)
        title : str, optional
            Title to display (the default is None)
        legend: str, optional
            Where to position the legend
            None, left, right or inline (default is None)
        yaxis : bool, optional
            Whether to show the yaxis and labels
        range_tool : bool, optional
            Show the the range slider tool (default is True)
        height : int, optional
            The height of the plot figure
            (the default is auto-calculated height)
        width : int, optional
            The width of the plot figure (the default is 900)
        xgrid : bool, optional
            Whether to show the xaxis grid (default is True)
        ygrid : bool, optional
            Whether to show the yaxis grid (default is False)
        color : list, optional
            List of colors to use in 3 plots as specified in order
            3 plots- line(observed), circle(baseline), circle_x/user specified(anomalies).
            (the default is ["navy", "green", "firebrick"])

        Returns
        -------
        figure
            The bokeh plot figure.

        """
        return display_timeseries_anomalies(data=self._df, **kwargs)


# Constants
_DEFAULT_KWARGS = [
    "seasonal",
    "period",
    "score_threshold",
    "time_column",
    "data_column",
]


@export
def ts_anomalies_stl(data: pd.DataFrame, **kwargs) -> pd.DataFrame:
    """
    Return anomalies in Timeseries using STL.

    Parameters
    ----------
    data : pd.DataFrame
        DataFrame as a time series data set retrived from data connector or
        external data source. Dataframe must have 2 columns with time column
        set as index and other numeric value.

    Other Parameters
    ----------------
    time_column : str, optional
        If the input data is not indexed on the time column, use this column
        as the time index
    data_column : str, optional
        Use named column if the input data has more than one column.
    seasonal : int, optional
        Seasonality period of the input data required for STL.
        Must be an odd integer, and should normally be >= 7 (default).
    period: int, optional
        Periodicity of the the input data. by default 24 (Hourly).
    score_threshold : float, optional
        standard deviation threshold value calculated using Z-score used to
        flag anomalies, by default 3

    Returns
    -------
    pd.DataFrame
        Returns a dataframe with additional columns by decomposing time series data
        into residual, trend, seasonal, weights, baseline, score and anomalies.
        The anomalies column will have 0, 1,-1 values based on score_threshold set.

    Notes
    -----
    The decomposition method is STL - Seasonal-Trend Decomposition using LOESS

    """
    check_kwargs(kwargs, _DEFAULT_KWARGS)
    seasonal: int = kwargs.get("seasonal", 7)
    period: int = kwargs.get("period", 24)
    score_threshold: float = kwargs.get("score_threshold", 3.0)
    time_column = kwargs.get("time_column")
    data_column = kwargs.get("data_column")

    if not isinstance(data, pd.DataFrame):
        raise MsticpyException("input data should be a pandas dataframe")

    if time_column:
        data = data.set_index(time_column)
    if data_column:
        data = data[[data_column]]

    # STL method does Season-Trend decomposition using LOESS.
    # Accepts timeseries dataframe
    stl = STL(data, seasonal=seasonal, period=period)
    # Fitting the data - Estimate season, trend and residuals components.
    res = stl.fit()
    result = data.copy()
    # Create dataframe columns from decomposition results
    result["residual"] = res.resid
    result["trend"] = res.trend
    result["seasonal"] = res.seasonal
    result["weights"] = res.weights
    # Baseline is generally seasonal + trend
    result["baseline"] = result["seasonal"] + result["trend"]
    # Type cast and replace na values with 0
    result = result.fillna(0).astype("int64")
    # Calculate zscore based on residual column
    # this column does not contain seasonal/trend components
    result["score"] = stats.zscore(result["residual"])
    # create spikes(1) and dips(-1) based on threshold and seasonal columns
    result.loc[
        (result["score"] > score_threshold) & (result["seasonal"] > 0), "anomalies"
    ] = 1
    result.loc[
        (result["score"] > score_threshold) & (result["seasonal"] < 0), "anomalies"
    ] = -1
    result.loc[(result["score"] < score_threshold), "anomalies"] = 0
    # Datatype casting
    result["anomalies"] = result["anomalies"].astype("int64")

    time_index_name = data.index.name or "index"
    return result.reset_index().sort_values(time_index_name, ascending=True)


timeseries_anomalies_stl = ts_anomalies_stl


def extract_anomaly_periods(
    data: pd.DataFrame,
    time_column: str = "TimeGenerated",
    period: str = "1H",
    pos_only: bool = True,
    anomalies_column: str = "anomalies",
) -> Dict[datetime, datetime]:
    """
    Return dictionary of anomaly periods, merging adjacent ones.

    Parameters
    ----------
    data : pd.DataFrame
        The data to process
    time_column : str, optional
        The name of the time column
    period : str, optional
        pandas-compatible time period designator,
        by default "1H"
    pos_only : bool, optional
        If True only extract positive anomaly periods,
        else extract both positive and negative.
        By default, True
    anomalies_column : str, optional
        The column containing the anomalies flag.

    Returns
    -------
    Dict[datetime, datetime] :
        start_period, end_period

    """
    # Resample data based on period - period is the granularity that
    # we want to merge 2 adjacent samples on.
    anom_filter = [1] if pos_only else [1, -1]
    resampled = (
        data[(data[anomalies_column].isin(anom_filter))]
        .sort_values(time_column)
        .set_index(time_column)
        .resample(period)
    )

    end_period = None
    start_period = None
    periods = {}

    # iterate through the resampled data
    for time, group in resampled:
        if group.empty:
            continue
        if not end_period:
            # If we're not already in an anomaly period
            # create start/end for a new one
            start_period = time - pd.Timedelta(period)
            end_period = time + pd.Timedelta(period)
            periods[start_period] = end_period
        elif (time - end_period) <= pd.Timedelta(
            period
        ) * 2 and start_period is not None:
            # if the current time is less than 2x the period away
            # from our current end_period time, update the end_time
            periods[start_period] = time + pd.Timedelta(period)
        else:
            # otherwise start a new period
            start_period = time - pd.Timedelta(period)
            periods[start_period] = time + pd.Timedelta(period)
        end_period = time
    return periods


def find_anomaly_periods(
    data: pd.DataFrame,
    time_column: str = "TimeGenerated",
    period: str = "1H",
    pos_only: bool = True,
    anomalies_column: str = "anomalies",
) -> List[TimeSpan]:
    """
    Return list of anomaly period as TimeSpans.

    Parameters
    ----------
    data : pd.DataFrame
        The data to process
    time_column : str, optional
        The name of the time column
    period : str, optional
        pandas-compatible time period designator,
        by default "1H"
    pos_only : bool, optional
        If True only extract positive anomaly periods,
        else extract both positive and negative.
        By default, True
    anomalies_column : str, optional
        The column containing the anomalies flag.

    Returns
    -------
    List[TimeSpan] :
        TimeSpan(start, end)

    """
    return [
        TimeSpan(start=key, end=val)
        for key, val in extract_anomaly_periods(
            data=data,
            time_column=time_column,
            period=period,
            pos_only=pos_only,
            anomalies_column=anomalies_column,
        ).items()
    ]


def create_time_period_kqlfilter(periods: Dict[datetime, datetime]) -> str:
    """
    Return KQL time filter expression from anomaly periods.

    Parameters
    ----------
    periods : Dict[datetime, datetime]
        Dict of start, end periods

    Returns
    -------
    str
        KQL filter clause

    """
    time_column = "TimeGenerated"
    time_brackets = [
        f"{time_column} between (datetime({start}) .. datetime({end}))"
        for start, end in periods.items()
    ]

    return f"| where {' or '.join(time_brackets)}"


def set_new_anomaly_threshold(
    data: pd.DataFrame,
    threshold: float,
    threshold_low: Optional[float] = None,
    anomalies_column: str = "anomalies",
) -> pd.DataFrame:
    """
    Return DataFrame with anomalies calculated based on new threshold.

    Parameters
    ----------
    data : pd.DataFrame
        Input DataFrame
    threshold : float
        Threshold above (beyond) which values will be marked as
        anomalies. Used as positive and negative threshold
        unless `threshold_low` is specified.
    threshold_low : Optional[float], optional
        The threshhold below which values will be reported
        as anomalies, by default None.
    anomalies_column : str, optional
        The column containing the anomalies flag.

    Returns
    -------
    pd.DataFrame
        Output DataFrame with recalculated anomalies.

    """
    new_anoms_col = "__new_anomalies__"
    threshold_low = threshold_low or threshold
    new_df = data.assign(__new_anomalies__=0)
    new_df.loc[new_df["score"] >= threshold, new_anoms_col] = 1
    new_df.loc[new_df["score"] <= -threshold_low, new_anoms_col] = -1
    return new_df.drop(columns=[anomalies_column]).rename(
        columns={new_anoms_col: anomalies_column}
    )


def _doc_remove_data(func, remove_other_params: bool = False):
    new_doc = inspect.getdoc(func)
    if not new_doc:
        return ""
    doc_lines = iter(new_doc.split("\n"))
    out_lines = []
    while True:
        try:
            line = next(doc_lines)
            if line.startswith("data"):
                line = next(doc_lines)
                while line.startswith("    "):
                    line = next(doc_lines)
                out_lines.append(line)
                continue
            if remove_other_params and line.startswith("Other Parameters"):
                line = next(doc_lines)
                continue
            out_lines.append(line)
        except StopIteration:
            break
    return "\n".join(out_lines)
