# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module for timeseries analysis functions."""
import pandas as pd
from scipy import stats
from statsmodels.tsa.seasonal import STL

from .._version import VERSION
from ..common.exceptions import MsticpyException
from ..common.utility import check_kwargs, export

__version__ = VERSION
__author__ = "Ashwin Patil"


# Constants
_DEFAULT_KWARGS = ["seasonal", "period", "score_threshold"]


@export
def timeseries_anomalies_stl(data: pd.DataFrame, **kwargs) -> pd.DataFrame:
    """
    Discover anomalies in Timeseries data using STL(Seasonal-Trend Decomposition using LOESS).

    Parameters
    ----------
    data : pd.DataFrame
        DataFrame as a time series data set retrived from data connector or external data source.
        Dataframe must have 2 columns with time column set as index and other numeric value.

    Other Parameters
    ----------------
    seasonal : int, optional
        Seasonality period of the input data required for STL.
        Must be an odd integer, and should normally be >= 7 (default).
    period: int, optional
        Periodicity of the the input data. by default 24 (Hourly).
    score_threshold : float, optional
        standard deviation threshold value calculated using Z-score used to flag anomalies,
        by default 3

    Returns
    -------
    pd.DataFrame
        Returns a dataframe with additional columns by decomposing time series data
        into residual, trend, seasonal, weights, baseline, score and anomalies.
        The anomalies column will have 0, 1,-1 values based on score_threshold set.

    """
    check_kwargs(kwargs, _DEFAULT_KWARGS)
    seasonal: int = kwargs.get("seasonal", 7)
    period: int = kwargs.get("period", 24)
    score_threshold: float = kwargs.get("score_threshold", 3.0)

    if not isinstance(data, pd.DataFrame):
        raise MsticpyException("input data should be a pandas dataframe")

    # STL method does Season-Trend decomposition using LOESS. Accepts timeseries dataframe
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
    result = result.reset_index()
    return result
