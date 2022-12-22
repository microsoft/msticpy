# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Polling detection module.

This module is used to highlight edges that are highly periodic and likely to be
generated automatically. The periodic edges could be software polling a server for 
updates or malware beaconing and checking for instructions.

There is currently is only one technique available for filtering polling data which is
the class PeriodogramPollingDetector. This requires a dataframe containing a time series
of source --> destination pairs. 

.. list-table:: Example DataFrame
   :header-rows: 1

   * - datetime
     - source_ip
     - destination_ip
   * - 2022-01-01 00:00:03
     - ip1
     - ip2
   * - 2022-01-01 00:02:34
     - ip1
     - ip2
   * - 2022-01-01 00:02:55
     - ip1
     - ip2
   * - ...
     - ip1
     - ip2
   * - 2022-01-01 07:22:11
     - ip1
     - ip3
   * - 2022-01-01 14:45:07
     - ip1
     - ip3
   * - 2022-01-01 15:10:23
     - ip1
     - ip3
   * - ...
     - ip1
     - ip3

The PeriodogramPollingDetector.detect_polling() function will then identify edges
(source --> destination pairs) that are highly periodic and likely include polling traffic
"""
import numpy as np
import numpy.typing as npt
import pandas as pd

from scipy import signal
from scipy import special
from typing import Tuple, Union

# TODO: Split data transformation and polling detector into separate classes

class PeriodogramPollingDetector:
    def __init__(self, edges_df: pd.DataFrame) -> None:
        if self._check_data_frame_columns(edges_df):
            self.edges_df = edges_df
        else:
            raise ValueError(
                (
                    "Dataframe supplied is missing some columns. Please ensure that dataframe "
                    "contains columns ['source_ip', 'destination_ip', 'datetime']"
                )
            )

    def _g_test(
        self, PSD: npt.ArrayLike, exclude_pi: bool = False
    ) -> Tuple[float, float]:
        """
        Carry out fishers g test for periodicity

        Fisher's g test tests the null hypothesis that the time series is gaussian white noise
        against the alternative that there is a deterministic periodic component[1].

        This implementation was taken from the R package GeneCycle[2]

        refs:
        [1] M. Ahdesmaki, H. Lahdesmaki and O. Yli-Harja, "Robust Fisher's Test for Periodicity Detection in Noisy Biological Time Series," 2007 IEEE International Workshop on Genomic Signal Processing and Statistics, 2007, pp. 1-4, doi: 10.1109/GENSIPS.2007.4365817.
        [2] https://github.com/cran/GeneCycle/blob/master/R/fisher.g.test.R

        Parameters
        ----------
        PSD: ArrayLike
            Estimate of the power spectral density

        Returns
        -------
        Tuple[float, float]
            G test test statistic
            G test P value
        """
        if exclude_pi:
            PSD = PSD[:-1]

        psd_length = len(PSD)
        test_statistic = np.max(PSD) / sum(PSD)
        upper = np.floor(1 / test_statistic).astype("int")

        compose = []
        for j in range(1, upper):
            compose.append(
                (-1) ** (j - 1)
                * np.exp(
                    np.log(special.binom(psd_length, j))
                    + (psd_length - 1) * np.log(1 - j * test_statistic)
                )
            )

        p_value = sum(compose)

        return test_statistic, p_value

    def _check_equally_spaced(self, df: pd.DataFrame) -> bool:
        """
        Check if the time series is equally spaced

        Parameters
        ----------
        df: DataFrame
          The dataframe containing the time series for a single edge

        Returns
        -------
        bool
            True if time series is equally spaced, False otherwise
        """
        return df["datetime"].diff().nunique() == 1

    def _check_data_frame_columns(self, df: pd.DataFrame) -> bool:
        """
        Check if the time series has the required columns

        Parameters
        ----------
        df: DataFrame
          The dataframe containing the edge time series data

        Returns
        -------
          bool
            True if dataframe has the required columns, false otherwise
        """
        return all(
            pd.Index(["edges", "datetime"]).isin(df.columns)
        )

    def transform_data(
        self,
        df: pd.DataFrame,
        freq: Union[str, pd.tseries.offsets.DateOffset],
        min_datetime: str,
        max_datetime: str
    ) -> pd.DataFrame:
        """
        Transforms edge time series data to a discrete time bernoulli process

        The dataframe supplied should have connection times for a single edge, this function then transforms
        this to a discrete time bernoulli process. The data for this process is a dataframe of equally space
        datetimes with a 1 if there was at least 1 connection during this interval or 0 otherwise.

        Parameters
        ----------
        df: DataFrame
          The dataframe containing the edge time series data
        freq: str or DateOffset
          A frequency string representing the bin size of the discrete time bernoulli process. For example
          a bernoulli process with observations every 5 minutes would have a frequency string of '5T'. 
          More information can be found at https://pandas.pydata.org/docs/user_guide/timeseries.html#timeseries-offset-aliases

        Returns
        -------
        DataFrame
          The transformed data
        """
        if df["edges"].nunique() != 1:
          raise ValueError(
            f"This function only transforms data for a single edge. The data frame supplied has {df['edges'].nunique()} edges"
          )

        expected_datetimes = pd.date_range(
            min_datetime, max_datetime, freq=freq, inclusive="left"
        ).to_series(name="datetime")

        connections = (
            df.set_index("datetime")
            .resample(freq)
            .apply(lambda x: any(x) * 1)[["edges"]]
            .rename({"edges": "connection"}, axis=1)
            .reset_index()
        )

        merged_df = pd.merge(
          expected_datetimes,
          connections,
          how="left",
          on="datetime"
        ).fillna(0)

        merged_df["connection"] = merged_df["connection"].astype("int64")
        merged_df["edges"] = df["edges"].iloc[1]

        return merged_df

    def detect_polling(self, df: pd.DataFrame) -> pd.DataFrame:
        edges = []
        for edge in df["edges"].unique():
            edge_df = df[df["edges"] == edge]
            edge_df = self.transform_data(edge_df, "T", "2022-01-01 00:00:00", "2022-01-02 00:00:00")

            f, Pxx = signal.periodogram(edge_df["connection"])

            test_stat, p_val = self._g_test(Pxx)

            edges.append(pd.DataFrame({"edges": [edge], "p_val": [p_val]}))
        
        edges = pd.concat(edges)

        return edges.sort_values(by="p_val")


