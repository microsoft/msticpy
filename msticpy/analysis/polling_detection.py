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

There is currently only one technique available for filtering polling data which is
the class PeriodogramPollingDetector.
"""
from collections import Counter
from typing import List, Optional, Tuple, Union

import numpy as np
import numpy.typing as npt
import pandas as pd
from scipy import signal, special

from ..common.utility import export


@export
class PeriodogramPollingDetector:
    """
    Polling detector using the Periodogram to detect strong frequencies.

    Attributes
    ----------
    data: DataFrame
        Dataframe containing the data to be analysed. Must contain a
        column of edges and a column of timestamps

    """

    def __init__(self, data: pd.DataFrame, copy: bool = False) -> None:
        """
        Create periodogram polling detector.

        Parameters
        ----------
        data: DataFrame
            Dataframe containing the data to be analysed. Must contain a
            column of edges and a column of timestamps

        copy: bool
            A bool to indicate whether to copy the dataframe supplied to data
        """
        if copy:
            self.data = data.copy()
        else:
            self.data = data

    def _g_test(self, pxx: npt.NDArray, exclude_pi: bool) -> Tuple[float, float]:
        """
        Carry out fishers g test for periodicity.

        Fisher's g test tests the null hypothesis that the time series is gaussian white noise
        against the alternative that there is a deterministic periodic component[1]

        If the length of the time series is even then the intensity at pi should be excluded

        If the length of the power spectral density estimate is larger than 700 then an approximate
        p value is calculated otherwise the exact p value is calculate.

        This implementation was taken from the R package GeneCycle[2]

        Parameters
        ----------
        pxx: ArrayLike
            Estimate of the power spectral density

        exclude_pi: bool
            A bool to indicate whether the frequnecy located at pi should be removed.

        Returns
        -------
        Tuple[float, float]
            G test test statistic
            G test P value

        References
        ----------
        [1] M. Ahdesmaki, H. Lahdesmaki and O. Yli-Harja, "Robust Fisher's Test for Periodicity
        Detection in Noisy Biological Time Series," 2007 IEEE International Workshop on Genomic
        Signal Processing and Statistics, 2007, pp. 1-4, doi: 10.1109/GENSIPS.2007.4365817.
        [2] https://github.com/cran/GeneCycle/blob/master/R/fisher.g.test.R

        """
        if exclude_pi:
            pxx = pxx[:-1]

        pxx_length = len(pxx)
        test_statistic = np.max(pxx) / sum(pxx)
        upper = np.floor(1 / test_statistic).astype("int")

        if pxx_length > 700:
            p_value = 1 - (1 - np.exp(-pxx_length * test_statistic)) ** pxx_length
        else:
            compose = []
            for j in range(1, upper):
                compose.append(
                    (-1) ** (j - 1)
                    * np.exp(
                        np.log(special.binom(pxx_length, j))
                        + (pxx_length - 1) * np.log(1 - j * test_statistic)
                    )
                )

            p_value = sum(compose)

        p_value = min(p_value, 1)

        return test_statistic, p_value

    def _detect_polling_arr(
        self,
        timestamps: npt.NDArray,
        process_start: int,
        process_end: int,
        interval: int = 1,
    ) -> Tuple[float, float, float]:
        """
        Carry out periodogram polling detecton on an array of timestamps.

        Carries out the the procedure outlined in [1] to detect if the arrival times have a strong
        periodic component.
        The procedure estimates the periodogram for the data and passes the results to fishers G
        test.

        For more information run PeriodogramPollingDetector._g_test.__doc__

        This code was adapted from [2].

        Parameters
        ----------
        timestamps: ArrayLike
            An array like object containing connection arrival times as timestamps
        process_start: int
            The timestamp representing the start of the counting process
        process_end: int
            The timestamp representing the end of the counting process
        interval: int
            The interval in seconds between observations

        Returns
        -------
        p_val: float
            The p value from fishers G test

        References
        ----------
          [1] Heard, N. A. and Rubin-Delanchy, P. T. G. and Lawson, D. J. (2014) Filtering
          automated polling traffic in computer network flow data. In proceedings of IEEE
          Joint Intelligence and Security Informatics Conference 2014
          [2] https://github.com/fraspass/human_activity/blob/master/fourier.py

        """
        time_steps = np.arange(process_start, process_end, step=interval)
        counting_process = Counter(timestamps)

        dn_ = np.array([counting_process[t] for t in time_steps])
        dn_star = dn_ - len(timestamps) / len(time_steps)

        freq, pxx = signal.periodogram(dn_star)

        max_pxx_freq = freq[np.argmax(pxx)]

        if len(dn_star) % 2 == 0:
            _, p_val = self._g_test(pxx, True)
        else:
            _, p_val = self._g_test(pxx, False)

        return p_val, max_pxx_freq, 1 / max_pxx_freq

    def detect_polling(
        self, time_column: str, groupby: Optional[Union[List[str], str]] = None
    ) -> None:
        """
        Detect the time interval which is highly periodic.

        Runs PeriodogramPollingDetector._detect_polling_arr on the time_column and populates a
        p_value column, dominant_frequency column and dominant_interval column.

        If groupby column(s) are given then PeriodogramPollingDetector._detect_polling_arr is ran on
        each group.

        Parameters
        ----------
        time_column: str
            The name of the column that contains timestamps
        groupby: str or list[str], optional
            Column(s) to group by
        """
        ts_col = self.data[time_column]

        start = min(ts_col)
        end = max(ts_col)

        if not groupby:
            p_value, freq, interval = self._detect_polling_arr(ts_col, start, end)

            self.data["p_value"] = p_value
            self.data["dominant_frequency"] = freq
            self.data["dominant_interval"] = interval
        else:
            grouped_results = self.data.groupby(groupby).apply(
                lambda x: self._detect_polling_arr(
                    x[time_column], min(x[time_column]), max(x[time_column])
                )
            )

            grouped_results_df = pd.DataFrame(
                grouped_results.tolist(),
                columns=["p_value", "dominant_frequency", "dominant_interval"],
                index=grouped_results.index,
            ).reset_index()

            self.data = self.data.merge(grouped_results_df)
