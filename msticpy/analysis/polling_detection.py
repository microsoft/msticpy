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
import numpy as np
import numpy.typing as npt
import pandas as pd

from collections import Counter
from scipy import signal, special
from typing import Tuple, Union

from ..common.utility import export


@export
class PeriodogramPollingDetector:
    def __init__(self) -> None:
        pass

    def _g_test(
        self, PSD: npt.ArrayLike, exclude_pi: bool = False
    ) -> Tuple[float, float]:
        """
        Carry out fishers g test for periodicity

        Fisher's g test tests the null hypothesis that the time series is gaussian white noise
        against the alternative that there is a deterministic periodic component[1]

        If the length of the time series is even then the intensity at pi should be excluded

        If the length of the power spectral density estimate is larger than 700 then an approximate
        p value is calculated otherwise the exact p value is calculate.

        This implementation was taken from the R package GeneCycle[2]

        Parameters
        ----------
        PSD: ArrayLike
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
        [1] M. Ahdesmaki, H. Lahdesmaki and O. Yli-Harja, "Robust Fisher's Test for Periodicity Detection in Noisy Biological Time Series," 2007 IEEE International Workshop on Genomic Signal Processing and Statistics, 2007, pp. 1-4, doi: 10.1109/GENSIPS.2007.4365817.
        [2] https://github.com/cran/GeneCycle/blob/master/R/fisher.g.test.R
        """
        if exclude_pi:
            PSD = PSD[:-1]

        psd_length = len(PSD)
        test_statistic = np.max(PSD) / sum(PSD)
        upper = np.floor(1 / test_statistic).astype("int")

        if psd_length > 700:
            p_value = 1 - (1 - np.exp(- psd_length * test_statistic))**psd_length
        else:
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
        
        if p_value > 1:
            p_value = 1

        return test_statistic, p_value

    def detect_polling(
        self,
        timestamps: npt.ArrayLike,
        process_start: int,
        process_end: int,
        interval: int = 1,
    ) -> float:
        """
        Carry out periodogram polling detecton

        Carries out the the procedure outlined in [1] to detect if the arrival times have a strong periodic component.
        The procedure estimates the periodogram for the data and passes the results to fishers G test.

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
          [1] Heard, N. A. and Rubin-Delanchy, P. T. G. and Lawson, D. J. (2014) Filtering automated polling traffic in computer network flow data. In proceedings of IEEE Joint Intelligence and Security Informatics Conference 2014
          [2] https://github.com/fraspass/human_activity/blob/master/fourier.py

        """
        time_steps = np.arange(process_start, process_end, step=interval)
        counting_process = Counter(timestamps)

        dN = np.array([counting_process[t] for t in time_steps])
        dN_star = dN - len(timestamps) / len(time_steps)

        _, Pxx = signal.periodogram(dN_star)

        _, p_val = self._g_test(Pxx)

        return p_val
