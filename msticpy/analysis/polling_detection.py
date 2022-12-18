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

The main function in this module is `auto_filter_periodic_edges` which given a timeseries
of the number of events between two IPs it will test if there are any significant
periodicities.

Contains:
auto_filter_periodic_edges: Method to automatically classify edges as periodic or not.
"""
import numpy as np
import numpy.typing as npt
import pandas as pd

from scipy import signal
from scipy import special
from typing import Tuple


class PeriodogramPollingDetector:
    def __init__(self, edges_df: pd.DataFrame) -> None:
        pass

    def _g_test(self, PSD: npt.ArrayLike, exclude_pi: bool = False) -> Tuple[float, float]:
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


    def _check_equally_spaced(self) -> bool:
        pass
