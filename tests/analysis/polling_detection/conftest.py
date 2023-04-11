# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Polling detection module test fixtures"""
import numpy as np
import pandas as pd
import pytest

np.random.seed(10)


@pytest.fixture()
def periodic_data():
    np.random.seed(10)

    N = 86400
    start_ts = 1669852800
    end_ts = start_ts + N

    homo_pois = np.random.poisson(1.5, N)
    freq = 0.01666666666666
    periodic = (10 * np.sin(2 * np.pi * freq * np.arange(0, N))).astype("int")
    periodic[periodic < 0] = 0
    x = (periodic + homo_pois).astype("bool")
    ts = np.arange(start_ts, end_ts)[x]

    return pd.DataFrame({"edges": "periodic_edge", "timestamps": ts})


@pytest.fixture()
def non_periodic_data():
    np.random.seed(10)

    N = 86400
    start_ts = 1669852800
    end_ts = start_ts + N

    homo_pois = np.random.poisson(1.5, N)
    x = homo_pois.astype("bool")
    ts = np.arange(start_ts, end_ts)[x]

    return pd.DataFrame({"edges": "non_periodic_edge", "timestamps": ts})
