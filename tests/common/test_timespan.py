# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Timespan unit test."""
from datetime import datetime, timedelta

import pytest
import pytest_check as check

from msticpy.common.timespan import TimeSpan

__author__ = "Ian Hellen"

# pylint: disable=redefined-outer-name


def _validate_timespan(timespan, start=None, end=None, period=None):
    if start is not None:
        check.equal(start, timespan.start)
    if end is not None:
        check.equal(end, timespan.end)
    if period is not None:
        check.equal(period, timespan.period)


def test_timespan_parms():
    """Test standard parameters."""
    end = datetime.utcnow()
    period = timedelta(days=1)
    start = end - period
    tspan = TimeSpan(start=start, end=end)
    _validate_timespan(tspan, start, end)

    tspan = TimeSpan(end=end, period=period)
    _validate_timespan(tspan, start, end)

    tspan = TimeSpan(end=end, period="1D")
    _validate_timespan(tspan, start, end)

    tspan = TimeSpan(end=str(end), period="1D")
    _validate_timespan(tspan, start, end)

    tspan = TimeSpan(start=str(start), end=str(end))
    _validate_timespan(tspan, start, end)

    tspan = TimeSpan(start=str(start), period="1D")
    _validate_timespan(tspan, start, end)

    # end is set to utcnow()
    tspan = TimeSpan(start=start)
    _validate_timespan(tspan, start)

    # end is set to utcnow()
    tspan = TimeSpan(period=period)
    _validate_timespan(tspan, period=period)


def test_timespan_eq():
    """Test creating Timespan from another Timespan."""
    period = timedelta(days=1)
    tspan = TimeSpan(period=period)

    # Timespan object as a parameter
    tspan2 = TimeSpan(timespan=tspan)
    check.equal(tspan2, tspan)
    check.equal(hash(tspan2), hash(tspan))

    tspan2 = TimeSpan(timespan=(tspan.start, tspan.end))
    check.equal(tspan2, tspan)
    tspan2 = TimeSpan(timespan=(str(tspan.start), str(tspan.end)))
    check.equal(tspan2, tspan)


def test_timespan_timeselector():
    """Test timespan with a time selector object."""
    end = datetime.utcnow()
    period = timedelta(days=1)
    start = end - period
    tspan = TimeSpan(period=period)

    # pylint: disable=too-few-public-methods
    class _TestTime:
        """Class to emulate QueryTimes widget. etc."""

        start = None
        end = None
        period = None

    test_t = _TestTime()
    test_t.start = start
    test_t.end = str(end)
    test_t.period = "1D"

    tspan = TimeSpan(timespan=test_t)
    _validate_timespan(tspan, start, end)


def test_timespan_invalid_params():
    """Test error handling for invalid params."""
    period = timedelta(days=1)
    with pytest.raises(ValueError):
        TimeSpan()
    with pytest.raises(ValueError):
        TimeSpan(start="foo", period=period)
    with pytest.raises(ValueError):
        TimeSpan(start=None, end=None)
    with pytest.raises(ValueError):
        TimeSpan(period="some length")
    with pytest.raises(ValueError):
        TimeSpan(period=1)
