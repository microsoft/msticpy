"""Fixture for testing msticpy."""

import datetime as dt
from typing import Union

import pandas as pd
import pytest

from msticpy.datamodel.result import QueryResult


@pytest.fixture(
    params=[
        "",
        "TeSt",
        "test",
        42,
        42.42,
        ["A", "B", "c"],
        [1, 2, 3],
        {
            "key_str": "Value",
            "key_int": 42,
            "key_list": ["A", "B", "c"],
            "key_dict": {"A": 33, "B": "C"},
        },
        dt.datetime.now(tz=dt.timezone.utc),
    ],
    name="sample_data",
)
def generate_sample_data(
    request: pytest.FixtureRequest,
) -> Union[str, int, float, list, dict, dt.datetime]:
    """Return sample data for pattern matching."""
    return request.param


@pytest.fixture(name="simple_dataframe")
def generate_simple_dataframe(
    sample_data: Union[str, float, list, dict, dt.datetime],
) -> pd.DataFrame:
    """Sample dataframe to test get_raw_data."""
    return pd.DataFrame(
        [
            {"key": "A", "value": sample_data},
        ],
    )


@pytest.fixture(name="simple_dataframeresult")
def generate_simple_dataframeresult(simple_dataframe: pd.DataFrame) -> QueryResult:
    """Sample dataframeresult objects to test get_raw_data."""
    return QueryResult(
        name="name",
        query="no query",
        raw_results=simple_dataframe,
        arguments={},
    )
