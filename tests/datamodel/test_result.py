"""Tests for Query Result datamodel."""

import pandas as pd
import pytest_check as check

from msticpy.datamodel.result import QueryResult


def test_normalizer(simple_dataframeresult: QueryResult) -> None:
    """Test attribute normalizer from DataFrameResult."""
    check.is_instance(simple_dataframeresult.normalizer, str)
    check.equal(simple_dataframeresult.normalizer, "QueryResult")


def test_total_results(simple_dataframeresult: QueryResult) -> None:
    """Test attribute total_results from DataFrameResult."""
    check.is_instance(simple_dataframeresult.total_results, int)
    check.greater_equal(simple_dataframeresult.total_results, 0)


def test_results(simple_dataframeresult: QueryResult) -> None:
    """Test attribute results from DataFrameResult."""
    check.is_instance(simple_dataframeresult.results, list)
    check.greater_equal(len(simple_dataframeresult.results), 0)


def test__repr_markdown_(simple_dataframeresult: QueryResult) -> None:
    """Test attribute _repr_markdown_ from DataFrameResult."""
    res: str = (
        QueryResult._repr_markdown_(  # noqa: SLF001 #pylint: disable=protected-access
            simple_dataframeresult
        )
    )
    check.is_instance(res, str)
    check.greater_equal(len(res), 0)


def test__repr_html_(simple_dataframeresult: QueryResult) -> None:
    """Test attribute _repr_html_ from DataFrameResult."""
    res: str = (
        QueryResult._repr_html_(  # noqa: SLF001 #pylint: disable=protected-access
            simple_dataframeresult
        )
    )
    check.is_instance(res, str)
    check.greater_equal(len(res), 0)


def test__eq_(simple_dataframeresult: QueryResult) -> None:
    """Test attribute _repr_html_ from DataFrameResult."""
    check.equal(simple_dataframeresult, simple_dataframeresult)
    other_sample: QueryResult = QueryResult(
        name="my name",
        query="my query",
        raw_results=pd.DataFrame(),
        arguments={},
    )
    check.not_equal(simple_dataframeresult, other_sample)
    check.not_equal(simple_dataframeresult, 42)
