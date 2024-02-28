"""Define standard models for query results."""
from __future__ import annotations

import datetime as dt
from dataclasses import dataclass, field
from typing import TYPE_CHECKING, Any

from dataclasses_json import dataclass_json
from typing_extensions import Self

if TYPE_CHECKING:
    from collections.abc import Hashable

    import pandas as pd


@dataclass_json
@dataclass
class QueryResult:
    """DataFrame model."""

    name: str
    query: str
    raw_results: pd.DataFrame
    arguments: dict[str, Any] = field(default_factory=dict)
    timestamp: dt.datetime = field(default_factory=dt.datetime.utcnow)

    @property
    def normalizer(self: Self) -> str:
        """Normalizer class name."""
        return str(self.__class__.__name__)

    @property
    def total_results(self: Self) -> int:
        """Total Results."""
        return len(self.results)

    @property
    def results(self: Self) -> list[dict[Hashable, Any]]:
        """Return results as a List of dicts."""
        return self.raw_results.to_dict(orient="records")

    def _repr_markdown_(self: Self) -> str:
        """Represent as markdown."""
        return self.raw_results.to_html(index=False)

    def _repr_html_(self: Self) -> str:
        """Represent as HTML."""
        return self.raw_results.to_html(index=False)

    def __eq__(self: Self, __value: object) -> bool:
        """Return True if self and __value are equal."""
        if not isinstance(__value, QueryResult):
            return False
        return (
            self.name == __value.name
            and self.query == __value.query
            and len(self.arguments) == len(__value.arguments)
            and self.raw_results.equals(__value.raw_results)
        )
