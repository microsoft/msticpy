"""Testing file notebook cache."""
from pathlib import Path
from typing import Any

import pandas as pd
import pytest
import pytest_check as check

from msticpy.common.cache import file
from msticpy.datamodel.result import QueryResult


def test_read_write_cache(tmp_path: Path, simple_dataframeresult: QueryResult) -> None:
    """Test method to read and write cache."""
    file_name: str = "digest"
    file_path: Path = tmp_path / file_name

    file.write_cache(simple_dataframeresult, file_name, str(tmp_path))

    check.is_true(file_path.exists())
    check.is_true(file_path.stat().st_size > 0)
    check.is_true(file_path.is_file())

    res: QueryResult = file.read_cache(file_name, export_folder=str(tmp_path))
    check.is_instance(res.raw_results, pd.DataFrame)
    check.is_false(res.raw_results.empty)
    try:
        check.is_true(res.raw_results.equals(simple_dataframeresult.raw_results))
    except ValueError as exc:
        df: pd.DataFrame = res.raw_results
        for i in range(df.shape[0]):
            ref: Any = df.value.iloc[i]
            if isinstance(ref, dict):
                check.is_instance(
                    simple_dataframeresult.raw_results.value.iloc[i],
                    dict,
                )
            else:
                error_msg = "DataFrame comparison is only working for dict."
                raise NotImplementedError(error_msg) from exc


def test_read_cache_from_missing_file() -> None:
    """Test to read cache when file does not exist."""
    file_name: str = "does_not_exist"

    with pytest.raises(FileNotFoundError):
        file.read_cache(file_name)


def test_write_cache_without_export_path(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    simple_dataframeresult: QueryResult,
) -> None:
    """Test to write cache without providing export path."""
    monkeypatch.chdir(tmp_path)
    file_name: str = "digest"
    file_path: Path = Path(file.CACHE_FOLDER_NAME) / file_name

    file.write_cache(simple_dataframeresult, file_name)

    check.is_true(file_path.exists())
    check.is_true(file_path.stat().st_size > 0)
    check.is_true(file_path.is_file())


def test_read_write_cache_with_file(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    simple_dataframeresult: QueryResult,
) -> None:
    """Test to write cache when providing a file as an export path."""
    monkeypatch.chdir(tmp_path)

    export_path_file: Path = Path("random_file")
    export_path_file.touch()
    check.is_true(export_path_file.exists())
    check.is_true(export_path_file.is_file())

    file_name: str = "digest"
    file_path: Path = export_path_file.parent / file.CACHE_FOLDER_NAME / file_name

    file.write_cache(
        simple_dataframeresult,
        file_name,
        export_folder=str(export_path_file),
    )

    check.is_true(file_path.exists())
    check.is_true(file_path.stat().st_size > 0)
    check.is_true(file_path.is_file())

    res: QueryResult = file.read_cache(
        file_name,
        export_folder=str(export_path_file),
    )
    check.equal(simple_dataframeresult, res)
