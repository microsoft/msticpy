# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Tests for archive_utils safe extraction utilities."""

from __future__ import annotations

import io
import tarfile
import zipfile
from pathlib import Path

import pytest

from msticpy.common.archive_utils import (
    safe_tar_extract,
    safe_zip_extract,
    validate_archive_member_path,
)
from msticpy.common.exceptions import MsticpyUserError


class TestValidateArchiveMemberPath:
    """Tests for validate_archive_member_path."""

    def test_valid_relative_path(self, tmp_path: Path) -> None:
        """Validate a simple relative path succeeds."""
        result = validate_archive_member_path("subdir/file.txt", tmp_path)
        assert result == (tmp_path / "subdir" / "file.txt").resolve()

    def test_valid_simple_filename(self, tmp_path: Path) -> None:
        """Validate a plain filename succeeds."""
        result = validate_archive_member_path("file.txt", tmp_path)
        assert result == (tmp_path / "file.txt").resolve()

    def test_rejects_absolute_path(self, tmp_path: Path) -> None:
        """Reject an absolute path member."""
        with pytest.raises(MsticpyUserError, match="absolute path"):
            validate_archive_member_path("/etc/passwd", tmp_path)

    def test_rejects_parent_traversal(self, tmp_path: Path) -> None:
        """Reject a path with parent directory traversal."""
        with pytest.raises(MsticpyUserError, match="parent directory"):
            validate_archive_member_path("../../etc/passwd", tmp_path)

    def test_rejects_hidden_traversal(self, tmp_path: Path) -> None:
        """Reject a path with embedded parent references."""
        with pytest.raises(MsticpyUserError, match="parent directory"):
            validate_archive_member_path("subdir/../../etc/passwd", tmp_path)

    def test_rejects_path_escaping_dest(self, tmp_path: Path) -> None:
        """Reject a path that resolves outside dest_dir."""
        with pytest.raises(MsticpyUserError):
            validate_archive_member_path("subdir/../../../escape", tmp_path)


def _create_tar_gz_bytes(
    members: dict[str, bytes],
    symlinks: dict[str, str] | None = None,
) -> bytes:
    """Create an in-memory tar.gz archive."""
    buf = io.BytesIO()
    with tarfile.open(fileobj=buf, mode="w:gz") as tar:
        for name, data in members.items():
            info = tarfile.TarInfo(name=name)
            info.size = len(data)
            tar.addfile(info, io.BytesIO(data))
        for link_name, target in (symlinks or {}).items():
            info = tarfile.TarInfo(name=link_name)
            info.type = tarfile.SYMTYPE
            info.linkname = target
            tar.addfile(info)
    buf.seek(0)
    return buf.read()


def _create_zip_bytes(members: dict[str, bytes]) -> bytes:
    """Create an in-memory zip archive."""
    buf = io.BytesIO()
    with zipfile.ZipFile(buf, "w") as zf:
        for name, data in members.items():
            zf.writestr(name, data)
    buf.seek(0)
    return buf.read()


class TestSafeTarExtract:
    """Tests for safe_tar_extract."""

    def test_extract_valid_member(self, tmp_path: Path) -> None:
        """Extract a valid tar member to the destination."""
        archive_bytes = _create_tar_gz_bytes({"subdir/data.txt": b"hello"})
        with tarfile.open(fileobj=io.BytesIO(archive_bytes), mode="r:gz") as tar:
            for member in tar.getmembers():
                safe_tar_extract(tar, member, tmp_path)
        extracted = tmp_path / "subdir" / "data.txt"
        assert extracted.is_file()
        assert extracted.read_bytes() == b"hello"

    def test_rejects_traversal_member(self, tmp_path: Path) -> None:
        """Reject tar member with path traversal."""
        archive_bytes = _create_tar_gz_bytes({"../../evil.txt": b"pwned"})
        with tarfile.open(fileobj=io.BytesIO(archive_bytes), mode="r:gz") as tar:
            for member in tar.getmembers():
                with pytest.raises(MsticpyUserError):
                    safe_tar_extract(tar, member, tmp_path)

    def test_rejects_symlink_escaping_dest(self, tmp_path: Path) -> None:
        """Reject tar symlink pointing outside dest_dir."""
        archive_bytes = _create_tar_gz_bytes(
            members={},
            symlinks={"escape_link": "../../../etc/passwd"},
        )
        with tarfile.open(fileobj=io.BytesIO(archive_bytes), mode="r:gz") as tar:
            for member in tar.getmembers():
                with pytest.raises(MsticpyUserError):
                    safe_tar_extract(tar, member, tmp_path)

    def test_allows_symlink_within_dest(self, tmp_path: Path) -> None:
        """Allow tar symlink pointing within dest_dir."""
        archive_bytes = _create_tar_gz_bytes(
            members={"subdir/data.txt": b"hello"},
            symlinks={"subdir/link.txt": "data.txt"},
        )
        with tarfile.open(fileobj=io.BytesIO(archive_bytes), mode="r:gz") as tar:
            for member in tar.getmembers():
                safe_tar_extract(tar, member, tmp_path)

    def test_rejects_absolute_symlink(self, tmp_path: Path) -> None:
        """Reject tar symlink with absolute target."""
        archive_bytes = _create_tar_gz_bytes(
            members={},
            symlinks={"abs_link": "/etc/shadow"},
        )
        with tarfile.open(fileobj=io.BytesIO(archive_bytes), mode="r:gz") as tar:
            for member in tar.getmembers():
                with pytest.raises(MsticpyUserError):
                    safe_tar_extract(tar, member, tmp_path)

    def test_rejects_device_member(self, tmp_path: Path) -> None:
        """Reject tar member with unsupported type (device file)."""
        buf = io.BytesIO()
        with tarfile.open(fileobj=buf, mode="w:gz") as tar:
            info = tarfile.TarInfo(name="devfile")
            info.type = tarfile.CHRTYPE
            tar.addfile(info)
        buf.seek(0)
        with tarfile.open(fileobj=buf, mode="r:gz") as tar:
            for member in tar.getmembers():
                with pytest.raises(MsticpyUserError, match="unsupported"):
                    safe_tar_extract(tar, member, tmp_path)


class TestSafeZipExtract:
    """Tests for safe_zip_extract."""

    def test_extract_valid_member(self, tmp_path: Path) -> None:
        """Extract a valid zip member to the destination."""
        archive_bytes = _create_zip_bytes({"subdir/data.txt": b"hello"})
        with zipfile.ZipFile(io.BytesIO(archive_bytes)) as zf:
            for name in zf.namelist():
                safe_zip_extract(zf, name, tmp_path)
        extracted = tmp_path / "subdir" / "data.txt"
        assert extracted.is_file()
        assert extracted.read_bytes() == b"hello"

    def test_rejects_traversal_member(self, tmp_path: Path) -> None:
        """Reject zip member with path traversal."""
        archive_bytes = _create_zip_bytes({"../../evil.txt": b"pwned"})
        with zipfile.ZipFile(io.BytesIO(archive_bytes)) as zf:
            for name in zf.namelist():
                with pytest.raises(MsticpyUserError):
                    safe_zip_extract(zf, name, tmp_path)

    def test_rejects_absolute_member(self, tmp_path: Path) -> None:
        """Reject zip member with absolute path."""
        archive_bytes = _create_zip_bytes({"/tmp/evil.txt": b"pwned"})
        with zipfile.ZipFile(io.BytesIO(archive_bytes)) as zf:
            for name in zf.namelist():
                with pytest.raises(MsticpyUserError):
                    safe_zip_extract(zf, name, tmp_path)
