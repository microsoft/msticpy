# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Safe archive extraction utilities to prevent path traversal attacks."""

from __future__ import annotations

import logging
import tarfile
import zipfile
from pathlib import Path

from .exceptions import MsticpyUserError

logger: logging.Logger = logging.getLogger(__name__)


def validate_archive_member_path(
    member_name: str,
    dest_dir: str | Path,
) -> Path:
    """
    Validate that an archive member path does not escape dest_dir.

    Checks for absolute paths, parent directory references, and
    resolved path containment within the destination directory.

    Parameters
    ----------
    member_name : str
        The archive member name/path to validate.
    dest_dir : str or Path
        The intended extraction destination directory.

    Returns
    -------
    Path
        The resolved target path within dest_dir.

    Raises
    ------
    MsticpyUserError
        If the member path would escape the destination directory.

    """
    member_path = Path(member_name)
    if member_path.is_absolute() or member_name.startswith("/"):
        raise MsticpyUserError(
            f"Archive member has an absolute path '{member_name}'.",
            "This may indicate a malicious archive (path traversal attack).",
            title="Unsafe archive member path",
        )
    if ".." in member_path.parts:
        raise MsticpyUserError(
            f"Archive member contains parent directory reference: '{member_name}'.",
            "This may indicate a malicious archive (path traversal attack).",
            title="Unsafe archive member path",
        )
    dest = Path(dest_dir).resolve()
    target = (dest / member_name).resolve()
    if not (target == dest or str(target).startswith(str(dest) + "\\")):
        # Also check with forward slash for cross-platform safety
        if not str(target).startswith(str(dest) + "/"):
            raise MsticpyUserError(
                f"Archive member path escapes the destination directory: '{member_name}'.",
                f"Resolved path '{target}' is outside '{dest}'.",
                "This may indicate a malicious archive (path traversal attack).",
                title="Unsafe archive member path",
            )
    logger.debug("Validated archive member path: %s", member_name)
    return target


def safe_tar_extract(
    tar: tarfile.TarFile,
    member: tarfile.TarInfo,
    dest_dir: str | Path,
) -> None:
    """
    Safely extract a single tar archive member after path validation.

    Validates that the member path does not escape dest_dir and
    rejects symlinks or hardlinks that could be used for traversal.

    Parameters
    ----------
    tar : tarfile.TarFile
        The open tar archive.
    member : tarfile.TarInfo
        The tar member to extract.
    dest_dir : str or Path
        The destination directory for extraction.

    Raises
    ------
    MsticpyUserError
        If the member path is unsafe or the member is a
        symlink/hardlink pointing outside dest_dir.

    """
    if member.issym() or member.islnk():
        _validate_tar_link(member, dest_dir)
    validate_archive_member_path(member.name, dest_dir)
    tar.extract(member, dest_dir)


def safe_zip_extract(
    zip_file: zipfile.ZipFile,
    file_name: str,
    dest_dir: str | Path,
) -> None:
    """
    Safely extract a single zip archive member after path validation.

    Validates that the member path does not escape dest_dir.

    Parameters
    ----------
    zip_file : zipfile.ZipFile
        The open zip archive.
    file_name : str
        The name of the file to extract.
    dest_dir : str or Path
        The destination directory for extraction.

    Raises
    ------
    MsticpyUserError
        If the member path would escape the destination directory.

    """
    validate_archive_member_path(file_name, dest_dir)
    zip_file.extract(file_name, path=dest_dir)


def _validate_tar_link(
    member: tarfile.TarInfo,
    dest_dir: str | Path,
) -> None:
    """
    Validate that a symlink or hardlink target is within dest_dir.

    Parameters
    ----------
    member : tarfile.TarInfo
        The tar member (symlink or hardlink) to validate.
    dest_dir : str or Path
        The destination directory for extraction.

    Raises
    ------
    MsticpyUserError
        If the link target escapes the destination directory.

    """
    dest = Path(dest_dir).resolve()
    link_target = member.linkname
    if Path(link_target).is_absolute():
        raise MsticpyUserError(
            "Archive contains a link with an absolute"
            f" target: '{member.name}'"
            f" -> '{link_target}'.",
            "This may indicate a malicious archive (path traversal attack).",
            title="Unsafe archive link target",
        )
    # Resolve link target relative to the member's directory
    member_dir = (dest / member.name).resolve().parent
    resolved_link = (member_dir / link_target).resolve()
    if not (
        resolved_link == dest
        or str(resolved_link).startswith(str(dest) + "\\")
        or str(resolved_link).startswith(str(dest) + "/")
    ):
        raise MsticpyUserError(
            "Archive contains a link that escapes the"
            f" destination: '{member.name}'"
            f" -> '{link_target}'.",
            f"Resolved link target '{resolved_link}' is outside '{dest}'.",
            "This may indicate a malicious archive (path traversal attack).",
            title="Unsafe archive link target",
        )
