# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""File Entity class."""
import pprint
from abc import ABC, abstractmethod
from enum import Enum
from ipaddress import IPv4Address, IPv6Address, ip_address
from typing import Any, Dict, Mapping, Type, Union, Optional

from ..._version import VERSION
from ...common.utility import export
from .entity import Entity

__version__ = VERSION
__author__ = "Ian Hellen"


_ENTITY_ENUMS: Dict[str, Type] = {}


@export
class File(Entity):
    """
    File Entity class.

    Attributes
    ----------
    FullPath : str
        File FullPath
    Directory : str
        File Directory
    Name : str
        File Name
    Md5 : str
        File Md5
    Host : str
        File Host
    Sha1 : str
        File Sha1
    Sha256 : str
        File Sha256
    Sha256Ac : str
        File Sha256Ac
    FileHashes : List[FileHash]
        File FileHashes

    """

    def __init__(
        self,
        src_entity: Mapping[str, Any] = None,
        src_event: Mapping[str, Any] = None,
        role: str = "new",
        **kwargs,
    ):
        """
        Create a new instance of the entity type.

        Parameters
        ----------
        src_entity : Mapping[str, Any], optional
            Create entity from existing entity or
            other mapping object that implements entity properties.
            (the default is None)
        src_event : Mapping[str, Any], optional
            Create entity from event properties
            (the default is None)
        role : str, optional
            'new' or 'parent' - only relevant if the entity
            is being constructed from an event.
            (the default is 'new')

        Other Parameters
        ----------------
        kwargs : Dict[str, Any]
            Supply the entity properties as a set of
            kw arguments.

        """
        super().__init__(src_entity=src_entity, **kwargs)

        if src_event is not None:
            if role == "new" and "NewProcessName" in src_event:
                self._add_paths(src_event["NewProcessName"])
            elif role == "parent" and "ParentProcessName" in src_event:
                self._add_paths(src_event["ParentProcessName"])

        if "FullPath" not in self:
            file = self["Name"]
            directory = self["Directory"]
            sep = self.path_separator if directory else None
            self["FullPath"] = f"{directory}{sep}{file}"

    @property
    def path_separator(self):
        """Return the path separator used by the file."""
        directory = self["Directory"]
        if directory and "/" in directory:
            return "/"
        return "\\"

    @property
    def description_str(self) -> str:
        """Return Entity Description."""
        return self.FullPath

    _entity_schema = {
        # FullPath (type System.String)
        "FullPath": None,
        # Directory (type System.String)
        "Directory": None,
        # Name (type System.String)
        "Name": None,
        # Md5 (type System.String)
        "Md5": None,
        # Host (type Microsoft.Azure.Security.Detection
        # .AlertContracts.V3.Entities.Host)
        "Host": None,
        # Sha1 (type System.String)
        "Sha1": None,
        # Sha256 (type System.String)
        "Sha256": None,
        # Sha256Ac (type System.String)
        "Sha256Ac": None,
        "FileHashes": (list, "FileHash"),
    }

    def _add_paths(self, full_path):
        if "/" in full_path:
            self.PathSeparator = "/"
            self.OSFamily = OSFamily.Linux
        else:
            self.PathSeparator = "\\"
            self.OSFamily = OSFamily.Windows

        self.FullPath = full_path
        self.Name = full_path.split(self.PathSeparator)[-1]
        self.Directory = full_path.split(self.PathSeparator)[:-1]

    @property
    def file_hash(self) -> str:
        """
        Return the first defined file hash.

        Returns
        -------
        str
            Returns first-defined file hash in order of
            SHA256, SHA1, MD5, SHA256AC (authenticode)
        """

        def _get_hash_of_type(file_hashes, alg):
            return any(hash for hash in file_hashes if hash.Algorithm == alg)

        alg_order = (
            Algorithm.Sha256,
            Algorithm.Sha1,
            Algorithm.Md5,
            Algorithm.Sha256Ac,
        )
        file_hashes = getattr(self, "FileHashes")

        if file_hashes:
            for alg in alg_order:
                hash = _get_hash_of_type(file_hashes, alg)
                if hash:
                    return hash

        return self.Sha256 or self.Sha1 or self.Md5 or self.Sha256Ac
