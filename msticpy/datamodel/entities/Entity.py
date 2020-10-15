# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Entity Entity class."""
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
class Entity(ABC):
    """
    Entity abstract base class.

    Implements common methods for Entity classes
    """

    ENTITY_NAME_MAP: Dict[str, type] = {}
    _entity_schema: Dict[str, Any] = {}

    def __init__(self, src_entity: Mapping[str, Any] = None, **kwargs):
        """
        Create a new instance of an entity.

        Parameters
        ----------
        src_entity : Mapping[str, Any], optional
            If src_entity is supplied it attempts to extract common
            properties from the source entity and assign them to
            the new instance. (the default is None)

        Other Parameters
        ----------------
        kwargs : Dict[str, Any]
            Supply the entity properties as a set of
            kw arguments.

        """
        self.Type = type(self).__name__.lower()
        # If we have an unknown entity see if we a type passed in
        if self.Type == "unknownentity" and "Type" in kwargs:
            self.Type = kwargs["Type"]
        # Make sure Type is in the class schema dictionary
        self._entity_schema["Type"] = None

        # if we didn't populate AdditionalData, add an empty dict in case it's
        # needed
        if "AdditionalData" not in self:
            self["AdditionalData"] = {}

        if src_entity is not None:
            self._extract_src_entity(src_entity)
            # add AdditionalData dictionary if it's populated
            if "AdditionalData" in src_entity:
                self["AdditionalData"] = src_entity["AdditionalData"]

        if kwargs:
            self.__dict__.update(kwargs)

    def _extract_src_entity(self, src_entity: Mapping[str, Any]):
        """
        Extract source entity properties.

        Parameters
        ----------
        src_entity : Mapping[str, Any]
            The source mappable object from which to
            extract entity properties.

        """
        schema_dict = dict(**(self._entity_schema))
        schema_dict["Type"] = None
        for k, v in schema_dict.items():
            if k not in src_entity:
                continue
            self[k] = src_entity[k]

            if v is not None:
                try:
                    # If the property is an enum
                    if v in _ENTITY_ENUMS:
                        self[k] = _ENTITY_ENUMS[v][src_entity[k]]
                        continue
                except KeyError:
                    # Catch key errors from invalid enum values
                    self[k] = None

                if isinstance(v, tuple):
                    # if the property is a collection
                    entity_list = []
                    for col_entity in src_entity[k]:
                        entity_list.append(Entity.instantiate_entity(col_entity))
                    self[k] = entity_list
                else:
                    # else try to instantiate an entity
                    self[k] = Entity.instantiate_entity(src_entity[k])

    def __getitem__(self, key: str):
        """Allow property get using dictionary key syntax."""
        if key in self.__dict__:
            return self.__dict__[key]
        if key in self._entity_schema:
            return None
        raise KeyError

    def __setitem__(self, key: str, value: Any):
        """Allow property set using dictionary key syntax."""
        self.__dict__[key] = value

    def __contains__(self, key: str):
        """Allow property in test."""
        # In operator overload
        return key in self.__dict__

    def __getattr__(self, name: str):
        """Return the value of the named property 'name'."""
        if name in self._entity_schema:
            return None
        raise AttributeError(f"{name} is not a valid attribute.")

    def __iter__(self):
        """Iterate over entity_properties."""
        return iter(self.properties)

    def __len__(self) -> int:
        """Return length/number of entity_properties."""
        return len(self.properties)

    def __str__(self) -> str:
        """Return string representation of entity."""
        return pprint.pformat(self._to_dict(self), indent=2, width=100)

    def __repr__(self) -> str:
        """Return repr of entity."""
        params = ", ".join(
            [f"{name}={val}" for name, val in self.properties.items() if val]
        )
        if len(params) > 80:
            params = params[:80] + "..."
        return f"{self.__class__.__name__}({params})"

    def _to_dict(self, entity) -> dict:
        """Return as simple nested dictionary."""
        return {
            prop: self._to_dict(val) if isinstance(val, Entity) else val
            for prop, val in entity.properties.items()
            if val is not None
        }

    def _repr_html_(self) -> str:
        """
        Display entity in IPython/Notebook.

        Returns
        -------
        HTML
            IPython HTML object

        """
        return self.to_html()

    def to_html(self) -> str:
        """
        Return HTML representation of entity.

        Returns
        -------
        str
            HTML representation of entity

        """
        e_text = str(self)
        e_type = self.Type
        e_text = e_text.replace("\n", "<br>").replace(" ", "&nbsp;")
        return f"<h3>{e_type}</h3>{e_text}"

    @property
    def properties(self) -> dict:
        """
        Return dictionary properties of entity.

        Returns
        -------
        dict
            Entity properties.

        """
        return {
            name: value
            for name, value in self.__dict__.items()
            if not name.startswith("_")
        }

    @property
    @abstractmethod
    def description_str(self) -> str:
        """
        Return Entity Description.

        Returns
        -------
        str
            Entity description (optional). If not overridden
            by the Entity instance type, it will return the
            Type string.

        """
        return self.Type

    # pylint: disable=too-many-branches
    @classmethod
    def instantiate_entity(  # noqa: C901
        cls, raw_entity: Mapping[str, Any]
    ) -> Union["Entity", Mapping[str, Any]]:
        """
        Class factory to return entity from raw dictionary representation.

        Parameters
        ----------
        raw_entity : Mapping[str, Any]
            A mapping object (e.g. dictionary or pandas Series)
            that contains the properties of the entity.

        Returns
        -------
        Entity
            The instantiated entity

        """
        if "Type" not in raw_entity:
            return raw_entity

        entity_type = raw_entity["Type"]

        # We get an undefined-variable warning here. _ENTITY_NAME_MAP
        # is not defined/populated until end of module since it needs
        # entity
        if entity_type in cls.ENTITY_NAME_MAP:
            return cls.ENTITY_NAME_MAP[entity_type](raw_entity)

        raise TypeError("Could not find a suitable type for {}".format(entity_type))
