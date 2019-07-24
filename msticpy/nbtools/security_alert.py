# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module for SecurityAlert class."""
import json
from json import JSONDecodeError
from typing import Dict, Any, List

import pandas as pd

from .._version import VERSION
from .entityschema import Entity, UnknownEntity
from .security_base import SecurityBase
from .utility import export

__version__ = VERSION
__author__ = "Ian Hellen"


@export
class SecurityAlert(SecurityBase):
    """
    Security Alert Class.

    Instantiates a security alert from a pandas Series and provides
    convenience access methods to retrieve properties.
    """

    def __init__(self, src_row: pd.Series = None):
        """Instantiate a security alert from a pandas Series."""
        super().__init__(src_row=src_row)

        # add entities to dictionary to remove dups
        self._src_entities: Dict[int, Entity] = dict()

        if src_row is not None:
            if "Entities" in src_row:
                self._extract_entities(src_row)

            if "ExtendedProperties" in src_row:
                if isinstance(src_row.ExtendedProperties, dict):
                    self.extended_properties = src_row.ExtendedProperties
                elif isinstance(src_row.ExtendedProperties, str):
                    try:
                        self.extended_properties = json.loads(
                            src_row.ExtendedProperties
                        )
                    except JSONDecodeError:
                        pass
        else:
            self.extended_properties: Dict[str, Any] = {}
        self._find_os_family()

    @property
    def entities(self) -> List[Entity]:
        """Return a list of the Security Alert entities."""
        return list(self._src_entities.values())

    @property
    def query_params(self) -> Dict[str, Any]:
        """
        Query parameters derived from alert.

        Returns
        -------
            Dict[str, Any]
                Dictionary of parameter names/value

        """
        params_dict = super().query_params
        if (
            "system_alert_id" not in params_dict
            or params_dict["system_alert_id"] is None
        ):
            params_dict["system_alert_id"] = self._ids["SystemAlertId"]
        return params_dict

    def to_html(self, show_entities=False) -> str:
        """Return the item as HTML string."""
        if self.properties is not None and not self.properties.empty:
            title = """
            <h3>Alert: '{name}'</h3><br>time=<b>{start}</b>,
            entity=<b>{entity}</b>, id=<b>{id}</b>
            <br/>
            """.format(
                start=self.properties["StartTimeUtc"],
                name=self.properties["AlertDisplayName"],
                entity=self.properties.get("CompromisedEntity", "unknown"),
                id=self.properties["SystemAlertId"],
            )
        else:
            title = "Alert has no data."
        return title + super().to_html(show_entities)

    # Public methods
    def __str__(self):
        """Print the alert contents to stdout."""
        alert_props = str(super().__str__())

        if self.extended_properties:
            str_rep = [
                f"ExtProp: {prop}: {val}"
                for prop, val in self.extended_properties.items()
            ]
            alert_props = alert_props + "\n" + "\n".join(str_rep)

        return alert_props

    # Private methods
    def _resolve_entity_refs(self):
        """
        Resolve and replace entity properties that are '$ref' type.

        When serialized the nested entities can be references to other
        referenced objects in the collection. This iterates through
        the raw entities and replaces referenced items with the actual
        object reference. If the Id referenced by this property exists
        in the entities dictionary we replace the original property
        with a reference to the entity in the dictionary.

        """
        for _, entity in self._src_entities.items():
            if not isinstance(entity, Entity):
                continue
            for prop_name, prop_val in entity.properties.items():
                if isinstance(prop_val, dict) and "$ref" in prop_val:
                    entity_id = prop_val["$ref"]
                    if entity_id in self._src_entities:
                        entity[prop_name] = self._src_entities[entity_id]

    def _find_os_family(self):
        """Discover OSFamily and path separator from entities or file paths."""
        self.path_separator = "\\"
        self.os_family = "Windows"

        # Use OSFamily if any entities have this property set
        os_family_entities = [e for e in self.entities if "OSFamily" in e]
        if os_family_entities:
            for os_entity in os_family_entities:
                if os_entity["OSFamily"] == "Linux":
                    self.os_family = "Linux"
                    self.path_separator = "/"
                    break
        else:
            # Otherwise try to infer from the file paths
            files = [e for e in self.entities if e["Type"] == "file"]
            if files:
                for file in files:
                    if "Directory" in file and "/" in file["Directory"]:
                        self.os_family = "Linux"
                        self.path_separator = "/"
                        break
            else:
                for proc in [
                    e
                    for e in self.entities
                    if e["Type"] == "process" and "ImageFile" in e
                ]:
                    file = proc["ImageFile"]
                    if "Directory" in file and "/" in file["Directory"]:
                        self.os_family = "Linux"
                        self.path_separator = "/"
                        break

    def _extract_entities(self, src_row):
        input_entities = []
        if isinstance(src_row.Entities, str):
            try:
                input_entities = json.loads(src_row["Entities"])
            except json.JSONDecodeError:
                pass
        elif isinstance(src_row.Entities, list):
            input_entities = src_row.Entities

        for ent in input_entities:
            try:
                entity = Entity.instantiate_entity(ent)
            except TypeError:
                # if we didn't instantiate a known entity
                # just add it as it is
                entity = UnknownEntity(**ent)
            if "$id" in ent:
                self._src_entities[ent["$id"]] = entity

        self._resolve_entity_refs()
