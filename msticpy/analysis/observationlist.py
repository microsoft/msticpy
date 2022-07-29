# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Observation summary collector."""
from collections import OrderedDict
from datetime import datetime
from typing import Any, Dict, Iterator, List, Mapping, Optional, Set, Tuple

import attr
import pandas as pd
from attr import Factory
from IPython.display import Markdown, display

from .._version import VERSION
from ..common.timespan import TimeSpan

__version__ = VERSION
__author__ = "Ian Hellen"


# Query definition
@attr.s(auto_attribs=True)
class Observation:
    """
    Observation definition.

    Notes
    -----
    caption : str
        The title and index of the observation. Must
        be unique in the observation set.
    description : Optional[str]
        Text description of the observation.
        (default is None)
    data : Any
        The data to be stored for the observation (e.g.
        a pandas DataFrame). The object should implement
        a useable __repr__ to display correctly.
    data_type : Optional[str]
        The data type of the `data` property
    link : Optional[str]
        Link (usually a document-local link) to the
        originating section of the notebook.
        (default is None)
    score : int
        The risk score associated with the observation.
        (default is 0)
    tags : List[str]
        Optional list of tags.
    additional_properties Dict[str, Any]
        Additional properties not covered
        by core properties.

    """

    caption: str
    data: Any
    description: Optional[str] = None
    data_type: Optional[str] = None
    link: Optional[str] = None
    score: int = 0
    tags: List[str] = Factory(list)
    additional_properties: Dict[str, Any] = Factory(dict)
    timestamp: Optional[datetime] = None
    time_span: Optional[TimeSpan] = None
    time_column: Optional[str] = None
    filter: Optional[str] = None
    schema: Optional[str] = None

    @classmethod
    def required_fields(cls) -> List[str]:
        """
        Return required fields for Observation instance.

        Returns
        -------
        List[str]
            List of field names.

        """
        return ["caption", "data"]

    @classmethod
    def all_fields(cls) -> Set[str]:
        """
        Return all fields of Observation class.

        Returns
        -------
        Set[str]
            Set of all field names.

        """
        return {field.name for field in attr.fields(cls)}

    def display(self):
        """Display the observation."""
        display(Markdown(f"### {self.caption}"))
        if self.description:
            display(Markdown(self.description))
        if self.score:
            display(Markdown(f"Score: {self.score}"))
        if self.link:
            display(Markdown(f"[Go to details](#{self.link})"))
        if self.tags:
            display(Markdown(f'tags: {", ".join(self.tags)}'))
        display(self.filtered_data)
        if self.additional_properties:
            display(Markdown("### Additional Properties"))
            # pylint: disable=no-member
            for key, val in self.additional_properties.items():
                display(Markdown(f"**{key}**: {val}"))
            # pylint: enable=no-member

    @property
    def filtered_data(self) -> Any:
        """Apply filtering to data if it is a DataFrame."""
        if not isinstance(self.data, pd.DataFrame):
            return self.data
        filtered_data = self.data
        if self.filter:
            filtered_data = filtered_data.query(self.filter)
        if self.time_span and self.time_column:
            filtered_data = filtered_data[
                (filtered_data[self.time_column] >= self.time_span.start)
                & (filtered_data[self.time_column] <= self.time_span.end)
            ]
        return filtered_data


class Observations:
    """Class to collect and display investigation observations."""

    def __init__(self, observationlist: "Observations" = None):
        """
        Create an observation list.

        Parameters
        ----------
        observationlist : Observations, optional
            Initialize from an existing Observations list
            (the default is None)

        """
        self.observation_list: Dict[str, Observation] = OrderedDict()
        if observationlist is not None:
            self.observation_list.update(observationlist.observations)

    def __getitem__(self, key: str) -> Observation:
        """Return the observation with a caption."""
        return self.observation_list[key]

    def __iter__(self) -> Iterator[Tuple[str, Observation]]:
        """Return iterator over observations."""
        yield from self.observation_list.items()

    @property
    def observations(self) -> Mapping[str, Observation]:
        """
        Return the current list of Observations.

        Returns
        -------
        Mapping[str, Observation]
            The current ordered dictionary of Observations

        """
        return self.observation_list

    def display_observations(self):
        """Display the current observations using IPython.display."""
        for observation in self.observation_list.values():
            display(observation)

    def add_observation(self, observation: Observation = None, **kwargs):
        """
        Add an observation.

        Add an observation as an Observation instance or as a
        set of keyword parameters (see Observation class for
        acceptable values). Any keyword parameters that are not
        properties of Observation will be stored in the
        Observation.additional_properties dictionary

        Parameters
        ----------
        observation : Observation
            An observation instance.

        Other Parameters
        ----------------
        kwargs : str, Any
            List of key value pairs of the property names
            and values of the Observation to be stored.

        """
        if observation is not None:
            self.observation_list[observation.caption] = observation
        else:
            req_fields = set(Observation.required_fields())
            missing_fields = req_fields.difference(kwargs.keys())
            if missing_fields:
                raise ValueError(
                    "The following fields are required",
                    f"in an Observation: {missing_fields}",
                )

            core_fields = {
                key: value
                for key, value in kwargs.items()
                if key in Observation.all_fields()
            }
            new_observation = Observation(**core_fields)
            addl_fields = {
                key: value
                for key, value in kwargs.items()
                if key not in Observation.all_fields()
            }
            # pylint: disable=no-member
            new_observation.additional_properties.update(addl_fields)
            self.observation_list[new_observation.caption] = new_observation
