# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Unit test class for Observations class."""
import os

# imports
import pytest
import pytest_check as check

from msticpy._version import VERSION
from msticpy.analysis.observationlist import Observation, Observations

__version__ = VERSION
__author__ = "Ian Hellen"

_test_data_folders = [
    d for d, _, _ in os.walk(os.getcwd()) if d.endswith("/tests/testdata")
]
if len(_test_data_folders) == 1:
    _TEST_DATA = _test_data_folders[0]
else:
    _TEST_DATA = "./tests/testdata"


_TEST_FIELDS = {
    "additional_properties",
    "caption",
    "data_type",
    "data",
    "description",
    "filter",
    "link",
    "schema",
    "score",
    "tags",
    "time_column",
    "time_span",
    "timestamp",
}


# pylint: disable=unsupported-assignment-operation
def test_create_observation_class():
    """Test creating observations instance."""
    check.is_in("caption", Observation.required_fields())
    check.is_in("data", Observation.required_fields())

    fields_set = set(Observation.all_fields())

    check.equal(len(fields_set.symmetric_difference(_TEST_FIELDS)), 0)

    # Shouldn't be able to create an observation without required
    # properties
    with pytest.raises(TypeError):
        Observation()

    obs = Observation(caption="My caption", data=fields_set)

    ob1 = Observation(
        caption="O1",
        description="Description1",
        data=obs,
        data_type="object",
        tags=["t1", "t2", "t3"],
    )
    ob1.additional_properties["foo_field"] = "xyz"
    check.equal(ob1.caption, "O1")
    check.equal(ob1.description, "Description1")
    check.equal(ob1.data, obs)
    check.equal(ob1.data_type, "object")
    check.equal(ob1.tags, ["t1", "t2", "t3"])
    check.is_in("foo_field", ob1.additional_properties)
    check.equal("xyz", ob1.additional_properties["foo_field"])


def test_create_observations():
    """Test adding observations."""
    obs_list = Observations()

    obs = Observation(caption="My caption", data=_TEST_FIELDS)

    ob1 = Observation(
        caption="O1",
        description="Description1",
        data=_TEST_FIELDS,
        data_type="object",
        tags=["t1", "t2", "t3"],
    )
    obs_list.add_observation(obs)
    obs_list.add_observation(ob1)
    check.equal(len(obs_list.observations), 2)

    # adding another with same caption should overwrite
    ob2 = Observation(
        caption="O1",
        description="Description2",
        data=_TEST_FIELDS,
        data_type="object",
        tags=["t1", "t2", "t3"],
    )
    obs_list.add_observation(ob2)
    check.equal(len(obs_list.observations), 2)
    check.equal(obs_list.observations["O1"].description, "Description2")
    check.equal(obs_list.observations["O1"], ob2)

    # Add observation with keywords
    obs_list.add_observation(
        caption="O3",
        description="Description1",
        data=_TEST_FIELDS,
        data_type="object",
        tags=["t1", "t2", "t3"],
        foo="some extra data",
    )
    check.equal(len(obs_list.observations), 3)
    check.equal(
        obs_list.observations["O3"].additional_properties["foo"], "some extra data"
    )

    obs_list.display_observations()
