# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Pivot pipeline tests."""
import pytest_check as check
import yaml

from msticpy.init.pivot_core.pivot_pipeline import Pipeline

# pylint: disable=redefined-outer-name, unused-import, unused-argument
from .pivot_fixtures import create_data_providers, create_pivot, data_providers

__author__ = "Ian Hellen"

# pylint: disable=redefined-outer-name


# @pytest.fixture(scope="session")
# def create_pivot():
#     with warnings.catch_warnings():
#         warnings.simplefilter("ignore", category=UserWarning)
#         return Pivot()


_EXPECTED_OUTPUT = """# Pipeline 1 description
(
    input_df
    # Standard pivot function
    .mp_pivot.run(IpAddress.util.whois, column='IpAddress', join='inner')
    # Pivot display
    .mp_pivot.display(title='The title', query='Computer.str.startswith('MSTICAlerts')', cols=['Computer', 'Account'], head=10)
    # Pivot tee
    .mp_pivot.tee(var_name='var_df', clobber=True)
    # Pivot tee_exec with mp_plot.timeline
    .mp_pivot.tee_exec('mp_plot.timeline', source_columns=['Computer', 'Account'])
    # Standard accessor with mp_plot.timeline
    .mp_plot.timeline('one', 2, source_columns=['Computer', 'Account'])
)"""


def test_pipeline_objects(create_pivot):
    """Test parse pipeline."""
    pipelines = list(Pipeline.from_yaml(_TEST_PIPELINES))
    check.equal(len(pipelines), 2)

    check.equal(pipelines[0].name, "pipeline1")
    check.equal(pipelines[0].description, "Pipeline 1 description")

    check.equal(len(pipelines[0].steps), 5)
    for step in pipelines[0].steps:
        step_type = step.step_type
        check.is_not_none(step.name)
        check.is_not_none(step.comment)
        check.is_not_none(step.params)
        if step_type in ("pivot", "pivot_tee_exec", "pd_accessor"):
            check.is_not_none(step.function)
        if step_type == "pivot":
            check.is_not_none(step.entity)

    pl_repr = repr(pipelines[0])
    check.is_in("Pipeline(name='pipeline1'", pl_repr)
    check.is_in("steps=[PipelineStep(name='get_logons", pl_repr)

    pl_txt = pipelines[0].print_pipeline(df_name="input_df")
    check.equal(pl_txt, _EXPECTED_OUTPUT)
    pl_txt = pipelines[0].print_pipeline(df_name="input_df", comments=False)
    exp_no_comments = "\n".join(
        nc_line
        for nc_line in _EXPECTED_OUTPUT.split("\n")
        if not nc_line.strip().startswith("#")
    )
    check.equal(pl_txt, exp_no_comments)

    pl_single_dict = yaml.safe_load(_TEST_SINGLE_PIPELINE)
    pl_single = Pipeline.parse_pipeline(pl_single_dict)
    check.equal(pl_single.name, "pipeline1")
    check.equal(pl_single.description, "Pipeline 1 description")

    check.equal(len(pl_single.steps), 5)

    # Test to_yaml
    out_yaml = pl_single.to_yaml()
    # The yaml won't be the same since None values will be null
    out_dict = yaml.safe_load(out_yaml)
    # but it should convert into an identical object
    new_pipeline = Pipeline.parse_pipeline(out_dict)
    check.equal(pl_single.name, new_pipeline.name)
    check.equal(pl_single.description, new_pipeline.description)
    for idx, step in enumerate(pl_single.steps):
        check.equal(step, new_pipeline.steps[idx])


_TEST_PIPELINES = """
pipelines:
  pipeline1:
    description: Pipeline 1 description
    steps:
      - name: get_logons
        step_type: pivot
        function: util.whois
        entity: IpAddress
        comment: Standard pivot function
        params:
          column: IpAddress
          join: inner
      - name: disp_logons
        step_type: pivot_display
        comment: Pivot display
        params:
          title: "The title"
          cols:
              - Computer
              - Account
          query: Computer.str.startswith('MSTICAlerts')
          head: 10
      - name: tee_logons
        step_type: pivot_tee
        comment: Pivot tee
        params:
          var_name: var_df
          clobber: True
      - name: tee_logons_disp
        step_type: pivot_tee_exec
        comment: Pivot tee_exec with mp_plot.timeline
        function: mp_plot.timeline
        params:
          source_columns:
              - Computer
              - Account
      - name: logons_timeline
        step_type: pd_accessor
        comment: Standard accessor with mp_plot.timeline
        function: mp_plot.timeline
        pos_params:
          - one
          - 2
        params:
          source_columns:
              - Computer
              - Account
  pipeline2:
    description: Pipeline 2 description
    steps:
      - name: get_logons
        step_type: pivot
        function: util.whois
        entity: IpAddress
        comment: Standard pivot function
        params:
          column: IpAddress
          join: inner
      - name: disp_logons
        step_type: pivot_display
        comment: Pivot display
        params:
          title: "The title"
          cols:
              - Computer
              - Account
          query: Computer.str.startswith('MSTICAlerts')
          head: 10
      - name: tee_logons
        step_type: pivot_tee
        comment: Pivot tee
        params:
          var_name: var_df
          clobber: True
"""

_TEST_SINGLE_PIPELINE = """
pipeline1:
    description: Pipeline 1 description
    steps:
      - name: get_logons
        step_type: pivot
        function: util.whois
        entity: IpAddress
        comment: Standard pivot function
        params:
          column: IpAddress
          join: inner
      - name: disp_logons
        step_type: pivot_display
        comment: Pivot display
        params:
          title: "The title"
          cols:
              - Computer
              - Account
          query: Computer.str.startswith('MSTICAlerts')
          head: 10
      - name: tee_logons
        step_type: pivot_tee
        comment: Pivot tee
        params:
          var_name: var_df
          clobber: True
      - name: tee_logons_disp
        step_type: pivot_tee_exec
        comment: Pivot tee_exec with mp_timeline.plot
        function: mp_timeline.plot
        params:
          source_columns:
              - Computer
              - Account
      - name: logons_timeline
        step_type: pd_accessor
        comment: Standard accessor with mp_timeline.plot
        function: mp_timeline.plot
        params:
          source_columns:
              - Computer
              - Account
"""
