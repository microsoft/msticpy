# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Msticpy Exception test class."""
from typing import Any, List, Tuple

import pytest
import pytest_check as check
from pytest import raises

# pylint: disable=relative-beyond-top-level, redefined-outer-name
from msticpy.common.exceptions import (
    MsticpyAzureConfigError,
    MsticpyConfigError,
    MsticpyConnectionError,
    MsticpyDataQueryError,
    MsticpyException,
    MsticpyImportExtraError,
    MsticpyKeyVaultConfigError,
    MsticpyKeyVaultMissingSecretError,
    MsticpyKqlConnectionError,
    MsticpyMissingDependencyError,
    MsticpyNoDataSourceError,
    MsticpyNotConnectedError,
    MsticpyResourceError,
    MsticpyUserConfigError,
    MsticpyUserError,
)

BASE_EX_CASES: List[Any] = [
    MsticpyException,
    MsticpyConfigError,
    MsticpyResourceError,
]

USER_EX_CASES: List[Any] = [
    (MsticpyKeyVaultConfigError, {}),
    (MsticpyAzureConfigError, {}),
    (MsticpyKeyVaultMissingSecretError, {}),
    (MsticpyNoDataSourceError, {}),
    (MsticpyNotConnectedError, {}),
    (MsticpyUserConfigError, {}),
    (MsticpyUserError, {}),
    (MsticpyConnectionError, {}),
    (MsticpyDataQueryError, {}),
    (MsticpyKqlConnectionError, {}),
    (MsticpyImportExtraError, {"extra": "ml"}),
    (MsticpyMissingDependencyError, {"packages": ["vt_py", "folium"]}),
]

_TEST_ARG = "test arg"
_TEST_URI = "https://msticpy.readthedocs.org/test"
_OTHER_URI = "https://msticpy.readthedocs.org/test2"
_TEST_TITLE = "test error"

_TEST_EX_CASES: List[Tuple] = []
tst_kwargs = dict(help_uri=_TEST_URI, title=_TEST_TITLE, other_uri=_OTHER_URI)
for case, case_kwargs in USER_EX_CASES:
    test_case_kwargs = {**tst_kwargs, **case_kwargs}
    _TEST_EX_CASES.append((case, [_TEST_ARG], test_case_kwargs))


# pylint: disable=protected-access
def _create_and_capture_exception(ex_cls, *args, html_repr=True, **kwargs):
    ex_inst = None
    ex_html = None
    ex_inst = ex_cls(*args, **kwargs)
    ex_text = ex_inst._get_exception_text()
    ex_html = ex_inst._repr_html_() if html_repr else ""
    return ex_inst, ex_text, ex_html


@pytest.fixture(params=_TEST_EX_CASES, ids=lambda t: t[0].__name__)
def get_except_cases(request):
    """Pytest fixture for parameterized tests."""
    return request.param


def test_user_exceptions(get_except_cases):
    """Test user exceptions with messages to std out."""
    ex_cls, ex_args, ex_kwargs = get_except_cases
    with raises(ex_cls):
        ex, ex_text, ex_html = _create_and_capture_exception(
            ex_cls, *ex_args, **ex_kwargs
        )
        for expected_item in [_TEST_URI, _TEST_TITLE, _TEST_ARG, _OTHER_URI]:
            check.is_in(expected_item, ex_text)
            check.is_in(expected_item, ex_html)
        raise ex


@pytest.mark.parametrize("test_ex", BASE_EX_CASES)
def test_base_exceptions(test_ex):
    """Test simple MP Exceptions."""
    with raises(test_ex):
        raise test_ex(_TEST_ARG)


def test_no_display_exceptions(get_except_cases):
    """Test that no exception output is generated if suppressed."""
    ex_cls, ex_args, ex_kwargs = get_except_cases
    with raises(ex_cls):
        with MsticpyUserError.no_display_exceptions():
            ex, stdout_txt, html = _create_and_capture_exception(
                ex_cls, *ex_args, html_repr=False, **ex_kwargs
            )
            for expected_item in [_TEST_URI, _TEST_TITLE, _TEST_ARG, _OTHER_URI]:
                # we can't reliably check for full content since redirect_stdout
                # is a global capture and other things outputting to std_out
                # might truncate or overwrite this.
                check.is_true(stdout_txt)
                check.is_not_in(expected_item, html)
            raise ex
