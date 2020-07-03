# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Msticpy Exception test class."""
from contextlib import redirect_stdout
import io
from typing import List, Any, Tuple

import pytest
import pytest_check as check
from pytest import raises

# pylint: disable=relative-beyond-top-level, redefined-outer-name
from msticpy.common.exceptions import (
    MsticpyException,
    MsticpyAzureConfigError,
    MsticpyConfigException,
    MsticpyKeyVaultConfigError,
    MsticpyKeyVaultMissingSecretError,
    MsticpyNoDataSourceError,
    MsticpyNotConnectedError,
    MsticpyResourceException,
    MsticpyUserConfigError,
    MsticpyUserError,
)

BASE_EX_CASES: List[Any] = [
    MsticpyException,
    MsticpyConfigException,
    MsticpyResourceException,
]

USER_EX_CASES: List[Any] = [
    MsticpyAzureConfigError,
    MsticpyKeyVaultConfigError,
    MsticpyKeyVaultMissingSecretError,
    MsticpyNoDataSourceError,
    MsticpyNotConnectedError,
    MsticpyUserConfigError,
    MsticpyUserError,
]

_TEST_ARG = "test arg"
_TEST_URI = "https://msticpy.readthedocs.org/test"
_OTHER_URI = "https://msticpy.readthedocs.org/test2"
_TEST_TITLE = "test error"

_TEST_EX_CASES: List[Tuple] = []
for case in USER_EX_CASES:
    tst_kwargs = dict(help_uri=_TEST_URI, title=_TEST_TITLE, other_uri=_OTHER_URI)
    _TEST_EX_CASES.append((case, [_TEST_ARG], tst_kwargs))


# pylint: disable=protected-access
def _create_and_capture_exception(ex_cls, *args, **kwargs):
    f_stream = io.StringIO()
    ex_inst = None
    ex_html = None
    with redirect_stdout(f_stream):
        ex_inst = ex_cls(*args, **kwargs)
        ex_html = ex_inst._repr_html_()
    return ex_inst, str(f_stream.getvalue()), ex_html


@pytest.fixture(params=_TEST_EX_CASES, ids=lambda t: t[0].__name__)
def get_except_cases(request):
    """Pytest fixture for parameterized tests."""
    return request.param


def test_user_exceptions(get_except_cases):
    """Test user exceptions with messages to std out."""
    ex_cls, ex_args, ex_kwargs = get_except_cases
    with raises(ex_cls) as ctxt:
        ex, stdout_txt, html = _create_and_capture_exception(
            ex_cls, *ex_args, **ex_kwargs
        )
        for expected_item in [_TEST_URI, _TEST_TITLE, _TEST_ARG, _OTHER_URI]:
            check.is_in(expected_item, stdout_txt)
            check.is_in(expected_item, html)
        raise ex

    for expected_item in [_TEST_URI, _TEST_TITLE, _TEST_ARG, _OTHER_URI]:
        check.is_in(expected_item, ctxt.value.args)


@pytest.mark.parametrize("test_ex", BASE_EX_CASES)
def test_base_exceptions(test_ex):
    """Test simple MP Exceptions."""
    with raises(test_ex) as ctxt:
        raise test_ex(_TEST_ARG)
    check.is_in(_TEST_ARG, ctxt.value.args)
