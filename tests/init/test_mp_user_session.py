# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Unit tests for mp_user_session."""
from unittest.mock import MagicMock, patch

import pytest
import yaml

from msticpy.init.mp_user_session import load_user_session

__author__ = "Ian Hellen"


@pytest.fixture
def mp_session_yaml(tmp_path):
    """
    Create a temporary YAML file for testing.

    Parameters
    ----------
    tmp_path : pathlib.Path
        Temporary directory provided by pytest.

    Returns
    -------
    pathlib.Path
        Path to the temporary YAML file.
    """
    config_data = {
        "QueryProviders": {
            "MyQueryProvider": {
                "DataEnvironment": "MSSentinel",
                "InitArgs": {"debug": True},
                "Connect": True,
                "ConnectArgs": {
                    "workspace": "MySoc",
                    "auth_methods": ["cli", "device_code"],
                },
            }
        },
        "Components": {
            "MyComponent": {
                "Module": "msticpy.context.azure",
                "Class": "MicrosoftSentinel",
                "InitArgs": {"debug": True},
                "Connect": False,
                "ConnectArgs": {
                    "workspace": "MySoc",
                    "auth_methods": ["cli", "device_code"],
                },
            }
        },
    }
    yaml_file = tmp_path / "test_config.yaml"
    with open(yaml_file, "w") as file:
        yaml.dump(config_data, file)
    return yaml_file


@patch("msticpy.init.mp_user_session.QueryProvider", autospec=True)
@patch("msticpy.init.mp_user_session._load_mp_components")
def test_load_query_provider(mock_load_components, mock_query_prov, mp_session_yaml):
    """
    Test load_user_session function.

    Parameters
    ----------
    mock_load_components : MagicMock
        Mocked _load_mp_components function.
    mock_load_providers : MagicMock
        Mocked _load_query_providers function.
    tmp_yaml_file : pathlib.Path
        Path to the temporary YAML file.

    Returns
    -------
    None
    """
    namespace = {}
    load_user_session(session_file=mp_session_yaml, namespace=namespace)

    mock_load_components.assert_called_once()
    assert "MyQueryProvider" in namespace
    qp_instance = namespace["MyQueryProvider"]
    mock_query_prov.assert_called_with("MSSentinel", debug=True)
    qp_instance.connect.assert_called_once()
    qp_instance.connect.assert_called_once_with(
        workspace="MySoc", auth_methods=["cli", "device_code"]
    )


@patch("msticpy.init.mp_user_session.QueryProvider", autospec=True)
@patch("msticpy.init.mp_user_session.importlib", autospec=True)
def test_load_component(mock_import_lib, mock_query_prov, mp_session_yaml):
    """
    Test load_user_session function.

    Parameters
    ----------
    mock_load_components : MagicMock
        Mocked _load_mp_components function.
    mock_load_providers : MagicMock
        Mocked _load_query_providers function.
    tmp_yaml_file : pathlib.Path
        Path to the temporary YAML file.

    Returns
    -------
    None
    """
    mock_module = MagicMock()
    mock_module.name = "msticpy.context.azure"
    mock_component = MagicMock()
    mock_component.name = "MicrosoftSentinel"
    mock_module.MicrosoftSentinel = mock_component
    mock_comp_instance = MagicMock()
    mock_comp_instance.name = "MyComponent"
    mock_component.return_value = mock_comp_instance
    mock_import_lib.import_module.return_value = mock_module
    namespace = {}
    load_user_session(session_file=mp_session_yaml, namespace=namespace)

    mock_import_lib.import_module.assert_called_once()
    assert "MyComponent" in namespace
    comp_instance = namespace["MyComponent"]
    assert comp_instance == mock_component.return_value
    mock_component.assert_called_with(debug=True)
    comp_instance.connect.assert_not_called()


@patch("msticpy.init.mp_user_session.get_ipython")
@patch("msticpy.init.mp_user_session._initialize_component")
def test_load_user_session_no_namespace(
    mock_initialize_component, mock_get_ipython, mp_session_yaml
):
    """
    Test load_user_session function with no namespace provided.

    Parameters
    ----------
    mock_load_components : MagicMock
        Mocked _load_mp_components function.
    mock_load_providers : MagicMock
        Mocked _load_query_providers function.
    mock_get_ipython : MagicMock
        Mocked get_ipython function.
    tmp_yaml_file : pathlib.Path
        Path to the temporary YAML file.

    Returns
    -------
    None
    """
    mock_ipython = MagicMock()
    mock_get_ipython.return_value = mock_ipython
    mock_ipython.user_ns = {}
    mock_initialize_component.return_value = MagicMock()

    load_user_session(session_file=mp_session_yaml)

    assert mock_initialize_component.call_count == 2
    assert "MyQueryProvider" in mock_ipython.user_ns
    assert "MyComponent" in mock_ipython.user_ns


@patch("msticpy.init.mp_user_session.get_ipython", return_value=None)
@patch("msticpy.init.mp_user_session._load_query_providers")
def test_load_user_session_no_ipython(
    mock_load_providers, mock_get_ipython, mp_session_yaml
):
    """
    Test that the function prints an error message when no IPython session is found.

    Parameters
    ----------
    mock_get_ipython : unittest.mock.Mock
        Mock for get_ipython function.
    mock_print : unittest.mock.Mock
        Mock for builtins.print function.
    """
    local_var = 1
    load_user_session(mp_session_yaml)
    mock_get_ipython.assert_called_once()
    mock_load_providers.assert_called_once()
    namespace = mock_load_providers.call_args[0][1]
    assert namespace["local_var"] == local_var


@patch("msticpy.init.mp_user_session.get_ipython")
@patch("msticpy.init.mp_user_session._load_query_providers")
@patch("msticpy.init.mp_user_session._load_mp_components")
def test_load_user_session_loads_config(
    mock_load_components,
    mock_load_providers,
    mock_get_ipython,
    mp_session_yaml,
):
    """
    Test that the function loads the configuration when the session file exists.

    Parameters
    ----------
    mock_load_components : unittest.mock.Mock
        Mock for _load_mp_components function.
    mock_load_providers : unittest.mock.Mock
        Mock for _load_query_providers function.
    mock_get_ipython : unittest.mock.Mock
        Mock for get_ipython function.
    mp_session_yaml : pathlib.Path
        Path to the temporary YAML file.

    """
    mock_ipython = MagicMock()
    mock_get_ipython.return_value = mock_ipython
    mock_ipython.user_ns = {}

    load_user_session(mp_session_yaml)

    mock_load_providers.assert_called_once()
    mock_load_components.assert_called_once()
    user_config = mock_load_providers.call_args[0][0]
    assert "QueryProviders" in user_config
    assert "Components" in user_config


@patch("msticpy.init.mp_user_session.Path.read_text")
def test_load_user_session_bad_missing_file(mock_path_read_text, mp_session_yaml):
    """
    Test that the function handles invalid YAML content gracefully.

    Parameters
    ----------
    mock_path_read_text : unittest.mock.Mock
        Mock for Path().read_text method.

    """
    with pytest.raises(FileNotFoundError):
        load_user_session("non_existent_file.yaml")

    invalid_yaml = "QueryProviders: ["
    mock_path_read_text.return_value = invalid_yaml

    with pytest.raises(yaml.YAMLError):
        load_user_session(mp_session_yaml)


@patch("msticpy.init.mp_user_session.logger")
@patch("msticpy.init.mp_user_session.QueryProvider", autospec=True)
@patch("msticpy.init.mp_user_session._load_mp_components")
def test_query_provider_instantiate_fail(
    mock_load_components, mock_query_prov, mock_logger, mp_session_yaml
):
    """
    Test load_user_session function.

    Parameters
    ----------
    mock_load_components : MagicMock
        Mocked _load_mp_components function.
    mock_load_providers : MagicMock
        Mocked _load_query_providers function.
    mock_logger : MagicMock
        Mocked logger.
    tmp_yaml_file : pathlib.Path
        Path to the temporary YAML file.

    Returns
    -------
    None
    """
    namespace = {}
    mock_query_prov.side_effect = Exception("Failed to instantiate QueryProvider")

    load_user_session(session_file=mp_session_yaml, namespace=namespace)

    mock_logger.exception.assert_called_with(
        "Failed to initialize %s", "MyQueryProvider", exc_info=True
    )
    mock_load_components.assert_called_once()
    mock_query_prov.assert_called_with("MSSentinel", debug=True)
    assert "MyQueryProvider" not in namespace

    # Fail at connect call
    mock_logger.reset_mock()
    mock_query_prov.side_effect = None
    mock_qp_instance = MagicMock()
    mock_query_prov.return_value = mock_qp_instance
    mock_qp_instance.connect.side_effect = Exception("Failed to connect QueryProvider")
    load_user_session(session_file=mp_session_yaml, namespace=namespace)
    mock_query_prov.assert_called_with("MSSentinel", debug=True)
    mock_qp_instance.connect.assert_called_once()
    mock_qp_instance.connect.assert_called_once_with(
        workspace="MySoc", auth_methods=["cli", "device_code"]
    )
    mock_logger.exception.assert_called_with(
        "Failed to connect to %s", "MyQueryProvider", exc_info=True
    )
    assert "MyQueryProvider" not in namespace
