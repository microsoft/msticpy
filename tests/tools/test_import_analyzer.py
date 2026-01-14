# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Unit tests for import_analyzer module."""

from __future__ import annotations

import sys
from collections import defaultdict
from pathlib import Path
from unittest.mock import MagicMock, mock_open, patch

import pytest

from tools.toollib.import_analyzer import (
    ModuleImports,
    _analyze_module_imports,
    _analyze_module_local_imports,
    _check_std_lib,
    _check_stdlib_path,
    _create_module_reverse_mapping,
    _extract_pkg_specs,
    _filter_exclusions,
    _filter_none_local_imports,
    _get_pkg_from_path,
    _get_pkg_module_paths,
    _get_pkg_modules,
    _match_pkg_to_reqs,
    _remap_partial_module_names,
    analyze_imports,
    analyze_local_imports,
    get_setup_reqs,
)


class TestModuleImports:
    """Tests for ModuleImports class."""

    def test_module_imports_initialization(self):
        """Test ModuleImports class initializes with empty sets."""
        mod_imports = ModuleImports()

        assert isinstance(mod_imports.internal, set)
        assert isinstance(mod_imports.external, set)
        assert isinstance(mod_imports.standard, set)
        assert isinstance(mod_imports.setup_reqs, set)
        assert isinstance(mod_imports.missing_reqs, set)
        assert isinstance(mod_imports.unknown, set)
        assert len(mod_imports.internal) == 0
        assert len(mod_imports.external) == 0


class TestExtractPkgSpecs:
    """Tests for _extract_pkg_specs function."""

    def test_extract_pkg_specs_simple(self):
        """Test extracting simple package specifications."""
        pkg_specs = ["requests>=2.28.0", "pandas>=1.4.0,<3.0.0"]
        result = _extract_pkg_specs(pkg_specs)

        assert len(result) == 2
        req_names = {req.name for req in result}
        assert "requests" in req_names
        assert "pandas" in req_names

    def test_extract_pkg_specs_with_comments(self):
        """Test extracting specs with inline comments."""
        pkg_specs = [
            "requests>=2.28.0  # HTTP library",
            "# This is a comment line",
            "pandas>=1.4.0",
            "",
        ]
        result = _extract_pkg_specs(pkg_specs)

        assert len(result) == 2
        req_names = {req.name for req in result}
        assert "requests" in req_names
        assert "pandas" in req_names

    def test_extract_pkg_specs_with_extras(self):
        """Test extracting specs with extras."""
        pkg_specs = ["ipywidgets[widgetsnbextension]>=7.4.2"]
        result = _extract_pkg_specs(pkg_specs)

        assert len(result) == 1
        req = list(result)[0]
        assert req.name == "ipywidgets"


class TestGetPkgFromPath:
    """Tests for _get_pkg_from_path function."""

    def test_get_pkg_from_path_simple(self):
        """Test getting module name from simple path."""
        pkg_file = "e:/src/msticpy/msticpy/common/utils.py"
        pkg_root = "e:/src/msticpy/msticpy"

        result = list(_get_pkg_from_path(pkg_file, pkg_root))

        assert "utils" in result
        assert "common.utils" in result

    def test_get_pkg_from_path_init(self):
        """Test getting module name from __init__.py path."""
        pkg_file = "e:/src/msticpy/msticpy/common/__init__.py"
        pkg_root = "e:/src/msticpy/msticpy"

        result = list(_get_pkg_from_path(pkg_file, pkg_root))

        assert "__init__" in result
        assert "common.__init__" in result


class TestGetPkgModules:
    """Tests for _get_pkg_modules function."""

    @patch("tools.toollib.import_analyzer._get_pkg_from_path")
    def test_get_pkg_modules(self, mock_get_pkg):
        """Test getting all package modules."""
        # Mock _get_pkg_from_path to return module name variants
        mock_get_pkg.return_value = ["utils", "common.utils"]

        pkg_root = Path("e:/src/msticpy/msticpy")

        with patch.object(Path, "glob") as mock_glob:
            mock_file1 = MagicMock(spec=Path)
            mock_file1.name = "utils.py"

            mock_file2 = MagicMock(spec=Path)
            mock_file2.name = "__init__.py"

            mock_glob.return_value = [mock_file1, mock_file2]

            result = _get_pkg_modules(pkg_root)

        assert isinstance(result, set)
        assert len(result) > 0


class TestMatchPkgToReqs:
    """Tests for _match_pkg_to_reqs function."""

    def test_match_pkg_to_reqs_direct_match(self):
        """Test matching imports to requirements with direct match."""
        imports = {"pandas", "requests"}
        setup_reqs = {"pandas": "pandas", "requests": "requests"}

        req_libs, req_missing = _match_pkg_to_reqs(imports, setup_reqs)

        assert "pandas" in req_libs
        assert "requests" in req_libs
        assert len(req_missing) == 0

    def test_match_pkg_to_reqs_partial_match(self):
        """Test matching imports with partial package names."""
        imports = {"azure.mgmt.resource"}
        setup_reqs = {"azure": "azure-core", "azure.mgmt": "azure-mgmt-core"}

        req_libs, req_missing = _match_pkg_to_reqs(imports, setup_reqs)

        # The function matches on the first matching prefix ('azure')
        assert "azure-core" in req_libs

    def test_match_pkg_to_reqs_missing(self):
        """Test matching imports with missing requirements."""
        imports = {"unknown_package"}
        setup_reqs = {"pandas": "pandas"}

        req_libs, req_missing = _match_pkg_to_reqs(imports, setup_reqs)

        assert len(req_libs) == 0
        assert "unknown_package" in req_missing


class TestCheckStdLib:
    """Tests for _check_std_lib function."""

    def test_check_std_lib_with_stdlib_modules(self):
        """Test checking standard library modules."""
        modules = {"os", "sys", "pathlib"}

        result = _check_std_lib(modules)

        assert isinstance(result, ModuleImports)
        assert len(result.standard) > 0
        assert "os" in result.standard or "sys" in result.standard

    def test_check_std_lib_with_external_modules(self):
        """Test checking external modules."""
        modules = {"pandas", "requests"}

        result = _check_std_lib(modules)

        assert isinstance(result, ModuleImports)
        # External or unknown depending on whether they're installed
        assert (
            len(result.external) > 0
            or len(result.unknown) > 0
        )


class TestCheckStdlibPath:
    """Tests for _check_stdlib_path function."""

    def test_check_stdlib_path_builtin(self):
        """Test checking built-in module."""
        module = sys.modules["sys"]
        stdlib_paths = {sys.prefix.lower()}

        result = _check_stdlib_path(module, "sys", stdlib_paths)

        assert result == "sys"

    def test_check_stdlib_path_none_module(self):
        """Test checking None module."""
        result = _check_stdlib_path(None, "test", set())

        assert result == "test"

    def test_check_stdlib_path_site_packages(self):
        """Test checking module in site-packages."""
        mock_module = MagicMock()
        mock_module.__file__ = "/path/to/site-packages/module.py"

        result = _check_stdlib_path(mock_module, "module", set())

        assert result is None


class TestFilterNoneLocalImports:
    """Tests for _filter_none_local_imports function."""

    def test_filter_none_local_imports(self):
        """Test filtering imports without dotted names."""
        module_imports = {
            "module1": {"local.import": [], "simple": []},
            "module2": {"another.local": [], "other.pkg.import": []},
        }

        result = _filter_none_local_imports(module_imports)

        assert "module1" in result
        assert "local.import" in result["module1"]
        assert "simple" not in result["module1"]


class TestFilterExclusions:
    """Tests for _filter_exclusions function."""

    def test_filter_exclusions(self):
        """Test filtering imports by prefix exclusions."""
        module_imports = {
            "module1": {"common.utils": [], "data.provider": []},
            "module2": {"common.const": [], "transform.base": []},
        }
        excl_prefixes = ["common"]

        result = _filter_exclusions(module_imports, excl_prefixes)

        assert "module1" in result
        assert "common.utils" not in result["module1"]
        assert "data.provider" in result["module1"]


class TestCreateModuleReverseMapping:
    """Tests for _create_module_reverse_mapping function."""

    def test_create_module_reverse_mapping(self):
        """Test creating reverse mapping of module names."""
        pkg_mod_mapping = {
            "pkg.module.__init__": {"module", "pkg.module"},
            "pkg.other": {"other", "pkg.other"},
        }

        result = _create_module_reverse_mapping(pkg_mod_mapping)

        assert isinstance(result, dict)
        assert "module" in result
        assert result["module"] == "pkg.module"


class TestRemapPartialModuleNames:
    """Tests for _remap_partial_module_names function."""

    def test_remap_partial_module_names(self):
        """Test remapping partial module names to full names."""
        module_imports = {"importer": {"partial": ["func1"], "full.path": []}}
        name_mapping = {"partial": "full.partial.path"}

        result = _remap_partial_module_names(module_imports, name_mapping)

        assert "importer" in result
        assert "full.partial.path" in result["importer"]
        assert "partial" not in result["importer"]


class TestGetSetupReqs:
    """Tests for get_setup_reqs function."""

    def test_get_setup_reqs(self, tmp_path):
        """Test getting setup requirements from file."""
        req_file = tmp_path / "requirements.txt"
        req_file.write_text(
            "pandas>=1.4.0,<3.0.0\nrequests>=2.28.0\n# comment line\n"
        )

        setup_reqs, setup_versions = get_setup_reqs(
            str(tmp_path), req_file="requirements.txt", skip_setup=True
        )

        assert "pandas" in setup_reqs
        assert "requests" in setup_reqs
        assert "pandas" in setup_versions

    def test_get_setup_reqs_with_azure_packages(self, tmp_path):
        """Test getting setup requirements with Azure packages."""
        req_file = tmp_path / "requirements.txt"
        req_file.write_text("azure-mgmt-resource>=21.0.0\n")

        setup_reqs, _ = get_setup_reqs(
            str(tmp_path), req_file="requirements.txt", skip_setup=True
        )

        # Azure packages should be remapped from "-" to "."
        assert "azure.mgmt.resource" in setup_reqs


class TestAnalyzeModuleImports:
    """Tests for _analyze_module_imports function."""

    @patch("tools.toollib.import_analyzer.analyze")
    def test_analyze_module_imports(self, mock_analyze):
        """Test analyzing module imports."""
        mock_analyze.return_value = {
            "imports": ["os", "sys"],
            "imports_from": {"pathlib": ["Path"]},
        }

        py_file = Path("test.py")
        pkg_modules = {"mypackage.module"}
        setup_reqs = {"pathlib": "pathlib"}

        result = _analyze_module_imports(py_file, pkg_modules, setup_reqs)

        assert isinstance(result, ModuleImports)
        mock_analyze.assert_called_once_with(py_file)


class TestAnalyzeModuleLocalImports:
    """Tests for _analyze_module_local_imports function."""

    @patch("tools.toollib.import_analyzer.analyze")
    def test_analyze_module_local_imports(self, mock_analyze):
        """Test analyzing local module imports."""
        mock_analyze.return_value = {
            "imports": ["mypackage.module1", "os"],
            "imports_from": {"mypackage.module2": ["func"]},
        }

        py_file = Path("test.py")
        pkg_modules = {"mypackage.module1", "mypackage.module2"}

        result = _analyze_module_local_imports(py_file, pkg_modules)

        assert isinstance(result, dict)
        assert "mypackage.module1" in result
        assert "mypackage.module2" in result
        assert "os" not in result


class TestAnalyzeImports:
    """Tests for analyze_imports function."""

    @patch("tools.toollib.import_analyzer._analyze_module_imports")
    @patch("tools.toollib.import_analyzer._get_pkg_modules")
    @patch("tools.toollib.import_analyzer.get_setup_reqs")
    @patch("tools.toollib.import_analyzer.Path.glob")
    def test_analyze_imports(
        self, mock_glob, mock_get_setup, mock_get_modules, mock_analyze
    ):
        """Test analyzing imports for a package."""
        mock_get_setup.return_value = ({"pandas": "pandas"}, {})
        mock_get_modules.return_value = {"mypackage.module"}
        mock_file = MagicMock(
            spec=Path, relative_to=lambda x: Path("module.py")
        )
        mock_glob.return_value = [mock_file]
        mock_analyze.return_value = ModuleImports()

        result = analyze_imports(
            package_root=".",
            package_name="mypackage",
            req_file="requirements.txt",
            process_setup_py=False,
        )

        assert isinstance(result, dict)
        mock_get_setup.assert_called_once()


class TestAnalyzeLocalImports:
    """Tests for analyze_local_imports function."""

    @patch("tools.toollib.import_analyzer._remap_partial_module_names")
    @patch("tools.toollib.import_analyzer._create_module_reverse_mapping")
    @patch("tools.toollib.import_analyzer._filter_exclusions")
    @patch("tools.toollib.import_analyzer._filter_none_local_imports")
    @patch("tools.toollib.import_analyzer._analyze_module_local_imports")
    @patch("tools.toollib.import_analyzer._get_pkg_module_paths")
    @patch("tools.toollib.import_analyzer.Path.glob")
    def test_analyze_local_imports(
        self,
        mock_glob,
        mock_get_paths,
        mock_analyze,
        mock_filter_none,
        mock_filter_excl,
        mock_create_map,
        mock_remap,
    ):
        """Test analyzing local imports for a package."""
        mock_get_paths.return_value = defaultdict(
            set, {"mypackage.module": {"module", "mypackage.module"}}
        )
        mock_file = MagicMock(
            spec=Path, relative_to=lambda x: Path("module.py")
        )
        mock_glob.return_value = [mock_file]
        mock_analyze.return_value = {"mypackage.module": []}
        mock_filter_none.return_value = {"mypackage.module": {}}
        mock_filter_excl.return_value = {"mypackage.module": {}}
        mock_create_map.return_value = {}
        mock_remap.return_value = {}

        result = analyze_local_imports(
            package_root=".", package_name="mypackage", filter_common=True
        )

        assert result is not None
        mock_get_paths.assert_called_once()
