# GitHub Copilot Instructions for msticpy

## Project Overview

**MSTICPy** is a Python library for InfoSec investigation and threat hunting.
It provides:

- Data query providers for Microsoft Sentinel, Azure Monitor, Kusto, Splunk, and more
- Threat intelligence lookups (VirusTotal, OTX, etc.)
- Data enrichment (GeoIP, WhoIs, etc.)
- Security-focused data analysis and visualization tools
- Jupyter notebook integration for interactive investigations

## Package Structure

- **Package name**: `msticpy` (in `msticpy/`)
- **Import pattern**: `import msticpy as mp`
- **Source**: `msticpy/` contains implementation modules
- **Tests**: `tests/` with pytest markers: `unit`, `integration`, `slow`
- **Tools**: `tools/` supplementary tools not core to the package
- **Documentation**: `docs/` - Sphinx source files and notebooks

### Key Subpackages

- `msticpy/data/` - Data providers and query execution
- `msticpy/context/` - Threat intelligence and enrichment providers
- `msticpy/auth/` - Authentication helpers (Azure, etc.)
- `msticpy/vis/` - Visualization components
- `msticpy/transform/` - Data transformation utilities
- `msticpy/init/` - Initialization and pandas accessors

## Code Conventions

### Python Standards (Enforced by Ruff)

- **Line length**: 93 characters
- **Type hints**: Required (enforced by mypy, annotations checked)
  - Always use built-in types like `list`, `dict` for type annotations
  - E.g. use `list[str]` instead of `List[str]`, `str | None` instead of `Optional[str]`
- **Docstrings**: Required for public functions (D-series rules) - use numpy style
  - Document parameters, return type and exceptions raised
  - **Single-line**: Keep on same line as triple quotes:
    `"""Return the user name."""`
  - **Multi-line**: Summary starts on new line after opening quotes, blank line before
    Parameters/Returns sections, blank line before closing quotes:

    ```python
    def example(name: str) -> str:
        """
        Return a greeting for the given name.

        Parameters
        ----------
        name : str
            The name to greet.

        Returns
        -------
        str
            The greeting message.

        """
    ```

- **Imports and Formatting**: Sorted/grouped automatically (isort)

### General Coding Style

- Avoid using Python built-in `open` function for file operations. Use `pathlib.Path`
  methods instead. Prefer `Path.*` methods over legacy `os.*` methods.
- **Logging**: Create a logger per module: `logger = logging.getLogger(__name__)`
  - Use `%s`, `%d` style variable substitution rather than f-strings in log calls
- Never use inline import statements. Always place imports at the top of the file
  (conditional imports should also be at the top, before main code).
- Be careful with indentation - always replace lines using the same indentation
  unless introducing branches, etc.
- Try to avoid a line length of over 90 characters - applies to code, docstrings,
  comments and suppressions.
- Prefer use of pydantic classes over dataclasses or attrs classes.
- Prefer use of pydantic classes over complex dictionaries for structured data.

## Documentation

### Sphinx API Documentation

If adding/changing/removing any public API, update the Sphinx API documentation:
```bash
cd docs
del /Q source\api\*
sphinx-apidoc --o source/api --force --module-first --separate ../msticpy
del source\api\modules.rst
```
Add any changed files to the commit. The docs build will generate HTML and
report errors/warnings during CI.

## Testing

### Test Creation

- Always use pytest and generate pytest-style test functions
- Mock httpx requests using the `respx` library
- Test file paths should mirror the source module:
  `msticpy/path/module.py` → `tests/path/test_module.py`
- Always add at least a single-line docstring to fixtures and test functions
- Unit test coverage should be >= 85% on new code
- For mock secrets (passwords, keys, etc.), always use `"[PLACEHOLDER]"` as the value

### Running Tests

```bash
pytest                                    # All tests
pytest --cov=msticpy --cov-report=html   # With coverage
```

## Code Quality Tools

**Always** run pre-commit before creating a PR.
**Always** run mypy before creating a PR.

### Pre-commit Hooks

```bash
pip install pre-commit
pre-commit install
pre-commit run --all-files    # Run manually
```

### Running Linters Manually

```bash
ruff check msticpy --fix      # Lint and auto-fix
ruff format msticpy           # Format code
mypy msticpy                  # Type checking
```

**Important**: When running mypy, run it completely. It is slow - avoid preliminary
runs to find error counts.

### After Generating New Python Code

**ALWAYS run pre-commit or equivalent checks:**
```bash
pre-commit run --all-files
# Or manually:
ruff check msticpy --fix && ruff format msticpy && mypy msticpy
```

Fix any errors before committing. Do not leave Ruff or mypy errors in generated code.

## Commit Guidelines

- Write clear, descriptive commit messages
- Always run pre-commit hooks before committing

## Key Files

- **`pyproject.toml`**: Package config, dependencies, tool settings (ruff, mypy, pytest)
- **`msticpyconfig.yaml`**: User configuration for data providers, TI providers, etc.
- **`docs/`**: Sphinx documentation source and example notebooks
