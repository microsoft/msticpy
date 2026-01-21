# GitHub Copilot Instructions for msticpy

## Package Structure
- **Package name**: `msticpy` (in `msticpy`)
- **Import pattern**: `import msticpy as mp`
- **Source**: `msticpy` contains implementation modules
- **Tests**: `tests/` with pytest markers: `unit`, `integration`, `slow`
- **Tools**: `tools/` supplementary tools not core to the package
- **Documentation**: `docs` - sphinx source files and notebooks

## Code Conventions

### Python Standards (Enforced by Ruff)
- **Line length**: 93 characters
- **Type hints**: Required (enforced by mypy, annotations checked)
  - Always use built-in types like `list`, `dict`, for type annotations and avoid use 
    types from `typing`.
  - E.g. use `list[str]` instead of `List[str]`, `str | None` instead of 
    `Optional[str]`.
- **Docstrings**: Required for public functions (D-series rules) - use numpy style.
  - Document parameters, return type and exceptions raised for public 
    functions/methods.
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
- Avoid using Python built-in `open` function for file operations. Use 
  `pathlib.Path` methods instead. Prefer `Path.*` methods over legacy `os.*` methods.
- **Logging**: Create a logger per module `logger = logging.getLogger(__name__)`.
  - When adding logging calls, use `%s`, `%d` style variable substitution rather than
    f-strings.
- Never use inline import statements. Always place imports at the top of the file
  (there are some exceptional cases where conditional imports are used but, these
  should also be at the top of the file, before the main code).
- When generating code, be careful with indentation - always replace lines using the 
  same indentation unless introducing branches, etc.
- Try to avoid a line length of over 90 characters - this applies to code, 
  docstrings, comments and suppressions.

## Testing

### Test Creation
- Always use pytest and generate pytest-style test functions.
- If you need to mock httpx requests, use the respx library.
- Test file modules should mirror the name/path of the tested module, e.g.
  `msticpy/path/module.py` → `tests/path/test_module.py`
- Always add at least a single-line docstring to fixtures and test functions.
  If the context of the parameters is not obvious, explain them in the docstring.
- Unit test coverage should be >= 85% on new code.
- Test "secrets" - if you need to mock a secret value (password, key, etc.), always 
  use the value `"[PLACEHOLDER]"` as the value of the secret.

### Running Tests
```bash
pytest                           # All tests
pytest --cov=msticpy --cov-report=html
```

## Code Quality Tools
**Always** run pre-commit before creating a PR.
**Always** run mypy before creating a PR.

### Pre-commit Hooks
This project uses pre-commit for automated code quality checks. Install and enable:
```bash
pip install pre-commit
pre-commit install
```

Run manually on all files:
```bash
pre-commit run --all-files
```

### Running Linters Manually
```bash
ruff check msticpy --fix         # Lint and auto-fix
ruff format msticpy              # Format code
mypy msticpy                     # Type checking
```

**Important**: When running mypy, always run it to get the full output. It is slow, so
avoid preliminary runs to find error counts - run it once completely.

### When Generating New Python Code
**ALWAYS run pre-commit or equivalent checks after generating new Python code:**
```bash
pre-commit run --all-files
# Or manually:
ruff check msticpy --fix && ruff format msticpy && mypy msticpy
```

Fix any errors before committing. Do not leave Ruff or mypy errors in generated code.

## Commit Guidelines
- Write clear, descriptive commit messages
- Always run pre-commit hooks (or linters manually) before committing
