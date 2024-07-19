"""Context Providers sub-package."""

from __future__ import annotations

from ..._version import VERSION
from .servicenow import ServiceNow  # noqa:F401

__version__ = VERSION

CONTEXT_PROVIDERS: dict[str, tuple[str, str]] = {
    "ServiceNow": ("servicenow", "ServiceNow"),
}
