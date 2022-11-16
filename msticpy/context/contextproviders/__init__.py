"""Context Providers sub-package."""
from typing import Dict, Tuple

from ..._version import VERSION
from .servicenow import ServiceNow  # noqa:F401

__version__ = VERSION

CONTEXT_PROVIDERS: Dict[str, Tuple[str, str]] = {
    "ServiceNow": ("servicenow", "ServiceNow"),
}
