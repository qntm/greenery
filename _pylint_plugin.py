from __future__ import annotations

import sys
from typing import TYPE_CHECKING

# mypy: disable-error-code=import

if TYPE_CHECKING:
    from pylint.lint import PyLinter


def register(_: PyLinter, /) -> None:
    """Called by pylint to register this plugin module."""


def load_configuration(linter: PyLinter, /) -> None:
    """Called by pylint to load configuration."""
    # We ham-fistedly disable 'arguments-differ' warning message on <Py3.10.
    # The type declaration of Generic.__new__ is over-broad in Python 3.8
    # (expecting also in 3.9), but fixed in later versions.
    # This causes misleading warnings if `Generic` subclasses override
    # `__new__` but don't expose a crazy "anything goes" (*args, **kwargs)
    # signature.
    # It's not worth writing a specialized transformer to fix it, and it's lame
    # to have to write lint suppressions on correct code, especially when it
    # only applies to a particular version.
    if sys.version_info < (3, 10):
        linter.disable("arguments-differ")
