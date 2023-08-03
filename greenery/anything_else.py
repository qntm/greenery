from enum import Enum, auto
from functools import total_ordering
from typing import (
    Any,
)

__all__ = (
    "ANYTHING_ELSE",
)

@total_ordering
class AnythingElse(Enum):
    """
    This is a surrogate symbol which you can use in your finite state
    machines to represent "any symbol not in the official alphabet". For
    example, if your state machine's alphabet is `{"a", "b", "c", "d",
    fsm.ANYTHING_ELSE}`, then if "e" is passed as a symbol, it will be
    converted to `fsm.ANYTHING_ELSE` before following the appropriate
    transition.

    This is an `Enum` to enforce a singleton value, detectable by type
    checkers, as described in:
    https://www.python.org/dev/peps/pep-0484/#support-for-singleton-types-in-unions
    """

    TOKEN = auto()

    def __lt__(self, _: Any, /) -> bool:
        """Ensure `fsm.ANYTHING_ELSE` always sorts last"""
        return False

    def __eq__(self, other: Any, /) -> bool:
        return self is other

    def __hash__(self, /) -> int:
        return hash(type(self))

    def __str__(self, /) -> str:
        return "ANYTHING_ELSE"

    def __repr__(self, /) -> str:
        return "ANYTHING_ELSE"


ANYTHING_ELSE = AnythingElse.TOKEN
