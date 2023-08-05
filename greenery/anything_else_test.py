from __future__ import annotations

import pickle
from copy import copy

import pytest

from .fsm import ANYTHING_ELSE, AnythingElse, Fsm, epsilon, from_charclass, null
from .charclass import Charclass

def test_anything_else_singleton() -> None:
    assert AnythingElse.TOKEN is ANYTHING_ELSE


def test_anything_else_self() -> None:
    """ANYTHING_ELSE is consistently equal to itself."""

    # pylint: disable=comparison-with-itself
    # pylint: disable=unneeded-not
    assert not ANYTHING_ELSE < ANYTHING_ELSE
    assert ANYTHING_ELSE <= ANYTHING_ELSE
    assert not ANYTHING_ELSE != ANYTHING_ELSE
    assert ANYTHING_ELSE == ANYTHING_ELSE
    assert ANYTHING_ELSE >= ANYTHING_ELSE
    assert not ANYTHING_ELSE > ANYTHING_ELSE


@pytest.mark.parametrize(
    argnames="val",
    argvalues=(
        float("-inf"),
        float("nan"),
        float("inf"),
        0,
        "abc",
        object(),
        str(ANYTHING_ELSE),
    ),
)
def test_anything_else_sorts_after(val: object) -> None:
    """ANYTHING_ELSE sorts strictly after anything."""

    assert not ANYTHING_ELSE < val
    assert not ANYTHING_ELSE <= val
    assert not ANYTHING_ELSE == val
    assert ANYTHING_ELSE != val
    assert ANYTHING_ELSE > val
    assert ANYTHING_ELSE >= val

    assert val < ANYTHING_ELSE
    assert val <= ANYTHING_ELSE
    assert val != ANYTHING_ELSE
    assert not val == ANYTHING_ELSE
    assert not val > ANYTHING_ELSE
    assert not val >= ANYTHING_ELSE


def test_anything_else_pickle() -> None:
    # [^z]*
    fsm1 = Fsm(
        alphabet={"z", ~Charclass("z")},
        states={0, 1, 2},
        initial=0,
        finals={1},
        map={
            0: {"z": 2, ~Charclass("z"): 1},
            1: {"z": 2, ~Charclass("z"): 1},
            2: {"z": 2, ~Charclass("z"): 2},
        },
    )

    fsm1_unpickled = pickle.loads(pickle.dumps(fsm1))

    # Newly-created instance.
    assert fsm1_unpickled is not fsm1

    # but equivalent.
    assert fsm1 == fsm1_unpickled

    assert fsm1_unpickled.alphabet == {Charclass("z"), ~Charclass("z")}
