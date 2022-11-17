from __future__ import annotations

from .bound import Bound, INF


def test_bound_str():
    assert str(Bound(2)) == "2"
    assert str(INF) == ""


def test_bound():
    assert min(Bound(0), INF) == Bound(0)
    assert min(Bound(1), INF) == Bound(1)
