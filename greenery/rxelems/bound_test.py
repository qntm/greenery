# -*- coding: utf-8 -*-

from .bound import bound, inf

def test_bound_str():
    assert str(bound(2)) == "2"
    assert str(inf) == ""

def test_bound():
    assert min(bound(0), inf) == bound(0)
    assert min(bound(1), inf) == bound(1)
