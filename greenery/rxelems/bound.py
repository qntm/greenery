# -*- coding: utf-8 -*-

from dataclasses import dataclass
from typing import Optional

@dataclass(frozen=True)
class bound:
    '''An integer but sometimes also possibly infinite (None)'''
    v: Optional[int]

    def __post_init__(self):
        if not self.v is None and self.v < 0:
            raise Exception("Invalid bound: " + repr(self.v))

    def __repr__(self):
        return "bound(" + repr(self.v) + ")"

    def __str__(self):
        if self == inf:
            # This only happens for an unlimited upper bound
            return ""
        return str(self.v)

    def __eq__(self, other):
        try:
            return self.v == other.v
        except AttributeError:
            return False

    def __hash__(self):
        return hash(self.v)

    def __lt__(self, other):
        if self == inf:
            return False
        if other == inf:
            return True
        return self.v < other.v

    def __ge__(self, other):
        return not self < other

    def __mul__(self, other):
        '''Multiply this bound by another'''
        if self == bound(0) or other == bound(0):
            return bound(0)
        if self == inf or other == inf:
            return inf
        return bound(self.v * other.v)

    def __add__(self, other):
        '''Add this bound to another'''
        if self == inf or other == inf:
            return inf
        return bound(self.v + other.v)

    def __sub__(self, other):
        '''
            Subtract another bound from this one.
            Caution: this operation is not meaningful for all bounds.
        '''
        if other == inf:
            if self != inf:
                raise Exception("Can't subtract " + repr(other) + " from " + repr(self))

            # Infinity minus infinity is zero. This has to be true so that
            # we can for example subtract multiplier(bound(0), inf) from
            # multiplier(bound(1), inf) to get multiplier(bound(1), bound(1))
            return bound(0)
        if self == inf:
            return self
        return bound(self.v - other.v)

    def copy(self):
        return bound(self.v)

# Use this for cases where no upper bound is needed
inf = bound(None)
