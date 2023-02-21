from __future__ import annotations

__all__ = (
    "Bound",
    "INF",
)

from dataclasses import dataclass


@dataclass(frozen=True)
class Bound:
    '''An integer but sometimes also possibly infinite (None)'''
    v: int | None

    def __post_init__(self):
        if self.v is not None and self.v < 0:
            raise Exception(f"Invalid bound: {repr(self.v)}")

    def __repr__(self):
        return f"Bound({repr(self.v)})"

    def __str__(self):
        if self == INF:
            # This only happens for an unlimited upper bound
            return ""
        return str(self.v)

    def __eq__(self, other):
        return self.v == other.v

    def __hash__(self):
        return hash(self.v)

    def __lt__(self, other):
        if self == INF:
            return False
        if other == INF:
            return True
        return self.v < other.v

    def __ge__(self, other):
        return not self < other

    def __mul__(self, other):
        '''Multiply this bound by another'''
        if self == Bound(0) or other == Bound(0):
            return Bound(0)
        if self == INF or other == INF:
            return INF
        return Bound(self.v * other.v)

    def __add__(self, other):
        '''Add this bound to another'''
        if self == INF or other == INF:
            return INF
        return Bound(self.v + other.v)

    def __sub__(self, other):
        '''
            Subtract another bound from this one.
            Caution: this operation is not meaningful for all bounds.
        '''
        if other == INF:
            if self != INF:
                raise Exception(
                    f"Can't subtract {repr(other)} from {repr(self)}"
                )

            # Infinity minus infinity is zero. This has to be true so that
            # we can for example subtract Multiplier(Bound(0), INF) from
            # Multiplier(Bound(1), INF) to get Multiplier(Bound(1), Bound(1))
            return Bound(0)
        if self == INF:
            return self
        return Bound(self.v - other.v)

    def copy(self):
        return Bound(self.v)


# Use this for cases where no upper bound is needed
INF = Bound(None)
