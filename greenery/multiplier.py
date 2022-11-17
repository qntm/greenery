from __future__ import annotations

from dataclasses import dataclass, field

from .bound import INF, Bound


@dataclass(frozen=True)
class Multiplier:
    '''
        A min and a max. The vast majority of characters in regular expressions
        occur without a specific multiplier, which is implicitly equivalent to
        a min of 1 and a max of 1, but many more have explicit multipliers like
        "*" (min = 0, max = inf) and so on.

        Although it seems odd and can lead to some confusing edge cases, we do
        also permit a max of 0 (iff min is 0 too). This allows the multiplier
        `ZERO` to exist, which actually are quite useful in their own special
        way.
    '''
    min: Bound
    max: Bound
    mandatory: Bound = field(init=False)
    optional: Bound = field(init=False)

    def __post_init__(self):
        if self.min == INF:
            raise Exception(
                f"Minimum bound of a multiplier can't be {repr(INF)}"
            )
        if self.min > self.max:
            raise Exception(
                "Invalid multiplier bounds: "
                f"{repr(self.min)} and {repr(self.max)}"
            )

        # More useful than "min" and "max" in many situations
        # are "mandatory" and "optional".
        object.__setattr__(self, "mandatory", self.min)
        object.__setattr__(self, "optional", self.max - self.min)

    def __eq__(self, other):
        return self.min == other.min and self.max == other.max

    def __hash__(self):
        return hash((self.min, self.max))

    def __repr__(self):
        return f"Multiplier({repr(self.min)}, {repr(self.max)})"

    def __str__(self):
        if self.max == Bound(0):
            raise Exception(
                f"Can't serialise a multiplier with bound {repr(self.max)}"
            )
        if self in symbolic.keys():
            return symbolic[self]
        if self.min == self.max:
            return "{" + str(self.min) + "}"
        return "{" + str(self.min) + "," + str(self.max) + "}"

    def canmultiplyby(self, other):
        '''
            Multiplication is not well-defined for all pairs of multipliers
            because the resulting possibilities do not necessarily form a
            continuous range.

            For example:
                {0,x} * {0,y} = {0,x*y}
                {2} * {3} = {6}
                {2} * {1,2} = ERROR

            The proof isn't simple but suffice it to say that {p,p+q} * {r,r+s}
            is equal to {pr, (p+q)(r+s)} only if s=0 or qr+1 >= p. If not, then
            at least one gap appears in the range. The first inaccessible
            number is (p+q)r+1. And no, multiplication is not commutative
        '''
        return other.optional == Bound(0) or \
            self.optional * other.mandatory + Bound(1) >= self.mandatory

    def __mul__(self, other):
        '''Multiply this multiplier by another'''
        if not self.canmultiplyby(other):
            raise Exception(
                f"Can't multiply {repr(self)} by {repr(other)}"
            )
        return Multiplier(self.min * other.min, self.max * other.max)

    def __add__(self, other):
        '''Add two multipliers together'''
        return Multiplier(self.min + other.min, self.max + other.max)

    def __sub__(self, other):
        '''
            Subtract another multiplier from this one.
            Caution: multipliers are not totally ordered.
            This operation is not meaningful for all pairs of multipliers.
        '''
        mandatory = self.mandatory - other.mandatory
        optional = self.optional - other.optional
        return Multiplier(mandatory, mandatory + optional)

    def canintersect(self, other):
        '''
            Intersection is not well-defined for all pairs of multipliers.
            For example:
                {2,3} & {3,4} = {3}
                {2,} & {1,7} = {2,7}
                {2} & {5} = ERROR
        '''
        return not (self.max < other.min or other.max < self.min)

    def __and__(self, other):
        '''
            Find the intersection of two multipliers: that is, a third
            multiplier expressing the range covered by both of the originals.
            This is not defined for all multipliers since they may not overlap.
        '''
        if not self.canintersect(other):
            raise Exception(
                f"Can't compute intersection of {repr(self)} and {repr(other)}"
            )
        a = max(self.min, other.min)
        b = min(self.max, other.max)
        return Multiplier(a, b)

    def canunion(self, other):
        '''
            Union is not defined for all pairs of multipliers.
            E.g. {0,1} | {3,4} -> nope
        '''
        return not (
            self.max + Bound(1) < other.min or
            other.max + Bound(1) < self.min
        )

    def __or__(self, other):
        '''
            Find the union of two multipliers: that is, a third multiplier
            expressing the range covered by either of the originals. This is
            not defined for all multipliers since they may not intersect.
        '''
        if not self.canunion(other):
            raise Exception(
                f"Can't compute the union of {repr(self)} and {repr(other)}"
            )
        a = min(self.min, other.min)
        b = max(self.max, other.max)
        return Multiplier(a, b)

    def common(self, other):
        '''
            Find the shared part of two multipliers. This is the largest
            multiplier which can be safely subtracted from both the originals.
            This may return the `ZERO` multiplier.
        '''
        mandatory = min(self.mandatory, other.mandatory)
        optional = min(self.optional, other.optional)
        return Multiplier(mandatory, mandatory + optional)

    def copy(self):
        return Multiplier(self.min.copy(), self.max.copy())


# Preset multipliers. These get used ALL THE TIME in unit tests
ZERO = Multiplier(Bound(0), Bound(0))  # has some occasional uses
QM = Multiplier(Bound(0), Bound(1))
ONE = Multiplier(Bound(1), Bound(1))
STAR = Multiplier(Bound(0), INF)
PLUS = Multiplier(Bound(1), INF)

# Symbol lookup table for preset multipliers.
symbolic = {
    QM: "?",
    ONE: "",
    STAR: "*",
    PLUS: "+",
}
