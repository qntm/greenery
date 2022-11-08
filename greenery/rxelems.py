# -*- coding: utf-8 -*-

'''
    Because of the circularity between `Pattern`, `Conc` and `Mult`, all three
    need to be in the same source file?
'''

from dataclasses import dataclass
from typing import Union

from .fsm import Fsm, ANYTHING_ELSE, null, epsilon, alphabet_key
from .multiplier import Multiplier, ZERO, QM, ONE, STAR
from .charclass import Charclass, NULLCHARCLASS
from .bound import Bound, INF


@dataclass(frozen=True)
class Conc():
    '''
        A `Conc` (short for "concatenation") is a tuple of `Mult`s i.e. an
        unbroken string of mults occurring one after the other.
        e.g. abcde[^fg]*h{4}[a-z]+(subpattern)(subpattern2)
        To express the empty string, use an empty `Conc`, Conc().
    '''
    def __init__(self, *mults):
        object.__setattr__(self, "mults", tuple(mults))

    def __eq__(self, other):
        return self.mults == other.mults

    def __hash__(self):
        return hash(self.mults)

    def __repr__(self):
        args = ", ".join(repr(mult) for mult in self.mults)
        return f"Conc({args})"

    def reduce(self):
        if self == NULLCONC:
            return self

        if self.empty():
            return NULLCONC

        # Try recursively reducing our mults
        reduced = tuple(mult.reduce() for mult in self.mults)
        if reduced != self.mults:
            return Conc(*reduced).reduce()

        # strip out mults which can only match the empty string
        for i, mult in enumerate(self.mults):
            if (
                # Conc contains "()" (i.e. a `Mult` containing only a `Pattern`
                # containing the empty string)? That can be removed
                # e.g. "a()b" -> "ab"
                (
                    mult.multiplicand == Pattern(EMPTYSTRING)
                ) \

                # If a `Mult` has an empty multiplicand, we can only match it
                # zero times => empty string => remove it entirely
                # e.g. "a[]{0,3}b" -> "ab"
                or (
                    mult.multiplicand.empty()
                    and mult.multiplier.min == Bound(0)
                ) \

                # Failing that, we have a positive multiplicand which we
                # intend to match zero times. In this case the only possible
                # match is the empty string => remove it
                # e.g. "a[XYZ]{0}b" -> "ab"
                or mult.multiplier == ZERO
            ):
                new = self.mults[:i] + self.mults[i + 1:]
                return Conc(*new).reduce()

        # We might be able to combine some mults together or at least simplify
        # the multiplier on one of them.
        if len(self.mults) > 1:
            for i in range(len(self.mults) - 1):
                r = self.mults[i]
                s = self.mults[i + 1]

                def to_pattern(multiplicand):
                    if isinstance(multiplicand, Pattern):
                        return multiplicand
                    return Pattern(Conc(Mult(multiplicand, ONE)))

                # so we can do intersection
                rm_pattern = to_pattern(r.multiplicand)
                sm_pattern = to_pattern(s.multiplicand)
                rm_sm_intersection = None

                # If R = S, then we can squish the multipliers together
                # e.g. ab?b?c -> ab{0,2}c
                if rm_pattern == sm_pattern:
                    squished = Mult(
                        rm_pattern,
                        r.multiplier + s.multiplier
                    )
                    new = self.mults[:i] + (squished,) + self.mults[i + 2:]
                    return Conc(*new).reduce()

                # If R's language is a subset of S's, then R{a,b}S{c,} reduces
                # to R{a}S{c,}...
                # e.g. \d+\w+ -> \d\w+
                # Do the cheapest checks first
                if r.multiplier.min < r.multiplier.max \
                   and s.multiplier.max == INF:
                    rm_sm_intersection = rm_pattern & sm_pattern
                    if rm_sm_intersection.equivalent(rm_pattern):
                        trimmed = Mult(
                            rm_pattern,
                            Multiplier(r.multiplier.min, r.multiplier.min)
                        )
                        new = self.mults[:i] + \
                            (trimmed, s) + \
                            self.mults[i + 2:]
                        return Conc(*new).reduce()

                # Conversely, if R is superset of S, then R{c,}S{a,b} reduces
                # to R{c,}S{a}.
                # e.g. [ab]+a? -> [ab]+
                # Do the cheapest checks first
                if r.multiplier.max == INF \
                   and s.multiplier.min < s.multiplier.max:
                    if rm_sm_intersection is None:
                        rm_sm_intersection = rm_pattern & sm_pattern
                    if rm_sm_intersection.equivalent(sm_pattern):
                        trimmed = Mult(
                            sm_pattern,
                            Multiplier(s.multiplier.min, s.multiplier.min)
                        )
                        new = self.mults[:i] + \
                            (r, trimmed) + \
                            self.mults[i + 2:]
                        return Conc(*new).reduce()

        # Conc contains (among other things) a *singleton* `Mult` containing
        # `Pattern` with only one internal `Conc`? Flatten out.
        # e.g. "a(d(ab|a*c))" -> "ad(ab|a*c)"
        # BUT NOT "a(d(ab|a*c)){2,}"
        # AND NOT "a(d(ab|a*c)|y)"
        for i, mult in enumerate(self.mults):
            if mult.multiplier == ONE \
               and isinstance(mult.multiplicand, Pattern) \
               and len(mult.multiplicand.concs) == 1:
                (conc,) = mult.multiplicand.concs
                new = self.mults[:i] + conc.mults + self.mults[i + 1:]
                return Conc(*new).reduce()

        return self

    def to_fsm(self, alphabet=None):
        if alphabet is None:
            alphabet = self.alphabet()

        # start with a component accepting only the empty string
        fsm1 = epsilon(alphabet)
        for mult in self.mults:
            fsm1 += mult.to_fsm(alphabet)
        return fsm1

    def alphabet(self):
        return {ANYTHING_ELSE}.union(*[mult.alphabet() for mult in self.mults])

    def empty(self):
        return any(mult.empty() for mult in self.mults)

    def __str__(self):
        return "".join(str(m) for m in self.mults)

    def common(self, other, suffix=False):
        '''
            Return the common prefix of these two `Conc`s; that is, the largest
            `Conc` which can be safely beheaded() from the front of both. The
            result could be `EMPTYSTRING`.
            "ZYAA, ZYBB" -> "ZY"
            "CZ, CZ" -> "CZ"
            "YC, ZC" -> ""

            With the "suffix" flag set, works from the end. E.g.:
            "AAZY, BBZY" -> "ZY"
            "CZ, CZ" -> "CZ"
            "CY, CZ" -> ""
        '''
        mults = []

        indices = range(min(len(self.mults), len(other.mults)))
        # e.g. [0, 1, 2, 3]

        # Work backwards from the end of both `Conc`s instead.
        if suffix:
            indices = [-i - 1 for i in indices]  # e.g. [-1, -2, -3, -4]

        for i in indices:
            x = self.mults[i]
            y = other.mults[i]
            common = x.common(y)

            # Happens when multiplicands disagree (e.g. "A.common(B)") or if
            # the multiplicand is shared but the common multiplier is `ZERO`
            # (e.g. "ABZ*.common(CZ)".)
            if common.multiplier == ZERO:
                break

            mults.append(common)

            # If we did not remove the entirety of both mults, we cannot
            # continue.
            if common != x or common != y:
                break

        if suffix:
            mults = reversed(mults)

        return Conc(*mults)

    def dock(self, other):
        '''
            Subtract another `Conc` from this one.
            This is the opposite of concatenation.
            For example, if ABC + DEF = ABCDEF,
            then logically ABCDEF - DEF = ABC.
        '''

        # e.g. self has mults at indices [0, 1, 2, 3, 4, 5, 6] len=7
        # e.g. other has mults at indices [0, 1, 2] len=3
        new = list(self.mults)
        for i in reversed(range(len(other.mults))):  # [2, 1, 0]
            # e.g. i = 1, j = 7 - 3 + 1 = 5
            j = len(self.mults) - len(other.mults) + i
            new[j] = new[j].dock(other.mults[i])

            if new[j].multiplier == ZERO:
                # omit that `Mult` entirely since it has been factored out
                del new[j]

            # If the subtraction is incomplete but there is more to
            # other.mults, then we have a problem. For example, "ABC{2} - BC"
            # subtracts the C successfully but leaves something behind,
            # then tries to subtract the B too, which isn't possible
            else:
                if i != 0:
                    raise Exception(
                        f"Can't subtract {repr(other)} from {repr(self)}"
                    )

        return Conc(*new)

    def behead(self, other):
        '''
            As with dock() but the other way around. For example, if
            ABC + DEF = ABCDEF, then ABCDEF.behead(AB) = CDEF.
        '''
        # Observe that FEDCBA - BA = FEDC.
        return self.reversed().dock(other.reversed()).reversed()

    def reversed(self):
        return Conc(*reversed([mult.reversed() for mult in self.mults]))


def from_fsm(f: Fsm):
    '''
        Turn the supplied finite state machine into a `Pattern`. This is
        accomplished using the Brzozowski algebraic method.
    '''
    # Make sure the supplied alphabet is kosher. It must contain only single-
    # character strings or `ANYTHING_ELSE`.
    for symbol in f.alphabet:
        if symbol == ANYTHING_ELSE:
            continue
        if isinstance(symbol, str) and len(symbol) == 1:
            continue
        raise Exception(
            f"Symbol {repr(symbol)} cannot be used in a regular expression"
        )

    # We need a new state not already used
    outside = object()

    # The set of strings that would be accepted by this FSM if you started
    # at state i is represented by the regex R_i.
    # If state i has a sole transition "a" to state j, then we know
    # R_i = a R_j.
    # If state i is final, then the empty string is also accepted by this
    # regex.
    # And so on...

    # From this we can build a set of simultaneous equations in len(f.states)
    # variables. This system is easily solved for all variables, but we only
    # need one: R_a, where a is the starting state.

    # The first thing we need to do is organise the states into order of depth,
    # so that when we perform our back-substitutions, we can start with the
    # last (deepest) state and therefore finish with R_a.
    states = [f.initial]
    i = 0
    while i < len(states):
        current = states[i]
        if current in f.map:
            for symbol in sorted(f.map[current], key=alphabet_key):
                next = f.map[current][symbol]
                if next not in states:
                    states.append(next)
        i += 1

    # Our system of equations is represented like so:
    brz = {}
    for a in f.states:
        brz[a] = {}
        for b in f.states:
            brz[a][b] = NULLPATTERN

        if a in f.finals:
            brz[a][outside] = Pattern(EMPTYSTRING)
        else:
            brz[a][outside] = NULLPATTERN

    # Populate it with some initial data.
    for a in f.map:
        for symbol in f.map[a]:
            b = f.map[a][symbol]
            if symbol == ANYTHING_ELSE:
                charclass = ~Charclass(f.alphabet - {ANYTHING_ELSE})
            else:
                charclass = Charclass({symbol})

            brz[a][b] = Pattern(
                *brz[a][b].concs,
                Conc(Mult(charclass, ONE))
            ).reduce()

    # Now perform our back-substitution
    for i in reversed(range(len(states))):
        a = states[i]

        # Before the equation for R_a can be substituted into the other
        # equations, we need to resolve the self-transition (if any).
        # e.g.    R_a = 0 R_a |   1 R_b |   2 R_c
        # becomes R_a =         0*1 R_b | 0*2 R_c
        loop = Mult(brz[a][a], STAR)  # i.e. "0*"
        del brz[a][a]

        for right in brz[a]:
            brz[a][right] = Pattern(
                Conc(
                    loop,
                    Mult(brz[a][right], ONE)
                )
            ).reduce()

        # Note: even if we're down to our final equation, the above step still
        # needs to be performed before anything is returned.

        # Now we can substitute this equation into all of the previous ones.
        for j in range(i):
            b = states[j]

            # e.g. substituting R_a =  0*1 R_b |      0*2 R_c
            # into              R_b =    3 R_a |        4 R_c | 5 R_d
            # yields            R_b = 30*1 R_b | (30*2|4) R_c | 5 R_d
            univ = brz[b][a]  # i.e. "3"
            del brz[b][a]

            for right in brz[a]:
                brz[b][right] = Pattern(
                    *brz[b][right].concs,
                    Conc(
                        Mult(univ, ONE),
                        Mult(brz[a][right], ONE)
                    )
                ).reduce()

    return brz[f.initial][outside].reduce()


def call_fsm(method):
    '''
        Take a method which acts on 0 or more regular expression objects...
        return a new method which simply converts them all to FSMs, calls the
        FSM method on them instead, then converts the result back to a regular
        expression. We do this for several of the more annoying operations.
    '''
    fsm_method = getattr(Fsm, method.__name__)

    def new_method(*elems):
        alphabet = set().union(*[elem.alphabet() for elem in elems])
        return from_fsm(fsm_method(*[elem.to_fsm(alphabet) for elem in elems]))
    return new_method


@dataclass(frozen=True)
class Pattern:
    '''
        A `Pattern` (also known as an "alt", short for "alternation") is a
        set of `Conc`s. A `Pattern` expresses multiple alternate possibilities.
        When written out as a regex, these would separated by pipes. A
        `Pattern` containing no possibilities is possible and represents a
        regular expression matching no strings whatsoever (there is no
        conventional string form for this).

        e.g. "abc|def(ghi|jkl)" is an alt containing two `Conc`s: "abc" and
        "def(ghi|jkl)". The latter is a `Conc` containing four `Mult`s: "d",
        "e", "f" and "(ghi|jkl)". The latter in turn is a `Mult` consisting of
        an upper bound 1, a lower bound 1, and a multiplicand which is a new
        subpattern, "ghi|jkl". This new subpattern again consists of two
        `Conc`s: "ghi" and "jkl".
    '''
    def __init__(self, *concs):
        object.__setattr__(self, "concs", frozenset(concs))

    def __eq__(self, other):
        return isinstance(other, Pattern) and self.concs == other.concs

    def __hash__(self):
        return hash(self.concs)

    def __repr__(self):
        args = ", ".join(repr(conc) for conc in self.concs)
        return f"Pattern({args})"

    def alphabet(self):
        return {ANYTHING_ELSE}.union(*[c.alphabet() for c in self.concs])

    def empty(self):
        return all(conc.empty() for conc in self.concs)

    def intersection(self, other):
        # A deceptively simple method for an astoundingly difficult operation
        alphabet = self.alphabet() | other.alphabet()

        # Which means that we can build finite state machines sharing that
        # alphabet
        combined = self.to_fsm(alphabet) & other.to_fsm(alphabet)
        return from_fsm(combined)

    def __and__(self, other):
        return self.intersection(other)

    @call_fsm
    def difference(*elems):
        '''
            Return a regular expression which matches any string which `self`
            matches but none of the strings which `other` matches.
        '''
        pass

    def __sub__(self, other):
        return self.difference(other)

    def union(self, other):
        return Pattern(*(self.concs | other.concs))

    def __or__(self, other):
        return self.union(other)

    def __str__(self):
        if len(self.concs) == 0:
            raise Exception(f"Can't serialise {repr(self)}")
        return "|".join(sorted(str(conc) for conc in self.concs))

    def reduce(self):
        if self == NULLPATTERN:
            return self

        if self.empty():
            return NULLPATTERN

        # Try recursively reducing our internal `Conc`s.
        reduced = frozenset(c.reduce() for c in self.concs)
        if reduced != self.concs:
            return Pattern(*reduced).reduce()

        # If one of our internal concs is empty, remove it
        for conc in self.concs:
            if conc.empty():
                new = self.concs - {conc}
                return Pattern(*new).reduce()

        # If we have just one `Conc` with just one `Mult` with a multiplier of
        # 1, and the multiplicand is a `Pattern`, pull that up
        if len(self.concs) == 1:
            (conc,) = self.concs
            if len(conc.mults) == 1 \
               and conc.mults[0].multiplier == ONE \
               and isinstance(conc.mults[0].multiplicand, Pattern):
                return conc.mults[0].multiplicand.reduce()

        # If this `Pattern` contains several `Conc`s each containing just 1
        # `Mult` and their multiplicands agree, we may be able to merge the
        # multipliers.
        # e.g. "a{1,2}|a{3,4}|bc" -> "a{1,4}|bc"
        oldconcs = list(self.concs)  # so we can index the things
        for i in range(len(oldconcs)):
            conc1 = oldconcs[i]
            if len(conc1.mults) != 1:
                continue
            multiplicand1 = conc1.mults[0].multiplicand
            for j in range(i + 1, len(oldconcs)):
                conc2 = oldconcs[j]
                if len(conc2.mults) != 1:
                    continue
                multiplicand2 = conc2.mults[0].multiplicand
                if multiplicand2 != multiplicand1:
                    continue
                multiplicand = multiplicand1
                multiplier1 = conc1.mults[0].multiplier
                multiplier2 = conc2.mults[0].multiplier
                if not multiplier1.canunion(multiplier2):
                    continue
                multiplier = multiplier1 | multiplier2
                newconcs = \
                    oldconcs[:i] + \
                    oldconcs[i + 1:j] + \
                    oldconcs[j + 1:] + \
                    [Conc(Mult(multiplicand, multiplier))]
                return Pattern(*newconcs).reduce()

        # If this `Pattern` contains several `Conc`s each containing just 1
        # `Mult` each containing just a `Charclass`, with a multiplier of 1,
        # then we can merge those `Charclass`es together.
        # e.g. "0|[1-9]|ab" -> "[0-9]|ab"
        merged_charclass = NULLCHARCLASS
        num_merged = 0
        rest = []
        for conc in self.concs:
            if len(conc.mults) == 1 \
               and conc.mults[0].multiplier == ONE \
               and isinstance(conc.mults[0].multiplicand, Charclass):
                merged_charclass |= conc.mults[0].multiplicand
                num_merged += 1
            else:
                rest.append(conc)
        if num_merged >= 2:
            rest.append(Conc(Mult(merged_charclass, ONE)))
            return Pattern(*rest).reduce()

        # If one of the present `Pattern`'s `Conc`s is the empty string...
        if EMPTYSTRING in self.concs:
            for conc in self.concs:
                # ...and there is another `Conc`
                # with a single `Mult` whose lower bound is 0...
                if len(conc.mults) == 1 \
                   and conc.mults[0].multiplier.min == Bound(0):
                    # Then we can omit the empty string.
                    # E.g. "|(ab)*|def" => "(ab)*|def".
                    rest = self.concs - {EMPTYSTRING}
                    return Pattern(*rest).reduce()

            for conc in self.concs:
                # ...and there is another `Conc`
                # with a single `Mult` whose lower bound is 1...
                if len(conc.mults) == 1 \
                   and conc.mults[0].multiplier.min == Bound(1):
                    # Then we can merge the empty string into that.
                    # E.g. "|(ab)+|def" => "(ab)*|def".
                    merged_conc = Conc(
                        Mult(
                            conc.mults[0].multiplicand,
                            conc.mults[0].multiplier * QM
                        )
                    )
                    rest = self.concs - {EMPTYSTRING, conc} | {merged_conc}
                    return Pattern(*rest).reduce()

        # If the present `Pattern`'s `Conc`s all have a common prefix, split
        # that out. This increases the depth of the object
        # but it is still arguably simpler/ripe for further reduction
        # e.g. "abc|ade" -> a(bc|de)"
        if len(self.concs) > 1:
            prefix = self._commonconc()
            if prefix != EMPTYSTRING:
                leftovers = self.behead(prefix)
                mults = prefix.mults + (Mult(leftovers, ONE),)
                return Pattern(Conc(*mults)).reduce()

            # Same but for suffixes.
            # e.g. "xyz|stz -> (xy|st)z"
            suffix = self._commonconc(suffix=True)
            if suffix != EMPTYSTRING:
                leftovers = self.dock(suffix)
                mults = (Mult(leftovers, ONE),) + suffix.mults
                return Pattern(Conc(*mults)).reduce()

        return self

    @call_fsm
    def symmetric_difference(*elems):
        '''
            Return a regular expression matching only the strings recognised by
            `self` or `other` but not both.
        '''
        pass

    def __xor__(self, other):
        return self.symmetric_difference(other)

    def dock(self, other):
        '''
            The opposite of concatenation. Remove a common suffix from the
            present `Pattern`; that is, from each of its constituent concs.

            AYZ|BYZ|CYZ - YZ -> A|B|C.
        '''
        return Pattern(*[conc.dock(other) for conc in self.concs])

    def behead(self, other):
        '''
            Like dock() but the other way around. Remove a common prefix from
            the present `Pattern`; that is, from each of its constituent concs.

            ZA|ZB|ZC.behead(Z) -> A|B|C
        '''
        return Pattern(*[conc.behead(other) for conc in self.concs])

    def _commonconc(self, suffix=False):
        '''
            Find the longest `Conc` which acts as prefix to every `Conc` in
            this `Pattern`. This could be `EMPTYSTRING`. Return the common
            prefix along with all the leftovers after truncating that common
            prefix from each `Conc`.

            "ZA|ZB|ZC" -> "Z", "(A|B|C)"
            "ZA|ZB|ZC|Z" -> "Z", "(A|B|C|)"
            "CZ|CZ" -> "CZ", "()"

            If "suffix" is True, the same result but for suffixes.
        '''
        if len(self.concs) == 0:
            raise Exception(f"Can't call _commonconc on {repr(self)}")

        from functools import reduce
        return reduce(
            lambda x, y: x.common(y, suffix=suffix),
            self.concs
        )

    def to_fsm(self, alphabet=None):
        if alphabet is None:
            alphabet = self.alphabet()

        fsm1 = null(alphabet)
        for conc in self.concs:
            fsm1 |= conc.to_fsm(alphabet)
        return fsm1

    def reversed(self):
        return Pattern(*(c.reversed() for c in self.concs))

    def copy(self):
        '''
            For completeness only, since `set.copy()` also exists. `Pattern`s
            are immutable, so I can see only very odd reasons to need this
        '''
        return Pattern(*self.concs)

    def equivalent(self, other):
        '''
            Two `Pattern`s are equivalent if they recognise the same strings.
            Note that in the general case this is actually quite an intensive
            calculation, but far from unsolvable, as we demonstrate here:
        '''
        return self.to_fsm().equivalent(other.to_fsm())

    def times(self, multiplier):
        '''
            Equivalent to repeated concatenation. Multiplier consists of a
            minimum and a maximum; maximum may be infinite (for Kleene star
            closure). Call using "a = b * qm"
        '''
        return Pattern(Conc(Mult(self, multiplier)))

    def __mul__(self, multiplier):
        return self.times(multiplier)

    @call_fsm
    def everythingbut(self):
        '''
            Return a `Pattern` which will match any string not matched by
            `self`, and which will not match any string matched by `self`.
            Another task which is very difficult in general (and typically
            returns utter garbage when actually printed), but becomes trivial
            to code thanks to FSM routines.
        '''
        pass

    def derive(self, string):
        return from_fsm(self.to_fsm().derive(string))

    @call_fsm
    def isdisjoint(self, other):
        '''
            Treat `self` and `other` as sets of strings and see if they are
            disjoint
        '''
        pass

    def matches(self, string):
        return self.to_fsm().accepts(string)

    def __contains__(self, string):
        '''
            This lets you use the syntax `"a" in pattern` to see whether the
            string "a" is in the set of strings matched by `pattern`.
        '''
        return self.matches(string)

    def __reversed__(self):
        return self.reversed()

    def cardinality(self):
        '''
            Consider the regular expression as a set of strings and return the
            cardinality of that set, or raise an OverflowError if there are
            infinitely many.
        '''
        # There is no way to do this other than converting to an FSM, because
        # the `Pattern` may allow duplicate routes, such as "a|a".
        return self.to_fsm().cardinality()

    def __len__(self):
        return self.cardinality()

    def strings(self, otherchar=None):
        '''
            Each time next() is called on this iterator, a new string is
            returned which this `Pattern` can match. `StopIteration`
            is raised once all such strings have been returned, although a
            regex with a * in may match infinitely many strings.
        '''

        # In the case of a regex like "[^abc]", there are infinitely many
        # (well, a very large finite number of) single characters which will
        # match. It's not productive to iterate over all of these giving every
        # single example. You must supply your own "otherchar" to stand in for
        # all of these possibilities.
        for string in self.to_fsm().strings():

            # Have to represent `ANYTHING_ELSE` somehow.
            if ANYTHING_ELSE in string:
                if otherchar is None:
                    raise Exception("Please choose an `otherchar`")
                string = [
                    otherchar if char == ANYTHING_ELSE else char
                    for char in string
                ]

            yield "".join(string)

    def __iter__(self):
        '''
            This allows you to do `for string in pattern` as a list
            comprehension!
        '''
        return self.strings()


@dataclass(frozen=True)
class Mult:
    '''
        A `Mult` is a combination of a multiplicand with a multiplier (a min
        and a max). The vast majority of characters in regular expressions
        occur without a specific multiplier, which is implicitly equivalent to
        a min of 1 and a max of 1, but many more have explicit multipliers like
        "*" (min = 0, max = INF) and so on.

        e.g. a, b{2}, c?, d*, [efg]{2,5}, f{2,}, (anysubpattern)+, .*, ...
    '''
    multiplicand: Union[Charclass, Pattern]
    multiplier: Multiplier

    def __eq__(self, other):
        return isinstance(other.multiplicand, type(self.multiplicand)) \
               and self.multiplicand == other.multiplicand \
               and self.multiplier == other.multiplier

    def __hash__(self):
        return hash((self.multiplicand, self.multiplier))

    def __repr__(self):
        return f"Mult({repr(self.multiplicand)}, {repr(self.multiplier)})"

    def dock(self, other):
        '''
            "Dock" another `Mult` from this one (i.e. remove part of the tail)
            and return the result. The reverse of concatenation. This is a lot
            trickier.
            e.g. a{4,5} - a{3} = a{1,2}
        '''
        if other.multiplicand != self.multiplicand:
            raise Exception(
                f"Can't subtract {repr(other)} from {repr(self)}"
            )
        return Mult(self.multiplicand, self.multiplier - other.multiplier)

    def common(self, other):
        '''
            Return the common part of these two mults. This is the largest
            `Mult` which can be safely subtracted from both the originals. The
            multiplier on this `Mult` could be `ZERO`: this is the case if, for
            example, the multiplicands disagree.
        '''
        if self.multiplicand == other.multiplicand:
            return Mult(
                self.multiplicand,
                self.multiplier.common(other.multiplier)
            )

        # Multiplicands disagree, no common part at all.
        return Mult(NULLCHARCLASS, ZERO)

    def alphabet(self):
        return {ANYTHING_ELSE} | self.multiplicand.alphabet()

    def empty(self):
        return self.multiplicand.empty() and self.multiplier.min > Bound(0)

    def reduce(self):
        if self == NULLMULT:
            return self

        # Can't match anything: reduce to empty `Mult`
        if self.empty():
            return NULLMULT

        # Try recursively reducing our multiplicand
        reduced = self.multiplicand.reduce()
        if reduced != self.multiplicand:
            return Mult(reduced, self.multiplier).reduce()

        # If our multiplicand is a `Pattern` containing an empty `Conc`
        # we can pull that "optional" bit out into our own multiplier
        # instead.
        # e.g. (A|B|C|) -> (A|B|C)?
        # e.g. (A|B|C|){2} -> (A|B|C){0,2}
        if isinstance(self.multiplicand, Pattern) \
           and EMPTYSTRING in self.multiplicand.concs \
           and self.multiplier.canmultiplyby(QM):
            return Mult(
                Pattern(
                    *filter(
                        lambda conc: len(conc.mults) != 0,
                        self.multiplicand.concs
                    )
                ),
                self.multiplier * QM,
            ).reduce()

        # If our multiplicand is a `Pattern` containing a single `Conc`
        # containing a single `Mult`, we can scrap the `Pattern` in favour of
        # that `Mult`'s multiplicand
        # e.g. ([ab])* -> [ab]*
        # e.g. ((a))* -> (a)* -> a*
        # NOTE: this logic lives here at the `Mult` level, NOT in
        # `Pattern.reduce` because we want to return another `Mult` (same type)
        if isinstance(self.multiplicand, Pattern) \
           and len(self.multiplicand.concs) == 1:
            (conc,) = self.multiplicand.concs
            if len(conc.mults) == 1 \
               and conc.mults[0].multiplier.canmultiplyby(self.multiplier):
                return Mult(
                    conc.mults[0].multiplicand,
                    conc.mults[0].multiplier * self.multiplier
                ).reduce()

        # no reduction possible
        return self

    def __str__(self):
        if isinstance(self.multiplicand, Pattern):
            return f"({str(self.multiplicand)}){str(self.multiplier)}"
        if isinstance(self.multiplicand, Charclass):
            return f"{str(self.multiplicand)}{str(self.multiplier)}"
        raise Exception(f"Unknown type {str(type(self.inner))}")

    def to_fsm(self, alphabet=None):
        if alphabet is None:
            alphabet = self.alphabet()

        # worked example: (min, max) = (5, 7) or (5, INF)
        # (mandatory, optional) = (5, 2) or (5, INF)

        unit = self.multiplicand.to_fsm(alphabet)
        # accepts e.g. "ab"

        # accepts "ababababab"
        mandatory = unit * self.multiplier.mandatory.v

        # unlimited additional copies
        if self.multiplier.optional == INF:
            optional = unit.star()
            # accepts "(ab)*"

        else:
            optional = epsilon(alphabet) | unit
            # accepts "(ab)?"

            optional *= self.multiplier.optional.v
            # accepts "(ab)?(ab)?"

        return mandatory + optional

    def reversed(self):
        return Mult(self.multiplicand.reversed(), self.multiplier)


NULLMULT = Mult(NULLCHARCLASS, ONE)
NULLCONC = Conc(NULLMULT)
EMPTYSTRING = Conc()
NULLPATTERN = Pattern(NULLCONC)
