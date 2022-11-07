# -*- coding: utf-8 -*-

'''
    rxelems:
    Classes and methods for the creation and manipulation of regular expression
    objects and components.

    * A regular expression is a "pattern" object.
    * Each pattern alternates (with a pipe, "|") between zero or more "conc"
    (concatenation) objects.
    * Each conc is a concatenation of zero or more "mult" (multiplication)
    objects.
    * Each mult consists of a multiplicand and a multiplier. A multiplier consists
    of a minimum and a maximum, e.g. min = 0, max = 1 indicates the "?"
    multiplier. The multiplicand is either a nested pattern object, or a
    charclass object.
    * A charclass is a set of chars, such as "a", "[a-z]", "\\d", ".", with a
    possible "negated" flag as in "[^a]".
    * Since these can be combined together freely they are, in the absence of a
    better metaphor, collectively referred to as rxelems pieces.

    We also include methods for parsing a string into a pattern object,
    serialising a pattern object out as a string (or "regular expression", if you
    will), and for concatenating or alternating between arbitrary elements,
    using overloaded operators.

    If the FSM module is available, call rxelems.to_fsm() on any element to
    return a finite state machine capable of accepting strings described by
    that element.

    Most important are the reduce() methods present in mult, conc and
    pattern. While there is no such thing as a canonical form for a given regex
    pattern, these procedures can drastically simplify a regex structure for
    readability. They're also pretty extensible.
'''

from typing import Optional, Union, Tuple, FrozenSet
from greenery import fsm
from dataclasses import dataclass, field
from greenery.rxelems.bound import bound, inf
from greenery.rxelems.multiplier import multiplier, star, one, zero, qm
from greenery.rxelems.charclass import charclass, nothing
from greenery.rxelems.rxelem import rxelem

def call_fsm(method):
    '''
        Take a method which acts on 0 or more regular expression objects... return a
        new method which simply converts them all to FSMs, calls the FSM method
        on them instead, then converts the result back to a regular expression.
        We do this for several of the more annoying operations.
    '''
    fsm_method = getattr(fsm.fsm, method.__name__)
    def new_method(*elems):
        alphabet = set().union(*[elem.alphabet() for elem in elems])
        return from_fsm(fsm_method(*[elem.to_fsm(alphabet) for elem in elems]))
    return new_method

def from_fsm(f: fsm.fsm):
    '''
        Turn the supplied finite state machine into a `rxelems` object. This is
        accomplished using the Brzozowski algebraic method.
    '''
    # Make sure the supplied alphabet is kosher. It must contain only single-
    # character strings or `fsm.anything_else`.
    for symbol in f.alphabet:
        if symbol == fsm.anything_else:
            continue
        if isinstance(symbol, str) and len(symbol) == 1:
            continue
        raise Exception("Symbol " + repr(symbol) + " cannot be used in a regular expression")

    # We need a new state not already used
    outside = object()

    # The set of strings that would be accepted by this FSM if you started
    # at state i is represented by the regex R_i.
    # If state i has a sole transition "a" to state j, then we know R_i = a R_j.
    # If state i is final, then the empty string is also accepted by this regex.
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
            for symbol in sorted(f.map[current], key=fsm.key):
                next = f.map[current][symbol]
                if next not in states:
                    states.append(next)
        i += 1

    # Our system of equations is represented like so:
    brz = {}
    for a in f.states:
        brz[a] = {}
        for b in f.states:
            brz[a][b] = pattern(conc(mult(nothing, one)))

        if a in f.finals:
            brz[a][outside] = pattern(conc())
        else:
            brz[a][outside] = pattern(conc(mult(nothing, one)))

    # Populate it with some initial data.
    for a in f.map:
        for symbol in f.map[a]:
            b = f.map[a][symbol]
            if symbol == fsm.anything_else:
                linkcharclass = ~charclass(f.alphabet - {fsm.anything_else})
            else:
                linkcharclass = charclass({symbol})

            brz[a][b] = pattern(*brz[a][b].concs, conc(mult(linkcharclass, one))).reduce()

    # Now perform our back-substitution
    for i in reversed(range(len(states))):
        a = states[i]

        # Before the equation for R_a can be substituted into the other
        # equations, we need to resolve the self-transition (if any).
        # e.g.    R_a = 0 R_a |   1 R_b |   2 R_c
        # becomes R_a =         0*1 R_b | 0*2 R_c
        loopmult = mult(brz[a][a], star) # i.e. "0*"
        del brz[a][a]

        for right in brz[a]:
            brz[a][right] = pattern(conc(loopmult, mult(brz[a][right], one))).reduce()

        # Note: even if we're down to our final equation, the above step still
        # needs to be performed before anything is returned.

        # Now we can substitute this equation into all of the previous ones.
        for j in range(i):
            b = states[j]

            # e.g. substituting R_a =  0*1 R_b |      0*2 R_c
            # into              R_b =    3 R_a |        4 R_c | 5 R_d
            # yields            R_b = 30*1 R_b | (30*2|4) R_c | 5 R_d
            univ = brz[b][a] # i.e. "3"
            del brz[b][a]

            for right in brz[a]:
                brz[b][right] = pattern(
                    *brz[b][right].concs,
                    conc(
                        mult(univ, one),
                        mult(brz[a][right], one)
                    )
                ).reduce()

    return brz[f.initial][outside].reduce()

class mult(rxelem):
    '''
        A mult is a combination of a multiplicand with
        a multiplier (a min and a max). The vast majority of characters in regular
        expressions occur without a specific multiplier, which is implicitly
        equivalent to a min of 1 and a max of 1, but many more have explicit
        multipliers like "*" (min = 0, max = inf) and so on.

        e.g. a, b{2}, c?, d*, [efg]{2,5}, f{2,}, (anysubpattern)+, .*, and so on
    '''

    def __init__(self, multiplicand, multiplier):
        self.__dict__["multiplicand"] = multiplicand
        self.__dict__["multiplier"]   = multiplier

    def __eq__(self, other):
        try:
            return self.multiplicand == other.multiplicand \
            and self.multiplier == other.multiplier
        except AttributeError:
            return False

    def __hash__(self):
        return hash((self.multiplicand, self.multiplier))

    def __repr__(self):
        string = "mult("
        string += repr(self.multiplicand)
        string += ", " + repr(self.multiplier)
        string += ")"
        return string

    def dock(self, other):
        '''
            "Dock" another mult from this one (i.e. remove part of the tail) and
            return the result. The reverse of concatenation. This is a lot trickier.
            e.g. a{4,5} - a{3} = a{1,2}
        '''
        if other.multiplicand != self.multiplicand:
            raise Exception("Can't subtract " + repr(other) + " from " + repr(self))
        return mult(self.multiplicand, self.multiplier - other.multiplier)

    def common(self, other):
        '''
            Return the common part of these two mults. This is the largest mult
            which can be safely subtracted from both the originals. The multiplier
            on this mult could be zero: this is the case if, for example, the
            multiplicands disagree.
        '''
        if self.multiplicand == other.multiplicand:
            return mult(self.multiplicand, self.multiplier.common(other.multiplier))

        # Multiplicands disagree, no common part at all.
        return mult(nothing, zero)

    def alphabet(self):
        return {fsm.anything_else} | self.multiplicand.alphabet()

    def empty(self):
        return self.multiplicand.empty() and self.multiplier.min > bound(0)

    def reduce(self):
        if self == emptymult:
            return self

        # Can't match anything: reduce to empty mult
        if self.empty():
            return emptymult

        # Try recursively reducing our multiplicand
        reduced = self.multiplicand.reduce()
        if reduced != self.multiplicand:
            return mult(reduced, self.multiplier).reduce()

        # If our multiplicand is a pattern containing an empty conc()
        # we can pull that "optional" bit out into our own multiplier
        # instead.
        # e.g. (A|B|C|) -> (A|B|C)?
        # e.g. (A|B|C|){2} -> (A|B|C){0,2}
        if hasattr(self.multiplicand, "concs") \
        and emptystring in self.multiplicand.concs \
        and self.multiplier.canmultiplyby(qm):
            return mult(
                pattern(
                    *self.multiplicand.concs.difference({emptystring})
                ),
                self.multiplier * qm,
            ).reduce()

        # If our multiplicand is a pattern containing a single conc
        # containing a single mult, we can scrap the pattern in favour of that mult's multiplicand
        # e.g. ([ab])* -> [ab]*
        # e.g. ((a))* -> (a)* -> a*
        # NOTE: this logic lives here at the `mult` level, NOT in `pattern.reduce`
        # because we want to return another `mult` (same type)
        if hasattr(self.multiplicand, "concs") \
        and len(self.multiplicand.concs) == 1:
            (conc,) = self.multiplicand.concs
            if len(conc.mults) == 1 \
            and conc.mults[0].multiplier.canmultiplyby(self.multiplier):
                return mult(
                    conc.mults[0].multiplicand,
                    conc.mults[0].multiplier * self.multiplier
                ).reduce()

        # no reduction possible
        return self

    def __str__(self):
        # recurse into subpattern
        if hasattr(self.multiplicand, "concs"):
            output = "(" + str(self.multiplicand) + ")"

        else:
            output = str(self.multiplicand)

        suffix = str(self.multiplier)

        return output + suffix

    def to_fsm(self, alphabet=None):
        if alphabet is None:
            alphabet = self.alphabet()

        # worked example: (min, max) = (5, 7) or (5, inf)
        # (mandatory, optional) = (5, 2) or (5, inf)

        unit = self.multiplicand.to_fsm(alphabet)
        # accepts e.g. "ab"

        # accepts "ababababab"
        mandatory = unit * self.multiplier.mandatory.v

        # unlimited additional copies
        if self.multiplier.optional == inf:
            optional = unit.star()
            # accepts "(ab)*"

        else:
            optional = fsm.epsilon(alphabet) | unit
            # accepts "(ab)?"

            optional *= self.multiplier.optional.v
            # accepts "(ab)?(ab)?"

        return mandatory + optional

    def reversed(self):
        return mult(self.multiplicand.reversed(), self.multiplier)

emptymult = mult(charclass(""), one)

class conc(rxelem):
    '''
        A conc (short for "concatenation") is a tuple of mults i.e. an unbroken
        string of mults occurring one after the other.
        e.g. abcde[^fg]*h{4}[a-z]+(subpattern)(subpattern2)
        To express the empty string, use an empty conc, conc().
    '''

    def __init__(self, *mults):
        self.__dict__["mults"] = tuple(mults)

    def __eq__(self, other):
        try:
            return self.mults == other.mults
        except AttributeError:
            return False

    def __hash__(self):
        return hash(self.mults)

    def __repr__(self):
        string = "conc("
        string += ", ".join(repr(m) for m in self.mults)
        string += ")"
        return string

    def reduce(self):
        if self == emptyconc:
            return self

        if self.empty():
            return emptyconc

        # Try recursively reducing our mults
        reduced = tuple(m.reduce() for m in self.mults)
        if reduced != self.mults:
            return conc(*reduced).reduce()

        # strip out mults which can only match the empty string
        for i in range(len(self.mults)):
            if (
                # Conc contains "()" (i.e. a mult containing only a pattern containing the
                # empty string)? That can be removed e.g. "a()b" -> "ab"
                (
                    hasattr(self.mults[i].multiplicand, "concs") \
                    and self.mults[i].multiplicand == pattern(emptystring)
                ) \

                # If a mult has an empty multiplicand, we can only match it
                # zero times => empty string => remove it entirely
                # e.g. "a[]{0,3}b" -> "ab"
                or (
                    self.mults[i].multiplicand.empty() \
                    and self.mults[i].multiplier.min == bound(0)
                ) \

                # Failing that, we have a positive multiplicand which we
                # intend to match zero times. In this case the only possible
                # match is the empty string => remove it
                # e.g. "a[XYZ]{0}b" -> "ab"
                or self.mults[i].multiplier == zero
            ):
                new = self.mults[:i] + self.mults[i + 1:]
                return conc(*new).reduce()

        # We might be able to combine some mults together or at least simplify the multiplier on
        # one of them.
        if len(self.mults) > 1:
            for i in range(len(self.mults) - 1):
                r = self.mults[i]
                s = self.mults[i + 1]

                def promote(multiplicand):
                    if hasattr(multiplicand, "concs"):
                        return multiplicand
                    return pattern(conc(mult(multiplicand, one)))

                # promote so we can do intersection
                rmPattern = promote(r.multiplicand)
                smPattern = promote(s.multiplicand)
                rmsmIntersection = None

                # If R = S, then we can squish the multipliers together
                # e.g. ab?b?c -> ab{0,2}c
                if rmPattern == smPattern:
                    squished = mult(
                        rmPattern,
                        r.multiplier + s.multiplier
                    )
                    new = self.mults[:i] + (squished,) + self.mults[i + 2:]
                    return conc(*new).reduce()

                # If R's language is a subset of S's, then R{a,b}S{c,} reduces to R{a}S{c,}...
                # e.g. \d+\w+ -> \d\w+
                # Do the cheapest checks first
                if r.multiplier.min < r.multiplier.max \
                and s.multiplier.max == inf:
                    rmsmIntersection = rmPattern & smPattern
                    if rmsmIntersection.equivalent(rmPattern):
                        trimmed = mult(
                            rmPattern,
                            multiplier(r.multiplier.min, r.multiplier.min)
                        )
                        new = self.mults[:i] + (trimmed, s) + self.mults[i + 2:]
                        return conc(*new).reduce()

                # Conversely, if R is superset of S, then R{c,}S{a,b} reduces to R{c,}S{a}.
                # e.g. [ab]+a? -> [ab]+
                # Do the cheapest checks first
                if r.multiplier.max == inf \
                and s.multiplier.min < s.multiplier.max:
                    if rmsmIntersection is None:
                        rmsmIntersection = rmPattern & smPattern
                    if rmsmIntersection.equivalent(smPattern):
                        trimmed = mult(
                            smPattern,
                            multiplier(s.multiplier.min, s.multiplier.min)
                        )
                        new = self.mults[:i] + (r, trimmed) + self.mults[i + 2:]
                        return conc(*new).reduce()

        # Conc contains (among other things) a *singleton* mult containing a pattern
        # with only one internal conc? Flatten out.
        # e.g. "a(d(ab|a*c))" -> "ad(ab|a*c)"
        # BUT NOT "a(d(ab|a*c)){2,}"
        # AND NOT "a(d(ab|a*c)|y)"
        for i in range(len(self.mults)):
            if self.mults[i].multiplier == one \
            and hasattr(self.mults[i].multiplicand, "concs") \
            and len(self.mults[i].multiplicand.concs) == 1:
                (singleton,) = self.mults[i].multiplicand.concs
                new = self.mults[:i] + singleton.mults + self.mults[i + 1:]
                return conc(*new).reduce()

        return self

    def to_fsm(self, alphabet=None):
        if alphabet is None:
            alphabet = self.alphabet()

        # start with a component accepting only the empty string
        fsm1 = fsm.epsilon(alphabet)
        for m in self.mults:
            fsm1 += m.to_fsm(alphabet)
        return fsm1

    def alphabet(self):
        return {fsm.anything_else}.union(*[m.alphabet() for m in self.mults])

    def empty(self):
        for m in self.mults:
            if m.empty():
                return True
        return False

    def __str__(self):
        return "".join(str(m) for m in self.mults)

    def common(self, other, suffix=False):
        '''
            Return the common prefix of these two concs; that is, the largest conc
            which can be safely beheaded() from the front of both.
            The result could be emptystring.
            "ZYAA, ZYBB" -> "ZY"
            "CZ, CZ" -> "CZ"
            "YC, ZC" -> ""

            With the "suffix" flag set, works from the end. E.g.:
            "AAZY, BBZY" -> "ZY"
            "CZ, CZ" -> "CZ"
            "CY, CZ" -> ""
        '''
        mults = []

        indices = range(min(len(self.mults), len(other.mults))) # e.g. [0, 1, 2, 3]

        # Work backwards from the end of both concs instead.
        if suffix:
            indices = [-i - 1 for i in indices] # e.g. [-1, -2, -3, -4]

        for i in indices:

            common = self.mults[i].common(other.mults[i])

            # Happens when multiplicands disagree (e.g. "A.common(B)") or if
            # the multiplicand is shared but the common multiplier is zero
            # (e.g. "ABZ*.common(CZ)".)
            if common.multiplier == zero:
                break

            mults.append(common)

            # If we did not remove the entirety of both mults, we cannot continue.
            if common != self.mults[i] or common != other.mults[i]:
                break

        if suffix:
            mults = reversed(mults)

        return conc(*mults)

    def dock(self, other):
        '''
            Subtract another conc from this one.
            This is the opposite of concatenation. For example, if ABC + DEF = ABCDEF,
            then logically ABCDEF - DEF = ABC.
        '''

        # e.g. self has mults at indices [0, 1, 2, 3, 4, 5, 6] len=7
        # e.g. other has mults at indices [0, 1, 2] len=3
        new = list(self.mults)
        for i in reversed(range(len(other.mults))): # [2, 1, 0]
            # e.g. i = 1, j = 7 - 3 + 1 = 5
            j = len(self.mults) - len(other.mults) + i
            new[j] = new[j].dock(other.mults[i])

            if new[j].multiplier == zero:
                # omit that mult entirely since it has been factored out
                del new[j]

            # If the subtraction is incomplete but there is more to
            # other.mults, then we have a problem. For example, "ABC{2} - BC"
            # subtracts the C successfully but leaves something behind,
            # then tries to subtract the B too, which isn't possible
            else:
                if i != 0:
                    raise Exception("Can't subtract " + repr(other) + " from " + repr(self))

        return conc(*new)

    def behead(self, other):
        '''
            As with dock() but the other way around. For example, if
            ABC + DEF = ABCDEF, then ABCDEF.behead(AB) = CDEF.
        '''
        # Observe that FEDCBA - BA = FEDC.
        return self.reversed().dock(other.reversed()).reversed()

    def reversed(self):
        return conc(*reversed([m.reversed() for m in self.mults]))

emptyconc = conc(emptymult)

class pattern(rxelem):
    '''
        A pattern (also known as an "alt", short for "alternation") is a
        set of concs. A pattern expresses multiple alternate possibilities.
        When written out as a regex, these would separated by pipes. A pattern
        containing no possibilities is possible and represents a regular expression
        matching no strings whatsoever (there is no conventional string form for
        this).

        e.g. "abc|def(ghi|jkl)" is an alt containing two concs: "abc" and
        "def(ghi|jkl)". The latter is a conc containing four mults: "d", "e", "f"
        and "(ghi|jkl)". The latter in turn is a mult consisting of an upper bound
        1, a lower bound 1, and a multiplicand which is a new subpattern, "ghi|jkl".
        This new subpattern again consists of two concs: "ghi" and "jkl".
    '''
    def __init__(self, *concs):
        self.__dict__["concs"] = frozenset(concs)
        for c in concs:
            if not hasattr(c, "mults"):
                raise Exception(repr(c))

    def __eq__(self, other):
        try:
            return self.concs == other.concs
        except AttributeError:
            return False

    def __hash__(self):
        return hash(self.concs)

    def __repr__(self):
        string = "pattern("
        string += ", ".join(repr(c) for c in self.concs)
        string += ")"
        return string

    def alphabet(self):
        return {fsm.anything_else}.union(*[c.alphabet() for c in self.concs])

    def empty(self):
        for c in self.concs:
            if not c.empty():
                return False
        return True

    def intersection(self, other):
        '''
            Intersection function. Return an element which can match any string
            that both self and other can match. Fairly elementary results relating
            to regular languages and finite state machines show that this is
            possible, but implementation is a BEAST in many cases. Here, we convert
            both elements to FSMs (see to_fsm(), above) for the intersection, then
            back to elements afterwards.
            Call using "a = b & c"
        '''
        # A deceptively simple method for an astoundingly difficult operation
        alphabet = self.alphabet() | other.alphabet()

        # Which means that we can build finite state machines sharing that alphabet
        combined = self.to_fsm(alphabet) & other.to_fsm(alphabet)
        return from_fsm(combined)

    def __and__(self, other):
        return self.intersection(other)

    @call_fsm
    def difference(*elems):
        '''
            Return a regular expression which matches any string which `self` matches
            but none of the strings which `other` matches.
        '''
        pass

    def __sub__(self, other):
        return self.difference(other)

    def union(self, other):
        return pattern(*(self.concs | other.concs))

    def __or__(self, other):
        return self.union(other)

    def __str__(self):
        if len(self.concs) == 0:
            raise Exception("Can't serialise " + repr(self))

        # take the alternation of the input collection of regular expressions.
        # i.e. jam "|" between each element

        # 1+ elements.
        return "|".join(sorted(str(c) for c in self.concs))

    def reduce(self):
        if self == emptypattern:
            return self

        if self.empty():
            return emptypattern

        # Try recursively reducing our internal concs.
        reduced = frozenset(c.reduce() for c in self.concs)
        if reduced != self.concs:
            return pattern(*reduced).reduce()

        # If one of our internal concs is empty, remove it
        for c in self.concs:
            if c.empty():
                new = self.concs - {c}
                return pattern(*new).reduce()

        # If we have just one conc with just one mult with a multiplier of 1,
        # and the multiplicand is a pattern, pull that up
        if len(self.concs) == 1:
            (singleconc,) = self.concs
            if len(singleconc.mults) == 1 \
            and singleconc.mults[0].multiplier == one \
            and hasattr(singleconc.mults[0].multiplicand, "concs"):
                return singleconc.mults[0].multiplicand.reduce()

        # If this pattern contains several concs each containing just 1 mult and
        # their multiplicands agree, we may be able to merge the multipliers
        # e.g. "a{1,2}|a{3,4}|bc" -> "a{1,4}|bc"
        oldconcs = list(self.concs) # so we can index the things
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
                    [conc(mult(multiplicand, multiplier))]
                return pattern(*newconcs).reduce()

        # If this pattern contains several concs each containing just 1 mult
        # each containing just a charclass, with a multiplier of 1,
        # then we can merge those charclasses together.
        # e.g. "0|[1-9]|ab" -> "[0-9]|ab"
        changed = False
        merger = nothing
        rest = []
        for c in self.concs:
            if len(c.mults) == 1 \
            and c.mults[0].multiplier == one \
            and hasattr(c.mults[0].multiplicand, "chars"):
                if merger != nothing:
                    changed = True

                def union(a, b):
                    # ¬A OR ¬B = ¬(A AND B)
                    # ¬A OR B = ¬(A - B)
                    # A OR ¬B = ¬(B - A)
                    # A OR B
                    if a.negated:
                        if b.negated:
                            return ~charclass(a.chars & b.chars)
                        else:
                            return ~charclass(a.chars - b.chars)
                    else:
                        if b.negated:
                            return ~charclass(b.chars - a.chars)
                        else:
                            return charclass(a.chars | b.chars)

                merger = union(merger, c.mults[0].multiplicand)
            else:
                rest.append(c)
        if changed:
            rest.append(conc(mult(merger, one)))
            return pattern(*rest).reduce()

        # If one of the present pattern's concs is the empty string...
        if emptystring in self.concs:
            for c in self.concs:
                # ...and there is another conc with a single mult whose lower bound is 0...
                if len(c.mults) == 1 and c.mults[0].multiplier.min == bound(0):
                    # Then we can omit the empty string.
                    # E.g. "|(ab)*|def" => "(ab)*|def".
                    rest = self.concs - {emptystring}
                    return pattern(*rest).reduce()

            for c in self.concs:
                # ...and there is another conc with a single mult whose lower bound is 1...
                if len(c.mults) == 1 and c.mults[0].multiplier.min == bound(1):
                    # Then we can merge the empty string into that.
                    # E.g. "|(ab)+|def" => "(ab)*|def".
                    rest = self.concs - {emptystring}
                    rest = rest - {c} | {conc(mult(c.mults[0].multiplicand, c.mults[0].multiplier * qm))}
                    return pattern(*rest).reduce()

        # If the present pattern's concs all have a common prefix, split
        # that out. This increases the depth of the object
        # but it is still arguably simpler/ripe for further reduction
        # e.g. "abc|ade" -> a(bc|de)"
        if len(self.concs) > 1:
            prefix = self._commonconc()
            if prefix != emptystring:
                leftovers = self.behead(prefix)
                mults = prefix.mults + (mult(leftovers, one),)
                return pattern(conc(*mults)).reduce()

            # Same but for suffixes.
            # e.g. "xyz|stz -> (xy|st)z"
            suffix = self._commonconc(suffix=True)
            if suffix != emptystring:
                leftovers = self.dock(suffix)
                mults = (mult(leftovers, one),) + suffix.mults
                return pattern(conc(*mults)).reduce()

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
            The opposite of concatenation. Remove a common suffix from the present
            pattern; that is, from each of its constituent concs.
            AYZ|BYZ|CYZ - YZ = A|B|C.
        '''
        return pattern(*[c.dock(other) for c in self.concs])

    def behead(self, other):
        '''
            Like dock() but the other way around. Remove a common prefix from the
            present pattern; that is, from each of its constituent concs.
            ZA|ZB|ZC.behead(Z) = A|B|C
        '''
        return pattern(*[c.behead(other) for c in self.concs])

    def _commonconc(self, suffix=False):
        '''
            Find the longest conc which acts as prefix to every conc in this pattern.
            This could be the empty string. Return the common prefix along with all
            the leftovers after truncating that common prefix from each conc.
            "ZA|ZB|ZC" -> "Z", "(A|B|C)"
            "ZA|ZB|ZC|Z" -> "Z", "(A|B|C|)"
            "CZ|CZ" -> "CZ", "()"

            If "suffix" is True, the same result but for suffixes.
        '''
        if len(self.concs) == 0:
            raise Exception("Can't call _commonconc on " + repr(self))

        from functools import reduce
        return reduce(
            lambda x, y: x.common(y, suffix=suffix),
            self.concs
        )

    def to_fsm(self, alphabet=None):
        if alphabet is None:
            alphabet = self.alphabet()

        fsm1 = fsm.null(alphabet)
        for c in self.concs:
            fsm1 |= c.to_fsm(alphabet)
        return fsm1

    def reversed(self):
        return pattern(*(c.reversed() for c in self.concs))

    def copy(self):
        '''
            For completeness only, since `set.copy()` also exists. `pattern`s
            are immutable, so I can see only very odd reasons to need this
        '''
        return self

    def equivalent(self, other):
        '''
            Two lego objects are equivalent if they recognise the same strings. Note
            that in the general case this is actually quite an intensive calculation,
            but far from unsolvable, as we demonstrate here:
        '''
        return self.to_fsm().equivalent(other.to_fsm())

    @call_fsm
    def everythingbut(self):
        '''
            Return a lego object which will match any string not matched by self,
            and which will not match any string matched by self.
            Another task which is very difficult in general (and typically returns
            utter garbage when actually printed), but becomes trivial to code
            thanks to FSM routines.
        '''
        pass

    def derive(self, string):
        return from_fsm(self.to_fsm().derive(string))

    @call_fsm
    def isdisjoint(self, other):
        '''
            Treat `self` and `other` as sets of strings and see if they are disjoint
        '''
        pass

    def matches(self, string):
        return self.to_fsm().accepts(string)

    def __contains__(self, string):
        '''
            This lets you use the syntax `"a" in pattern1` to see whether the string
            "a" is in the set of strings matched by `pattern1`.
        '''
        return self.matches(string)

    def __reversed__(self):
        return self.reversed()

    def cardinality(self):
        '''
            Consider the regular expression as a set of strings and return the
            cardinality of that set, or raise an OverflowError if there are infinitely
            many.
        '''
        # There is no way to do this other than converting to an FSM, because the
        # pattern may allow duplicate routes, such as "a|a".
        return self.to_fsm().cardinality()

    def __len__(self):
        return self.cardinality()

    def strings(self, otherchar=None):
        '''
            Each time next() is called on this iterator, a new string is returned
            which will the present lego piece can match. StopIteration is raised once
            all such strings have been returned, although a regex with a * in may
            match infinitely many strings.
        '''

        # In the case of a regex like "[^abc]", there are infinitely many (well, a
        # very large finite number of) single characters which will match. It's not
        # productive to iterate over all of these giving every single example.
        # You must supply your own "otherchar" to stand in for all of these
        # possibilities.
        for string in self.to_fsm().strings():

            # Have to represent `fsm.anything_else` somehow.
            if fsm.anything_else in string:
                if otherchar == None:
                    raise Exception("Please choose an 'otherchar'")
                string = [
                    otherchar if char == fsm.anything_else else char
                    for char in string
                ]

            yield "".join(string)

    def __iter__(self):
        '''
            This allows you to do `for string in pattern1` as a list comprehension!
        '''
        return self.strings()

emptypattern = pattern(emptyconc)

# Special and useful values go here.

# A very special conc expressing the empty string, ""
emptystring = conc()
