"""
Finite state machine library, intended to be used by `greenery` only
"""

from __future__ import annotations

__all__ = (
    "ANYTHING_ELSE",
    "Fsm",
    "alpha_type",
    "epsilon",
    "null",
    "state_type",
)

from dataclasses import dataclass
from enum import Enum, auto
from functools import total_ordering
from typing import (
    Any,
    Callable,
    Collection,
    Iterable,
    Iterator,
    Mapping,
    TypeVar,
    Union,
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


class OblivionError(Exception):
    """
    This exception is thrown while `crawl()`ing an FSM if we transition to
    the oblivion state. For example while crawling two FSMs in parallel we
    may transition to the oblivion state of both FSMs at once. This
    warrants an out-of-band signal which will reduce the complexity of the
    new FSM's map.
    """

    pass


alpha_type = Union[str, AnythingElse]


state_type = Union[int, str, None]

M = TypeVar("M")
"""Meta-state type for crawl(). Can be anything."""


@dataclass(frozen=True, init=False)
class Fsm:
    """
    A Finite State Machine or FSM has an alphabet and a set of states. At
    any given moment, the FSM is in one state. When passed a symbol from
    the alphabet, the FSM jumps to another state (or possibly the same
    state). A map (Python dictionary) indicates where to jump.
    One state is nominated as a starting state. Zero or more states are
    nominated as final states. If, after consuming a string of symbols,
    the FSM is in a final state, then it is said to "accept" the string.
    This class also has some pretty powerful methods which allow FSMs to
    be concatenated, alternated between, multiplied, looped (Kleene star
    closure), intersected, and simplified.
    The majority of these methods are available using operator overloads.
    """

    alphabet: frozenset[alpha_type]
    states: frozenset[state_type]
    initial: state_type
    finals: frozenset[state_type]
    map: Mapping[state_type, Mapping[alpha_type, state_type]]

    # noinspection PyShadowingBuiltins
    # pylint: disable-next=too-many-arguments
    def __init__(
        self,
        /,
        *,
        alphabet: Iterable[alpha_type],
        states: Iterable[state_type],
        initial: state_type,
        finals: Iterable[state_type],
        # pylint: disable=redefined-builtin
        map: Mapping[state_type, Mapping[alpha_type, state_type]],
    ) -> None:
        """
        `alphabet` is an iterable of symbols the FSM can be fed.
        `states` is the set of states for the FSM
        `initial` is the initial state
        `finals` is the set of accepting states
        `map` may be sparse (i.e. it may omit transitions). In the case of
        omitted transitions, a non-final "oblivion" state is simulated.
        """
        alphabet = frozenset(alphabet)
        states = frozenset(states)
        finals = frozenset(finals)

        # Validation. Thanks to immutability, this only needs to be carried out
        # once.
        if initial not in states:
            raise Exception(f"Initial state {initial!r} must be one of {states!r}")
        if not finals.issubset(states):
            raise Exception(f"Final states {finals!r} must be a subset of {states!r}")
        for state, _state_trans in map.items():
            if state not in states:
                raise Exception(f"Transition from unknown state {state!r}")
            for symbol in map[state]:
                if symbol not in alphabet:
                    raise Exception(
                        f"Invalid symbol {symbol!r}"
                        f" in transition from {state!r}"
                        f" to {map[state][symbol]!r}"
                    )
                if not map[state][symbol] in states:
                    raise Exception(
                        f"Transition for state {state!r}"
                        f" and symbol {symbol!r}"
                        f" leads to {map[state][symbol]!r},"
                        " which is not a state"
                    )

        # Initialise the hard way due to immutability.
        object.__setattr__(self, "alphabet", alphabet)
        object.__setattr__(self, "states", states)
        object.__setattr__(self, "initial", initial)
        object.__setattr__(self, "finals", finals)
        object.__setattr__(self, "map", map)

    def accepts(self, input: Iterable[alpha_type], /) -> bool:
        """
        Test whether the present FSM accepts the supplied string (iterable
        of symbols). Equivalently, consider `self` as a possibly-infinite
        set of strings and test whether `string` is a member of it. This is
        actually mainly used for unit testing purposes. If `ANYTHING_ELSE`
        is in your alphabet, then any symbol not in your alphabet will be
        converted to `ANYTHING_ELSE`.
        """
        state = self.initial
        for symbol in input:
            if ANYTHING_ELSE in self.alphabet and symbol not in self.alphabet:
                symbol = ANYTHING_ELSE

            # Missing transition = transition to dead state
            if not (state in self.map and symbol in self.map[state]):
                return False

            state = self.map[state][symbol]
        return state in self.finals

    def __contains__(self, string: Iterable[alpha_type], /) -> bool:
        """
        This lets you use the syntax `"a" in fsm1` to see whether the
        string "a" is in the set of strings accepted by `fsm1`.
        """
        return self.accepts(string)

    def reduce(self, /) -> Fsm:
        """
        A result by Brzozowski (1963) shows that a minimal finite state
        machine equivalent to the original can be obtained by reversing the
        original twice.
        """
        return self.reversed().reversed()

    def __repr__(self, /) -> str:
        args = ", ".join(
            [
                f"alphabet={self.alphabet!r}",
                f"states={self.states!r}",
                f"initial={self.initial!r}",
                f"finals={self.finals!r}",
                f"map={self.map!r}",
            ]
        )
        return f"Fsm({args})"

    def __str__(self, /) -> str:
        rows = []

        sorted_alphabet = sorted(self.alphabet)

        # top row
        row = ["", "name", "final?"]
        row.extend(str(symbol) for symbol in sorted_alphabet)
        rows.append(row)

        # other rows
        for state in self.states:
            row = []
            if state == self.initial:
                row.append("*")
            else:
                row.append("")
            row.append(str(state))
            if state in self.finals:
                row.append("True")
            else:
                row.append("False")
            for symbol in sorted_alphabet:
                if state in self.map and symbol in self.map[state]:
                    row.append(str(self.map[state][symbol]))
                else:
                    row.append("")
            rows.append(row)

        # column widths
        colwidths = []
        for x in range(len(rows[0])):
            colwidths.append(max(len(str(rows[y][x])) for y in range(len(rows))) + 1)

        # apply padding
        for y in range(len(rows)):
            for x in range(len(rows[y])):
                rows[y][x] = rows[y][x].ljust(colwidths[x])

        # horizontal line
        rows.insert(1, ["-" * colwidth for colwidth in colwidths])

        return "".join("".join(row) + "\n" for row in rows)

    def concatenate(*fsms: Fsm) -> Fsm:
        """
        Concatenate arbitrarily many finite state machines together.
        """
        alphabet = set().union(*[fsm.alphabet for fsm in fsms])

        def connect_all(
            i: int,
            substate: state_type,
        ) -> Iterable[tuple[int, state_type]]:
            """
            Take a state in the numbered FSM and return a set containing
            it, plus (if it's final) the first state from the next FSM,
            plus (if that's final) the first state from the next but one
            FSM, plus...
            """
            result = {(i, substate)}
            while i < len(fsms) - 1 and substate in fsms[i].finals:
                i += 1
                substate = fsms[i].initial
                result.add((i, substate))
            return result

        # Use a superset containing states from all FSMs at once.
        # We start at the start of the first FSM. If this state is final in the
        # first FSM, then we are also at the start of the second FSM. And so
        # on.
        initial_: set[tuple[int, state_type]] = set()
        if len(fsms) > 0:
            initial_.update(connect_all(0, fsms[0].initial))
        initial: frozenset[tuple[int, state_type]] = frozenset(initial_)

        def final(state: frozenset[tuple[int, state_type]]) -> bool:
            """If you're in a final state of the final FSM, it's final"""
            for i, substate in state:
                if i == len(fsms) - 1 and substate in fsms[i].finals:
                    return True
            return False

        def follow(
            current: frozenset[tuple[int, state_type]],
            symbol: alpha_type,
        ) -> frozenset[tuple[int, state_type]]:
            """
            Follow the collection of states through all FSMs at once,
            jumping to the next FSM if we reach the end of the current one
            TODO: improve all follow() implementations to allow for dead
            metastates?
            """
            next: set[tuple[int, state_type]] = set()
            for i, substate in current:
                fsm = fsms[i]
                if substate in fsm.map:
                    if symbol in fsm.map[substate]:
                        next.update(connect_all(i, fsm.map[substate][symbol]))
                    elif (
                        ANYTHING_ELSE in fsm.map[substate]
                        and symbol not in fsm.alphabet
                    ):
                        next.update(connect_all(i, fsm.map[substate][ANYTHING_ELSE]))
            if len(next) == 0:
                raise OblivionError
            return frozenset(next)

        return crawl(alphabet, initial, final, follow).reduce()

    def __add__(self, other: Fsm, /) -> Fsm:
        """
        Concatenate two finite state machines together.
        For example, if self accepts "0*" and other accepts "1+(0|1)",
        will return a finite state machine accepting "0*1+(0|1)".
        Accomplished by effectively following non-deterministically.
        Call using "fsm3 = fsm1 + fsm2"
        """
        return self.concatenate(other)

    def star(self, /) -> Fsm:
        """
        If the present FSM accepts X, returns an FSM accepting X* (i.e. 0
        or more Xes). This is NOT as simple as naively connecting the final
        states back to the initial state: see (b*ab)* for example.
        """
        alphabet = self.alphabet

        initial: Collection[state_type] = {self.initial}

        def follow(
            state: Collection[state_type],
            symbol: alpha_type,
        ) -> Collection[state_type]:
            next = set()

            for substate in state:
                if substate in self.map and symbol in self.map[substate]:
                    next.add(self.map[substate][symbol])

                # If one of our substates is final, then we can also consider
                # transitions from the initial state of the original FSM.
                if (
                    substate in self.finals
                    and self.initial in self.map
                    and symbol in self.map[self.initial]
                ):
                    next.add(self.map[self.initial][symbol])

            if len(next) == 0:
                raise OblivionError

            return frozenset(next)

        def final(state: Collection[state_type]) -> bool:
            return any(substate in self.finals for substate in state)

        return crawl(alphabet, initial, final, follow) | epsilon(alphabet)

    def times(self, multiplier: int, /) -> Fsm:
        """
        Given an FSM and a multiplier, return the multiplied FSM.
        """
        if multiplier < 0:
            raise Exception(f"Can't multiply an FSM by {multiplier!r}")

        alphabet = self.alphabet

        # metastate is a set of iterations+states
        initial: Collection[tuple[state_type, int]] = {(self.initial, 0)}

        def final(state: Collection[tuple[state_type, int]]) -> bool:
            """
            If the initial state is final then multiplying doesn't alter
            that
            """
            for substate, iteration in state:
                if substate == self.initial and (
                    self.initial in self.finals or iteration == multiplier
                ):
                    return True
            return False

        def follow(
            current: Collection[tuple[state_type, int]],
            symbol: alpha_type,
        ) -> Collection[tuple[state_type, int]]:
            next = []
            for substate, iteration in current:
                if (
                    iteration < multiplier
                    and substate in self.map
                    and symbol in self.map[substate]
                ):
                    next.append((self.map[substate][symbol], iteration))
                    # final of self? merge with initial on next iteration
                    if self.map[substate][symbol] in self.finals:
                        next.append((self.initial, iteration + 1))
            if len(next) == 0:
                raise OblivionError
            return frozenset(next)

        return crawl(alphabet, initial, final, follow).reduce()

    def __mul__(self, multiplier: int, /) -> Fsm:
        """
        Given an FSM and a multiplier, return the multiplied FSM.
        """
        return self.times(multiplier)

    def union(*fsms: Fsm) -> Fsm:
        """
        Treat `fsms` as a collection of arbitrary FSMs and return the union
        FSM. Can be used as `fsm1.union(fsm2, ...)` or
        `fsm.union(fsm1, ...)`. `fsms` may be empty.
        """
        return parallel(fsms, any)

    def __or__(self, other: Fsm, /) -> Fsm:
        """
        Alternation.
        Return a finite state machine which accepts any sequence of symbols
        that is accepted by either self or other. Note that the set of
        strings recognised by the two FSMs undergoes a set union.
        Call using "fsm3 = fsm1 | fsm2"
        """
        return self.union(other)

    def intersection(*fsms: Fsm) -> Fsm:
        """
        Intersection.
        Take FSMs and AND them together. That is, return an FSM which
        accepts any sequence of symbols that is accepted by both of the
        original FSMs. Note that the set of strings recognised by the two
        FSMs undergoes a set intersection operation.
        Call using "fsm3 = fsm1 & fsm2"
        """
        return parallel(fsms, all)

    def __and__(self, other: Fsm, /) -> Fsm:
        """
        Treat the FSMs as sets of strings and return the intersection of
        those sets in the form of a new FSM.
        """
        return self.intersection(other)

    def symmetric_difference(*fsms: Fsm) -> Fsm:
        """
        Treat `fsms` as a collection of sets of strings and compute the
        symmetric difference of them all. The python set method only allows
        two sets to be operated on at once, but we go the extra mile since
        it's not too hard.
        """
        return parallel(fsms, lambda accepts: (accepts.count(True) % 2) == 1)

    def __xor__(self, other: Fsm, /) -> Fsm:
        """
        Symmetric difference. Returns an FSM which recognises only the
        strings recognised by `self` or `other` but not both.
        """
        return self.symmetric_difference(other)

    def everythingbut(self, /) -> Fsm:
        """
        Return a finite state machine which will accept any string NOT
        accepted by self, and will not accept any string accepted by self.
        This is more complicated if there are missing transitions, because
        the missing "dead" state must now be reified.
        """
        alphabet = self.alphabet

        initial: Mapping[int, state_type] = {0: self.initial}

        def follow(
            current: Mapping[int, state_type],
            symbol: alpha_type,
        ) -> Mapping[int, state_type]:
            next = {}
            if (
                0 in current
                and current[0] in self.map
                and symbol in self.map[current[0]]
            ):
                next[0] = self.map[current[0]][symbol]
            return next

        # state is final unless the original was
        def final(state: Mapping[int, state_type]) -> bool:
            return not (0 in state and state[0] in self.finals)

        return crawl(alphabet, initial, final, follow).reduce()

    def reversed(self, /) -> Fsm:
        """
        Return a new FSM such that for every string that self accepts (e.g.
        "beer", the new FSM accepts the reversed string ("reeb").
        """
        alphabet = self.alphabet

        # Start from a composite "state-set" consisting of all final states.
        # If there are no final states, this set is empty and we'll find that
        # no other states get generated.
        initial = frozenset(self.finals)

        # Find every possible way to reach the current state-set
        # using this symbol.
        def follow(
            current: frozenset[state_type],
            symbol: alpha_type,
        ) -> frozenset[state_type]:
            next = frozenset(
                [
                    prev
                    for prev in self.map
                    for state in current
                    if symbol in self.map[prev] and self.map[prev][symbol] == state
                ]
            )
            if len(next) == 0:
                raise OblivionError
            return next

        # A state-set is final if the initial state is in it.
        def final(state: frozenset[state_type]) -> bool:
            return self.initial in state

        # Man, crawl() is the best!
        return crawl(alphabet, initial, final, follow)
        # Do not reduce() the result, since reduce() calls us in turn

    def __reversed__(self, /) -> Fsm:
        """
        Return a new FSM such that for every string that self accepts (e.g.
        "beer", the new FSM accepts the reversed string ("reeb").
        """
        return self.reversed()

    def islive(self, /, state: state_type) -> bool:
        """A state is "live" if a final state can be reached from it."""
        reachable = [state]
        i = 0
        while i < len(reachable):
            current = reachable[i]
            if current in self.finals:
                return True
            if current in self.map:
                for symbol in self.map[current]:
                    next = self.map[current][symbol]
                    if next not in reachable:
                        reachable.append(next)
            i += 1
        return False

    def empty(self, /) -> bool:
        """
        An FSM is empty if it recognises no strings. An FSM may be
        arbitrarily complicated and have arbitrarily many final states
        while still recognising no strings because those final states may
        all be inaccessible from the initial state. Equally, an FSM may be
        non-empty despite having an empty alphabet if the initial state is
        final.
        """
        return not self.islive(self.initial)

    def equivalent(self, other: Fsm, /) -> bool:
        """
        Two FSMs are considered equivalent if they recognise the same
        strings. Or, to put it another way, if their symmetric difference
        recognises no strings.
        """
        return (self ^ other).empty()

    def __eq__(self, other: object, /) -> bool:
        """
        You can use `fsm1 == fsm2` to determine whether two FSMs recognise
        the same strings.
        """
        if not isinstance(other, Fsm):
            return NotImplemented
        return self.equivalent(other)

    def different(self, other: Fsm, /) -> bool:
        """
        Two FSMs are considered different if they have a non-empty
        symmetric difference.
        """
        return not (self ^ other).empty()

    def __ne__(self, other: object, /) -> bool:
        """
        Use `fsm1 != fsm2` to determine whether two FSMs recognise
        different strings.
        """
        return not self == other

    def difference(*fsms: Fsm) -> Fsm:
        """
        Difference. Returns an FSM which recognises only the strings
        recognised by the first FSM in the list, but none of the others.
        """
        return parallel(fsms, lambda accepts: accepts[0] and not any(accepts[1:]))

    def __sub__(self, other: Fsm, /) -> Fsm:
        return self.difference(other)

    def isdisjoint(self, other: Fsm, /) -> bool:
        """
        Treat `self` and `other` as sets of strings and see if they are
        disjoint
        """
        return (self & other).empty()

    def issubset(self, other: Fsm, /) -> bool:
        """
        Treat `self` and `other` as sets of strings and see if `self` is a
        subset of `other`... `self` recognises no strings which `other`
        doesn't.
        """
        return (self - other).empty()

    def __le__(self, other: Fsm, /) -> bool:
        """
        Treat `self` and `other` as sets of strings and see if `self` is a
        subset of `other`... `self` recognises no strings which `other`
        doesn't.
        """
        return self.issubset(other)

    def ispropersubset(self, other: Fsm, /) -> bool:
        """
        Treat `self` and `other` as sets of strings and see if `self` is a
        proper subset of `other`.
        """
        return self <= other and self != other

    def __lt__(self, other: Fsm, /) -> bool:
        """
        Treat `self` and `other` as sets of strings and see if `self` is a
        strict subset of `other`.
        """
        return self.ispropersubset(other)

    def issuperset(self, other: Fsm, /) -> bool:
        """
        Treat `self` and `other` as sets of strings and see if `self` is a
        superset of `other`.
        """
        return (other - self).empty()

    def __ge__(self, other: Fsm, /) -> bool:
        """
        Treat `self` and `other` as sets of strings and see if `self` is a
        superset of `other`.
        """
        return self.issuperset(other)

    def ispropersuperset(self, other: Fsm, /) -> bool:
        """
        Treat `self` and `other` as sets of strings and see if `self` is a
        proper superset of `other`.
        """
        return self >= other and self != other

    def __gt__(self, other: Fsm, /) -> bool:
        """
        Treat `self` and `other` as sets of strings and see if `self` is a
        strict superset of `other`.
        """
        return self.ispropersuperset(other)

    def copy(self, /) -> Fsm:
        """
        For completeness only, since `set.copy()` and `frozenset.copy()` exist.
        FSM objects are immutable; like `frozenset`, this just returns `self`.
        """
        return self

    __copy__ = copy

    def derive(self, input: Iterable[alpha_type], /) -> Fsm:
        """
        Compute the Brzozowski derivative of this FSM with respect to the
        input string of symbols.
        <https://en.wikipedia.org/wiki/Brzozowski_derivative>
        If any of the symbols are not members of the alphabet, that's a
        `KeyError`. If you fall into oblivion, then the derivative is an
        FSM accepting no strings.
        """
        try:
            # Consume the input string.
            state = self.initial
            for symbol in input:
                if symbol not in self.alphabet:
                    if ANYTHING_ELSE not in self.alphabet:
                        raise KeyError(symbol)
                    symbol = ANYTHING_ELSE

                # Missing transition = transition to dead state
                if not (state in self.map and symbol in self.map[state]):
                    raise OblivionError

                state = self.map[state][symbol]

            # OK so now we have consumed that string, use the new location as
            # the starting point.
            return Fsm(
                alphabet=self.alphabet,
                states=self.states,
                initial=state,
                finals=self.finals,
                map=self.map,
            )

        except OblivionError:
            # Fell out of the FSM. The derivative of this FSM is the empty FSM.
            return null(self.alphabet)


def null(alphabet: Iterable[alpha_type]) -> Fsm:
    """
    An FSM accepting nothing (not even the empty string). This is
    demonstrates that this is possible, and is also extremely useful
    in some situations
    """
    return Fsm(
        alphabet=alphabet,
        states={0},
        initial=0,
        finals=(),
        map={
            0: dict([(symbol, 0) for symbol in alphabet]),
        },
    )


def epsilon(alphabet: Iterable[alpha_type]) -> Fsm:
    """
    Return an FSM matching an empty string, "", only.
    This is very useful in many situations
    """
    return Fsm(
        alphabet=alphabet,
        states={0},
        initial=0,
        finals={0},
        map={},
    )


def parallel(
    fsms: tuple[Fsm, ...],
    test: Callable[[list[bool]], bool],
    /,
) -> Fsm:
    """
    Crawl several FSMs in parallel, mapping the states of a larger
    meta-FSM. To determine whether a state in the larger FSM is final, pass
    all of the finality statuses (e.g. [True, False, False] to `test`.
    """
    alphabet = set().union(*[fsm.alphabet for fsm in fsms])

    initial: Mapping[int, state_type] = dict(
        [(i, fsm.initial) for i, fsm in enumerate(fsms)]
    )

    # dedicated function accepts a "superset" and returns the next "superset"
    # obtained by following this transition in the new FSM
    def follow(
        current: Mapping[int, state_type],
        symbol: alpha_type,
    ) -> Mapping[int, state_type]:
        next = {}
        for i in range(len(fsms)):
            actual_symbol: alpha_type
            if symbol not in fsms[i].alphabet and ANYTHING_ELSE in fsms[i].alphabet:
                actual_symbol = ANYTHING_ELSE
            else:
                actual_symbol = symbol
            if (
                i in current
                and current[i] in fsms[i].map
                and actual_symbol in fsms[i].map[current[i]]
            ):
                next[i] = fsms[i].map[current[i]][actual_symbol]
        if len(next.keys()) == 0:
            raise OblivionError
        return next

    # Determine the "is final?" condition of each substate, then pass it to the
    # test to determine finality of the overall FSM.
    def final(state: Mapping[int, state_type]) -> bool:
        accepts = [i in state and state[i] in fsm.finals for i, fsm in enumerate(fsms)]
        return test(accepts)

    return crawl(alphabet, initial, final, follow).reduce()


def crawl(
    alphabet: Iterable[alpha_type],
    initial: M,
    final: Callable[[M], bool],
    follow: Callable[[M, alpha_type], M],
) -> Fsm:
    """
    Given the above conditions and instructions, crawl a new unknown FSM,
    mapping its states, final states and transitions. Return the new FSM.
    This is a pretty powerful procedure which could potentially go on
    forever if you supply an evil version of follow().
    """

    states: list[M] = [initial]
    finals: set[state_type] = set()
    map: dict[state_type, dict[alpha_type, state_type]] = {}

    # iterate over a growing list
    i = 0
    while i < len(states):
        state = states[i]

        # add to finals
        if final(state):
            finals.add(i)

        # compute map for this state
        map[i] = {}
        for symbol in sorted(alphabet):
            try:
                next = follow(state, symbol)

                try:
                    j = states.index(next)
                except ValueError:
                    j = len(states)
                    states.append(next)

            except OblivionError:
                # Reached an oblivion state. Don't list it.
                continue

            map[i][symbol] = j

        i += 1

    return Fsm(
        alphabet=alphabet,
        states=set(range(len(states))),
        initial=0,
        finals=finals,
        map=map,
    )
