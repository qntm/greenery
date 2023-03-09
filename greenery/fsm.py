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
from typing import Union
from .alphabet import ANYTHING_ELSE, Alphabet, AnythingElse, alpha_type, event_type


# mypy: allow-incomplete-defs
# mypy: allow-untyped-calls
# mypy: allow-untyped-defs
# mypy: no-check-untyped-defs


class OblivionError(Exception):
    """
    This exception is thrown while `crawl()`ing an FSM if we transition to
    the oblivion state. For example while crawling two FSMs in parallel we
    may transition to the oblivion state of both FSMs at once. This
    warrants an out-of-band signal which will reduce the complexity of the
    new FSM's map.
    """

    pass


state_type = Union[int, str, None]


@dataclass(frozen=True)
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

    initial: state_type
    finals: set[state_type]
    alphabet: Alphabet
    states: set[state_type]
    map: dict[state_type, dict[event_type, state_type]]

    def __post_init__(self):
        """
        `alphabet` is an instance of Alphabet
        `states` is the set of states for the FSM
        `initial` is the initial state
        `finals` is the set of accepting states
        `map` may be sparse (i.e. it may omit transitions). In the case of
        omitted transitions, a non-final "oblivion" state is simulated.
        """

        # Initialise the hard way due to immutability.
        if not isinstance(self.alphabet, Alphabet):
            object.__setattr__(self, "alphabet", Alphabet.distinct(self.alphabet))
            translate_symbols = True
        else:
            translate_symbols = False
        object.__setattr__(self, "states", set(self.states))

        object.__setattr__(self, "finals", set(self.finals))
        # Validation. Thanks to immutability, this only needs to be carried out
        # once.
        if self.initial not in self.states:
            raise Exception(
                f"Initial state {self.initial!r}"
                f" must be one of {self.states!r}"
            )
        if not self.finals.issubset(self.states):
            raise Exception(
                f"Final states {self.finals!r}"
                f" must be a subset of {self.states!r}"
            )
        for state, _state_trans in self.map.items():
            if state not in self.states:
                raise Exception(f"Transition from unknown state {state!r}")
            if not translate_symbols:
                for event in self.map[state]:
                    if event not in self.alphabet.events:
                        raise Exception(
                            f"Invalid event {event!r}"
                            f" in transition from {state!r}"
                            f" to {self.map[state][event]!r}"
                        )
                    if not self.map[state][event] in self.states:
                        raise Exception(
                            f"Transition for state {state!r}"
                            f" and transition {event!r}"
                            f" leads to {self.map[state][event]!r},"
                            " which is not a state"
                        )
            else:
                new_transition = {}
                for symbol in self.map[state]:
                    if symbol not in self.alphabet:
                        raise Exception(
                            f"Invalid symbol {symbol!r}"
                            f" in transition from {state!r}"
                            f" to {self.map[state][symbol]!r}"
                        )
                    if not self.map[state][symbol] in self.states:
                        raise Exception(
                            f"Transition for state {state!r}"
                            f" and transition {symbol!r}"
                            f" leads to {self.map[state][symbol]!r},"
                            " which is not a state"
                        )
                    new_transition[self.alphabet[symbol]] = self.map[state][symbol]
                self.map[state] = new_transition

    def accepts(self, input):
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
            event = self.alphabet[symbol]

            # Missing transition = transition to dead state
            if not (state in self.map and event in self.map[state]):
                return False

            state = self.map[state][event]
        return state in self.finals

    def __contains__(self, string):
        """
        This lets you use the syntax `"a" in fsm1` to see whether the
        string "a" is in the set of strings accepted by `fsm1`.
        """
        return self.accepts(string)

    def reduce(self):
        """
        A result by Brzozowski (1963) shows that a minimal finite state
        machine equivalent to the original can be obtained by reversing the
        original twice.
        """
        return self.reversed().reversed()

    def __repr__(self):
        args = ", ".join([
            f"alphabet={self.alphabet!r}",
            f"states={self.states!r}",
            f"initial={self.initial!r}",
            f"finals={self.finals!r}",
            f"map={self.map!r}",
        ])
        return f"Fsm({args})"

    def __str__(self):
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
                if state in self.map and self.alphabet[symbol] in self.map[state]:
                    row.append(str(self.map[state][self.alphabet[symbol]]))
                else:
                    row.append("")
            rows.append(row)

        # column widths
        colwidths = []
        for x in range(len(rows[0])):
            colwidths.append(max(
                len(str(rows[y][x])) for y in range(len(rows))
            ) + 1)

        # apply padding
        for y in range(len(rows)):
            for x in range(len(rows[y])):
                rows[y][x] = rows[y][x].ljust(colwidths[x])

        # horizontal line
        rows.insert(1, ["-" * colwidth for colwidth in colwidths])

        return "".join("".join(row) + "\n" for row in rows)

    def concatenate(*fsms):
        """
        Concatenate arbitrarily many finite state machines together.
        """
        alphabet, new_to_old = Alphabet.union(*[fsm.alphabet for fsm in fsms])

        def connect_all(i, substate):
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
        initial = set()
        if len(fsms) > 0:
            initial.update(connect_all(0, fsms[0].initial))
        initial = frozenset(initial)

        def final(state):
            """If you're in a final state of the final FSM, it's final"""
            for (i, substate) in state:
                if i == len(fsms) - 1 and substate in fsms[i].finals:
                    return True
            return False

        def follow(current, new_event):
            """
            Follow the collection of states through all FSMs at once,
            jumping to the next FSM if we reach the end of the current one
            TODO: improve all follow() implementations to allow for dead
            metastates?
            """
            next = set()
            for (i, substate) in current:
                fsm = fsms[i]
                subevent = new_to_old[i][new_event]
                if substate in fsm.map and subevent in fsm.map[substate]:
                    next.update(connect_all(i, fsm.map[substate][subevent]))
            if len(next) == 0:
                raise OblivionError
            return frozenset(next)

        return crawl(alphabet, initial, final, follow).reduce()

    def __add__(self, other):
        """
        Concatenate two finite state machines together.
        For example, if self accepts "0*" and other accepts "1+(0|1)",
        will return a finite state machine accepting "0*1+(0|1)".
        Accomplished by effectively following non-deterministically.
        Call using "fsm3 = fsm1 + fsm2"
        """
        return self.concatenate(other)

    def star(self):
        """
        If the present FSM accepts X, returns an FSM accepting X* (i.e. 0
        or more Xes). This is NOT as simple as naively connecting the final
        states back to the initial state: see (b*ab)* for example.
        """
        alphabet = self.alphabet

        initial = {self.initial}

        def follow(state, event):
            next = set()
            for substate in state:
                if substate in self.map and event in self.map[substate]:
                    next.add(self.map[substate][event])

                # If one of our substates is final, then we can also consider
                # transitions from the initial state of the original FSM.
                if substate in self.finals \
                   and self.initial in self.map \
                   and event in self.map[self.initial]:
                    next.add(self.map[self.initial][event])

            if len(next) == 0:
                raise OblivionError

            return frozenset(next)

        def final(state):
            return any(substate in self.finals for substate in state)

        return crawl(alphabet, initial, final, follow) | epsilon(alphabet)

    def times(self, multiplier):
        """
        Given an FSM and a multiplier, return the multiplied FSM.
        """
        if multiplier < 0:
            raise Exception(f"Can't multiply an FSM by {multiplier!r}")

        alphabet = self.alphabet

        # metastate is a set of iterations+states
        initial = {(self.initial, 0)}

        def final(state):
            """
            If the initial state is final then multiplying doesn't alter
            that
            """
            for (substate, iteration) in state:
                if substate == self.initial \
                   and (
                       self.initial in self.finals
                       or iteration == multiplier
                   ):
                    return True
            return False

        def follow(current, event):
            next = []
            for (substate, iteration) in current:
                if iteration < multiplier \
                   and substate in self.map \
                   and event in self.map[substate]:
                    next.append((self.map[substate][event], iteration))
                    # final of self? merge with initial on next iteration
                    if self.map[substate][event] in self.finals:
                        next.append((self.initial, iteration + 1))
            if len(next) == 0:
                raise OblivionError
            return frozenset(next)

        return crawl(alphabet, initial, final, follow).reduce()

    def __mul__(self, multiplier):
        """
        Given an FSM and a multiplier, return the multiplied FSM.
        """
        return self.times(multiplier)

    def union(*fsms):
        """
        Treat `fsms` as a collection of arbitrary FSMs and return the union
        FSM. Can be used as `fsm1.union(fsm2, ...)` or
        `fsm.union(fsm1, ...)`. `fsms` may be empty.
        """
        return parallel(fsms, any)

    def __or__(self, other):
        """
        Alternation.
        Return a finite state machine which accepts any sequence of symbols
        that is accepted by either self or other. Note that the set of
        strings recognised by the two FSMs undergoes a set union.
        Call using "fsm3 = fsm1 | fsm2"
        """
        return self.union(other)

    def intersection(*fsms):
        """
        Intersection.
        Take FSMs and AND them together. That is, return an FSM which
        accepts any sequence of symbols that is accepted by both of the
        original FSMs. Note that the set of strings recognised by the two
        FSMs undergoes a set intersection operation.
        Call using "fsm3 = fsm1 & fsm2"
        """
        return parallel(fsms, all)

    def __and__(self, other):
        """
        Treat the FSMs as sets of strings and return the intersection of
        those sets in the form of a new FSM.
        """
        return self.intersection(other)

    def symmetric_difference(*fsms):
        """
        Treat `fsms` as a collection of sets of strings and compute the
        symmetric difference of them all. The python set method only allows
        two sets to be operated on at once, but we go the extra mile since
        it's not too hard.
        """
        return parallel(fsms, lambda accepts: (accepts.count(True) % 2) == 1)

    def __xor__(self, other):
        """
        Symmetric difference. Returns an FSM which recognises only the
        strings recognised by `self` or `other` but not both.
        """
        return self.symmetric_difference(other)

    def everythingbut(self):
        """
        Return a finite state machine which will accept any string NOT
        accepted by self, and will not accept any string accepted by self.
        This is more complicated if there are missing transitions, because
        the missing "dead" state must now be reified.
        """
        alphabet = self.alphabet

        initial = {0: self.initial}

        def follow(current, event):
            next = {}
            if 0 in current \
               and current[0] in self.map \
               and event in self.map[current[0]]:
                next[0] = self.map[current[0]][event]
            return next

        # state is final unless the original was
        def final(state):
            return not (0 in state and state[0] in self.finals)

        return crawl(alphabet, initial, final, follow).reduce()

    def reversed(self):
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
        def follow(current, event):
            next = frozenset([
                prev
                for prev in self.map
                for state in current
                if event in self.map[prev] and self.map[prev][event] == state
            ])
            if len(next) == 0:
                raise OblivionError
            return next

        # A state-set is final if the initial state is in it.
        def final(state):
            return self.initial in state

        # Man, crawl() is the best!
        return crawl(alphabet, initial, final, follow)
        # Do not reduce() the result, since reduce() calls us in turn

    def __reversed__(self):
        """
        Return a new FSM such that for every string that self accepts (e.g.
        "beer", the new FSM accepts the reversed string ("reeb").
        """
        return self.reversed()

    def islive(self, state):
        """A state is "live" if a final state can be reached from it."""
        reachable = [state]
        i = 0
        while i < len(reachable):
            current = reachable[i]
            if current in self.finals:
                return True
            if current in self.map:
                for event in self.map[current]:
                    next = self.map[current][event]
                    if next not in reachable:
                        reachable.append(next)
            i += 1
        return False

    def empty(self):
        """
        An FSM is empty if it recognises no strings. An FSM may be
        arbitrarily complicated and have arbitrarily many final states
        while still recognising no strings because those final states may
        all be inaccessible from the initial state. Equally, an FSM may be
        non-empty despite having an empty alphabet if the initial state is
        final.
        """
        return not self.islive(self.initial)

    def strings(self):
        """
        Generate strings (lists of symbols) that this FSM accepts. Since
        there may be infinitely many of these we use a generator instead of
        constructing a static list. Strings will be sorted in order of
        length and then lexically. This procedure uses arbitrary amounts of
        memory but is very fast. There may be more efficient ways to do
        this, that I haven't investigated yet. You can use this in list
        comprehensions.
        """

        # Many FSMs have "dead states". Once you reach a dead state, you can no
        # longer reach a final state. Since many strings may end up here, it's
        # advantageous to constrain our search to live states only.
        livestates = set(state for state in self.states if self.islive(state))

        # We store a list of tuples. Each tuple consists of an input string and
        # the state that this input string leads to. This means we don't have
        # to run the state machine from the very beginning every time we want
        # to check a new string.
        strings = []

        # Initial entry (or possibly not, in which case this is a short one)
        cstate = self.initial
        cstring = []
        if cstate in livestates:
            if cstate in self.finals:
                yield cstring
            strings.append((cstring, cstate))

        # Fixed point calculation
        i = 0
        while i < len(strings):
            (cstring, cstate) = strings[i]
            if cstate in self.map:
                # It's maybe possible to not duplicate the entries in strings and just storing the events
                # instead of the symbols, however, then it becomes a lot harder to guarantee
                # the lexical ordering
                relevant_symbols = sorted((s, e) for e in self.map[cstate] for s in self.alphabet.events[e])
                for symbol, event in relevant_symbols:
                    nstate = self.map[cstate][event]
                    nstring = cstring + [symbol]
                    if nstate in livestates:
                        if nstate in self.finals:
                            yield nstring
                        strings.append((nstring, nstate))
            i += 1

    def __iter__(self):
        """
        This allows you to do `for string in fsm1` as a list comprehension!
        """
        return self.strings()

    def equivalent(self, other):
        """
        Two FSMs are considered equivalent if they recognise the same
        strings. Or, to put it another way, if their symmetric difference
        recognises no strings.
        """
        return (self ^ other).empty()

    def __eq__(self, other):
        """
        You can use `fsm1 == fsm2` to determine whether two FSMs recognise
        the same strings.
        """
        return self.equivalent(other)

    def different(self, other):
        """
        Two FSMs are considered different if they have a non-empty
        symmetric difference.
        """
        return not (self ^ other).empty()

    def __ne__(self, other):
        """
        Use `fsm1 != fsm2` to determine whether two FSMs recognise
        different strings.
        """
        return self.different(other)

    def difference(*fsms):
        """
        Difference. Returns an FSM which recognises only the strings
        recognised by the first FSM in the list, but none of the others.
        """
        return parallel(
            fsms,
            lambda accepts: accepts[0] and not any(accepts[1:])
        )

    def __sub__(self, other):
        return self.difference(other)

    def cardinality(self):
        """
        Consider the FSM as a set of strings and return the cardinality of
        that set, or raise an OverflowError if there are infinitely many
        """
        num_strings = {}

        def get_num_strings(state):
            # Many FSMs have at least one oblivion state
            if self.islive(state):
                if state in num_strings:
                    if num_strings[state] is None:  # "computing..."
                        # Recursion! There are infinitely many strings
                        # recognised
                        raise OverflowError(state)
                    return num_strings[state]
                num_strings[state] = None  # i.e. "computing..."

                n = 0
                if state in self.finals:
                    n += 1
                if state in self.map:
                    for event in self.map[state]:
                        # When going this path, we have len(self.alphabet.events[event]) choices
                        n += get_num_strings(self.map[state][event]) * len(self.alphabet.events[event])
                num_strings[state] = n

            else:
                # Dead state
                num_strings[state] = 0

            return num_strings[state]

        return get_num_strings(self.initial)

    def __len__(self):
        """
        Consider the FSM as a set of strings and return the cardinality of
        that set, or raise an OverflowError if there are infinitely many
        """
        return self.cardinality()

    def isdisjoint(self, other):
        """
        Treat `self` and `other` as sets of strings and see if they are
        disjoint
        """
        return (self & other).empty()

    def issubset(self, other):
        """
        Treat `self` and `other` as sets of strings and see if `self` is a
        subset of `other`... `self` recognises no strings which `other`
        doesn't.
        """
        return (self - other).empty()

    def __le__(self, other):
        """
        Treat `self` and `other` as sets of strings and see if `self` is a
        subset of `other`... `self` recognises no strings which `other`
        doesn't.
        """
        return self.issubset(other)

    def ispropersubset(self, other):
        """
        Treat `self` and `other` as sets of strings and see if `self` is a
        proper subset of `other`.
        """
        return self <= other and self != other

    def __lt__(self, other):
        """
        Treat `self` and `other` as sets of strings and see if `self` is a
        strict subset of `other`.
        """
        return self.ispropersubset(other)

    def issuperset(self, other):
        """
        Treat `self` and `other` as sets of strings and see if `self` is a
        superset of `other`.
        """
        return (other - self).empty()

    def __ge__(self, other):
        """
        Treat `self` and `other` as sets of strings and see if `self` is a
        superset of `other`.
        """
        return self.issuperset(other)

    def ispropersuperset(self, other):
        """
        Treat `self` and `other` as sets of strings and see if `self` is a
        proper superset of `other`.
        """
        return self >= other and self != other

    def __gt__(self, other):
        """
        Treat `self` and `other` as sets of strings and see if `self` is a
        strict superset of `other`.
        """
        return self.ispropersuperset(other)

    def copy(self):
        """
        For completeness only, since `set.copy()` also exists. FSM objects
        are immutable, so I can see only very odd reasons to need this.
        """
        return Fsm(
            alphabet=self.alphabet,
            states=self.states,
            initial=self.initial,
            finals=self.finals,
            map=self.map,
        )

    def derive(self, input):
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
                event = self.alphabet[symbol]

                # Missing transition = transition to dead state
                if not (state in self.map and event in self.map[state]):
                    raise OblivionError

                state = self.map[state][event]

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


def null(alphabet):
    """
    An FSM accepting nothing (not even the empty string). This is
    demonstrates that this is possible, and is also extremely useful
    in some situations
    """
    if not isinstance(alphabet, Alphabet):
        alphabet = Alphabet.distinct(alphabet)
    return Fsm(
        alphabet=alphabet,
        states={0},
        initial=0,
        finals=set(),
        map={
            0: {event: 0 for event in alphabet.events},
        },
    )


def epsilon(alphabet):
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


def parallel(fsms, test):
    """
    Crawl several FSMs in parallel, mapping the states of a larger
    meta-FSM. To determine whether a state in the larger FSM is final, pass
    all of the finality statuses (e.g. [True, False, False] to `test`.
    """
    alphabet, new_to_old = Alphabet.union(*[fsm.alphabet for fsm in fsms])

    initial = dict([(i, fsm.initial) for (i, fsm) in enumerate(fsms)])

    # dedicated function accepts a "superset" and returns the next "superset"
    # obtained by following this transition in the new FSM
    def follow(current, new_event):
        next = {}
        for i in range(len(fsms)):
            actual_event = new_to_old[i][new_event]
            if i in current \
               and current[i] in fsms[i].map \
               and actual_event in fsms[i].map[current[i]]:
                next[i] = fsms[i].map[current[i]][actual_event]
        if len(next.keys()) == 0:
            raise OblivionError
        return next

    # Determine the "is final?" condition of each substate, then pass it to the
    # test to determine finality of the overall FSM.
    def final(state):
        accepts = [
            i in state and state[i] in fsm.finals
            for (i, fsm) in enumerate(fsms)
        ]
        return test(accepts)

    return crawl(alphabet, initial, final, follow).reduce()


def crawl(alphabet, initial, final, follow):
    """
    Given the above conditions and instructions, crawl a new unknown FSM,
    mapping its states, final states and transitions. Return the new FSM.
    This is a pretty powerful procedure which could potentially go on
    forever if you supply an evil version of follow().
    """

    states = [initial]
    finals = set()
    map = {}

    # iterate over a growing list
    i = 0
    while i < len(states):
        state = states[i]

        # add to finals
        if final(state):
            finals.add(i)

        # compute map for this state
        map[i] = {}
        for event in alphabet.events:
            try:
                next = follow(state, event)

                try:
                    j = states.index(next)
                except ValueError:
                    j = len(states)
                    states.append(next)

            except OblivionError:
                # Reached an oblivion state. Don't list it.
                continue

            map[i][event] = j

        i += 1

    return Fsm(
        alphabet=alphabet,
        states=set(range(len(states))),
        initial=0,
        finals=finals,
        map=map,
    )
