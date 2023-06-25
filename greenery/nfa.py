import greenery
from greenery import rxelems as rx
from collections import defaultdict

class State:
    def __init__(self, is_accept=False):
        self.is_accept = is_accept
        self.transitions = defaultdict(set)

    def add_transition(self, symbol, state):
        self.transitions[symbol].add(state)

    def __repr__(self):
        return '{' + ', '.join(f'{c} -> ...' for c in self.transitions) + '}'


class NFA:
    def __init__(self, start, accept):
        self.start = start
        self.accept = accept


class StateSet:
    def __init__(self, states):
        self.states = self.epsilon_closure(states)

    def forward(self, char):
        next_states = set()
        for state in self.states:
            for char_class, trans in state.transitions.items():
                if char_class is not None and \
                        (char in char_class.chars) != char_class.negated:
                    next_states |= trans
        return StateSet(self.epsilon_closure(next_states))

    @classmethod
    def epsilon_closure(cls, states):
        closure = set(states)
        stack = list(states)
        while stack:
            state = stack.pop()
            for next_state in state.transitions[None]:
                if next_state not in closure:
                    closure.add(next_state)
                    stack.append(next_state)
        return closure

    def is_accept(self):
        return any(state.is_accept for state in self.states)

    def is_dead(self):
        return not self.states

    def __repr__(self):
        return '[' + ', '.join(repr(s) for s in self.states) + ']'


def nfa_simulate(start_state, input_string):
    states = StateSet({start_state})
    for char in input_string:
        states = states.forward(char)
    return states.is_accept()


def regex_to_nfa(regex):
    pattern = greenery.parse(regex)
    nfa = thompson_construction(pattern)
    nfa.accept.is_accept = True
    return nfa.start


def thompson_construction(pattern):
    # See https://en.wikipedia.org/wiki/Thompson%27s_construction
    # for the idea beind this function.
    match pattern:
        # A `Pattern` (also known as an "alt", short for "alternation") is a
        # set of `Conc`s. A `Pattern` expresses multiple alternate possibilities.
        # When written out as a regex, these would separated by pipes. A
        # `Pattern` containing no possibilities is possible and represents a
        # regular expression matching no strings whatsoever (there is no
        # conventional string form for this).
        case rx.Pattern():
            nfa = NFA(State(), State())
            for conc in pattern.concs:
                sub = thompson_construction(conc)
                nfa.start.add_transition(None, sub.start)
                sub.accept.add_transition(None, nfa.accept)
            return nfa

        case rx.Charclass():
            nfa = NFA(State(), State())
            nfa.start.add_transition(pattern, nfa.accept)
            return nfa

        # A `Conc` (short for "concatenation") is a tuple of `Mult`s i.e. an
        # unbroken string of mults occurring one after the other.
        # e.g. abcde[^fg]*h{4}[a-z]+(subpattern)(subpattern2)
        # To express the empty string, use an empty `Conc`, Conc().
        case rx.Conc():
            nfa = NFA(State(), State())
            nfa.start.add_transition(None, nfa.accept)
            for sub in pattern.mults:
                next_nfa = thompson_construction(sub)
                nfa.accept.add_transition(None, next_nfa.start)
                nfa.accept = next_nfa.accept
            return nfa

        # A `Mult` is a combination of a multiplicand with a multiplier (a min
        # and a max). The vast majority of characters in regular expressions
        # occur without a specific multiplier, which is implicitly equivalent to
        # a min of 1 and a max of 1, but many more have explicit multipliers like
        # "*" (min = 0, max = INF) and so on.
        case rx.Mult(sub, rx.Multiplier(rx.Bound(a), rx.Bound(b))):
            # Not strictly necessary, but makes the graph a bit tighter
            if a == b == 1:
                return thompson_construction(sub)
            # First handle the mandatory part
            if a > 0:
                new_mult = rx.Multiplier(rx.Bound(0), rx.Bound(None if b is None else b-a))
                conc = [sub for _ in range(a)] + [rx.Mult(sub, new_mult)]
                return thompson_construction(rx.Conc(*conc))
            # Then handle the a=0 case
            outer = NFA(State(), State())
            # If no upper limit, add a Kleene star
            if b is None:
                inner = thompson_construction(sub)
                inner.accept.add_transition(None, inner.start)
                # Add outer nfa, allowing to skip the whole thing
                outer.start.add_transition(None, inner.start)
                outer.start.add_transition(None, outer.accept)
                # Allow inner construction to loop
                inner.accept.add_transition(None, outer.accept)
            else:
                # Kinda like concatenation, but we can stop at any time
                # outer.start -> inner -> extra1 -> outer.accept
                #                      -> out.accept
                #             -> out.accept
                outer.start.add_transition(None, outer.accept)
                tail = outer.start
                for _ in range(a, b):
                    extra_nfa = thompson_construction(sub)
                    tail.add_transition(None, extra_nfa.start)
                    tail.add_transition(None, outer.accept)
                    tail = extra_nfa.accept
                tail.add_transition(None, outer.accept)
            return outer
        case _:
            raise ValueError(f"Unsupported pattern type. {type(pattern)}")


