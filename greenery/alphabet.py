from __future__ import annotations

from collections import defaultdict
from dataclasses import dataclass, field
from typing import Mapping, Union, Any, Iterator, Iterable, Sequence
from enum import Enum, auto
from functools import total_ordering


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
alpha_type = Union[str, AnythingElse]
event_type = int


@dataclass(frozen=True)
class Alphabet(Mapping[alpha_type, event_type]):
    """ Represents an Alphabet for an FSM. It maps symbols to events."""
    _symbol_to_event: dict[alpha_type, event_type]
    _event_to_symbol: dict[event_type, tuple[alpha_type]] = field(init=False, repr=False, compare=False)

    def __post_init__(self):
        event_to_symbol = defaultdict(list)
        for s, t in self._symbol_to_event.items():
            event_to_symbol[t].append(s)
        object.__setattr__(self, '_event_to_symbol', {t: tuple(s) for t, s in event_to_symbol.items()})

    def __hash__(self):
        return hash(frozenset(self._symbol_to_event.items()))

    def __getitem__(self, k: alpha_type) -> event_type:
        if k in self._symbol_to_event:
            return self._symbol_to_event[k]
        elif ANYTHING_ELSE in self._symbol_to_event:
            return self._symbol_to_event[ANYTHING_ELSE]
        else:
            raise KeyError(k)

    def __len__(self) -> int:
        return len(self._symbol_to_event)

    def __iter__(self) -> Iterator[alpha_type]:
        return iter(self._symbol_to_event)

    @property
    def events(self):
        return self._event_to_symbol

    @classmethod
    def distinct(cls, s: Iterable[alpha_type]):
        """ For convenience, the resulting instance behaves like an old-style alphabet"""
        return cls({sym: i for i, sym in enumerate(s)})

    @classmethod
    def groups(cls, *equal_groups: Iterable[alpha_type] | AnythingElse):
        """ Each parameter gets turned into its own event"""
        return cls({sym: i
                    for i, group in enumerate(equal_groups)
                    for sym in (group if group != ANYTHING_ELSE else (group,))})

    def union(*alphabets: Alphabet) -> tuple[Alphabet, tuple[dict[event_type, Union[event_type, None]], ...]]:
        """
        Creates the union Alphabet from two alphabets.
        Also returns a mapping for each input alphabet mapping new event to old event (many-to-one)
        """
        #   Terminology
        # symbol           - an individual input character or ANYTHING_ELSE
        # events (plural)  - The combination of all events from each of the independent alphabets
        # new_event        - The new event that the symbol(s) got mapped to
        # old_event        - The old event that the events belongs to

        # Deduplicate alphabets
        unique_alphabets = tuple(set(alphabets))
        if len(unique_alphabets) == 1:
            res = unique_alphabets[0]
            return res, (dict(zip(res.events, res.events)),) * len(alphabets)

        all_symbols = set().union(*(a._symbol_to_event.keys() for a in unique_alphabets))

        symbol_to_events = {symbol: tuple(a.get(symbol) for a in unique_alphabets) for symbol in all_symbols}

        events_to_symbols = defaultdict(list)
        for symbol, events in symbol_to_events.items():
            events_to_symbols[events].append(symbol)
        events_to_new_event = {k: i for i, k in enumerate(events_to_symbols)}
        result = Alphabet({symbol: events_to_new_event[events]
                           for events, symbols in events_to_symbols.items()
                           for symbol in symbols})

        new_to_old_mappings = [{} for _ in unique_alphabets]
        for events, new_event in events_to_new_event.items():
            for old_event, new_to_old in zip(events, new_to_old_mappings):
                new_to_old[new_event] = old_event
        return result, tuple(new_to_old_mappings[unique_alphabets.index(a)] for a in alphabets)
