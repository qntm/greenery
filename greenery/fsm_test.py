from __future__ import annotations

import pickle
from collections import deque
from copy import copy
from typing import FrozenSet, Iterator, Literal, Union, cast

import pytest

from .fsm import (
    ANYTHING_ELSE,
    AnythingElse,
    Fsm,
    alpha_type,
    alphabet_sort_key,
    epsilon,
    null,
    state_type,
)

FixtureA = Fsm

FixtureB = Fsm

Symbol = Union[str, FrozenSet[str]]


def strings(fsm: Fsm) -> Iterator[list[alpha_type]]:
    """
    Generates a list of all strings `fsm` accepts.
    Only intended to test if the Fsm
    For a commented version of this code see rxelems.Pattern.strings
    """
    livestates = set(state for state in fsm.states if fsm.islive(state))
    strings: deque[tuple[list[alpha_type], state_type]] = deque()

    current_state: state_type = fsm.initial
    current_string: list[alpha_type] = []
    if current_state in livestates:
        if current_state in fsm.finals:
            yield current_string
        strings.append((current_string, current_state))
    while strings:
        current_string, current_state = strings.popleft()
        if current_state in fsm.map:
            for symbol in sorted(fsm.map[current_state], key=alphabet_sort_key):
                next_state = fsm.map[current_state][symbol]
                next_string = current_string + [symbol]
                if next_state in livestates:
                    if next_state in fsm.finals:
                        yield next_string
                    strings.append((next_string, next_state))


def test_addbug() -> None:
    # Odd bug with Fsm.__add__(), exposed by "[bc]*c"
    int5A = Fsm(
        alphabet={"a", "b", "c", ANYTHING_ELSE},
        states={0, 1},
        initial=1,
        finals={1},
        map={
            0: {ANYTHING_ELSE: 0, "a": 0, "b": 0, "c": 0},
            1: {ANYTHING_ELSE: 0, "a": 0, "b": 1, "c": 1},
        },
    )
    assert int5A.accepts("")

    int5B = Fsm(
        alphabet={"a", "b", "c", ANYTHING_ELSE},
        states={0, 1, 2},
        initial=1,
        finals={0},
        map={
            0: {ANYTHING_ELSE: 2, "a": 2, "b": 2, "c": 2},
            1: {ANYTHING_ELSE: 2, "a": 2, "b": 2, "c": 0},
            2: {ANYTHING_ELSE: 2, "a": 2, "b": 2, "c": 2},
        },
    )
    assert int5B.accepts("c")

    int5C = int5A + int5B
    assert int5C.accepts("c")
    # assert int5C.initial == 0


def test_builtins() -> None:
    assert not null("a").accepts("a")
    assert epsilon("a").accepts("")
    assert not epsilon("a").accepts("a")


@pytest.fixture(params=["str", "frozenset", "mixed"])
def mode(request: pytest.FixtureRequest) -> str:
    return cast(str, request.param)


@pytest.fixture
def a_sym(mode: Literal["str", "frozenset", "mixed"]) -> Symbol:
    if mode == "str":
        return "a"
    elif mode == "frozenset":
        return frozenset({"a", "c"})
    elif mode == "mixed":
        return "a"
    else:
        raise ValueError(mode)


@pytest.fixture
def b_sym(mode: Literal["str", "frozenset", "mixed"]) -> Symbol:
    if mode == "str":
        return "b"
    elif mode == "frozenset":
        return frozenset({"b", "d", "e"})
    elif mode == "mixed":
        return frozenset({"b", "d", "e"})
    else:
        raise ValueError(mode)


@pytest.fixture
def a(a_sym: Symbol, b_sym: Symbol) -> FixtureA:
    a = Fsm(
        alphabet={a_sym, b_sym},
        states={0, 1, "ob"},
        initial=0,
        finals={1},
        map={
            0: {a_sym: 1, b_sym: "ob"},
            1: {a_sym: "ob", b_sym: "ob"},
            "ob": {a_sym: "ob", b_sym: "ob"},
        },
    )
    return a


def test_a(a: FixtureA) -> None:
    assert not a.accepts("")
    assert a.accepts("a")
    assert not a.accepts("b")


@pytest.fixture
def b(a_sym: Symbol, b_sym: Symbol) -> FixtureB:
    b = Fsm(
        alphabet={a_sym, b_sym},
        states={0, 1, "ob"},
        initial=0,
        finals={1},
        map={
            0: {a_sym: "ob", b_sym: 1},
            1: {a_sym: "ob", b_sym: "ob"},
            "ob": {a_sym: "ob", b_sym: "ob"},
        },
    )
    return b


def test_b(b: FixtureB) -> None:
    assert not b.accepts("")
    assert not b.accepts("a")
    assert b.accepts("b")


def test_concatenation_aa(a: FixtureA) -> None:
    concAA = a + a
    assert not concAA.accepts("")
    assert not concAA.accepts("a")
    assert concAA.accepts("aa")
    assert not concAA.accepts("aaa")

    concAA = epsilon(a.alphabet) + a + a
    assert not concAA.accepts("")
    assert not concAA.accepts("a")
    assert concAA.accepts("aa")
    assert not concAA.accepts("aaa")


def test_concatenation_ab(a: FixtureA, b: FixtureB) -> None:
    concAB = a + b
    assert not concAB.accepts("")
    assert not concAB.accepts("a")
    assert not concAB.accepts("b")
    assert not concAB.accepts("aa")
    assert concAB.accepts("ab")
    assert not concAB.accepts("ba")
    assert not concAB.accepts("bb")


def test_alternation_a(a: FixtureA) -> None:
    altA = a | null(a.alphabet)
    assert not altA.accepts("")
    assert altA.accepts("a")


def test_alternation_ab(a: FixtureA, b: FixtureB) -> None:
    altAB = a | b
    assert not altAB.accepts("")
    assert altAB.accepts("a")
    assert altAB.accepts("b")
    assert not altAB.accepts("aa")
    assert not altAB.accepts("ab")
    assert not altAB.accepts("ba")
    assert not altAB.accepts("bb")


def test_star(a: FixtureA) -> None:
    starA = a.star()
    assert starA.accepts("")
    assert starA.accepts("a")
    assert not starA.accepts("b")
    assert starA.accepts("aaaaaaaaa")


def test_multiply_0(a: FixtureA) -> None:
    zeroA = a * 0
    assert zeroA.accepts("")
    assert not zeroA.accepts("a")


def test_multiply_1(a: FixtureA) -> None:
    oneA = a * 1
    assert not oneA.accepts("")
    assert oneA.accepts("a")
    assert not oneA.accepts("aa")


def test_multiply_2(a: FixtureA) -> None:
    twoA = a * 2
    assert not twoA.accepts("")
    assert not twoA.accepts("a")
    assert twoA.accepts("aa")
    assert not twoA.accepts("aaa")


def test_multiply_7(a: FixtureA) -> None:
    sevenA = a * 7
    assert not sevenA.accepts("aaaaaa")
    assert sevenA.accepts("aaaaaaa")
    assert not sevenA.accepts("aaaaaaaa")


def test_optional_mul(a: FixtureA, b: FixtureB) -> None:
    unit = a + b
    # accepts "ab"

    optional = epsilon(a.alphabet) | unit
    # accepts "(ab)?
    assert optional.accepts([])
    assert not optional.accepts(["a"])
    assert not optional.accepts(["b"])
    assert optional.accepts(["a", "b"])
    assert not optional.accepts(["a", "a"])

    optional = optional * 2
    # accepts "(ab)?(ab)?"
    assert optional.accepts([])
    assert not optional.accepts(["a"])
    assert not optional.accepts(["b"])
    assert not optional.accepts(["a", "a"])
    assert optional.accepts(["a", "b"])
    assert not optional.accepts(["b", "a"])
    assert not optional.accepts(["b", "b"])
    assert not optional.accepts(["a", "a", "a"])
    assert optional.accepts(["a", "b", "a", "b"])


def test_intersection_ab(a: FixtureA, b: FixtureB) -> None:
    intAB = a & b
    assert not intAB.accepts("")
    assert not intAB.accepts("a")
    assert not intAB.accepts("b")


def test_negation(a: FixtureA) -> None:
    everythingbutA = a.everythingbut()
    assert everythingbutA.accepts("")
    assert not everythingbutA.accepts("a")
    assert everythingbutA.accepts("b")
    assert everythingbutA.accepts("aa")
    assert everythingbutA.accepts("ab")


def test_crawl_reduction() -> None:
    # this is "0*1" in heavy disguise. crawl should resolve this duplication
    # Notice how states 2 and 3 behave identically. When resolved together,
    # states 1 and 2&3 also behave identically, so they, too should be resolved
    # (this is impossible to spot before 2 and 3 have been combined).
    # Finally, the oblivion state should be omitted.
    merged = Fsm(
        alphabet={"0", "1"},
        states={1, 2, 3, 4, "oblivion"},
        initial=1,
        finals={4},
        map={
            1: {"0": 2, "1": 4},
            2: {"0": 3, "1": 4},
            3: {"0": 3, "1": 4},
            4: {"0": "oblivion", "1": "oblivion"},
            "oblivion": {"0": "oblivion", "1": "oblivion"},
        },
    ).reduce()
    assert len(merged.states) == 2


def test_bug_28() -> None:
    # This is (ab*)* and it caused some defects.
    abstar = Fsm(
        alphabet={"a", "b"},
        states={0, 1},
        initial=0,
        finals={1},
        map={0: {"a": 1}, 1: {"b": 1}},
    )
    assert abstar.accepts("a")
    assert not abstar.accepts("b")
    assert abstar.accepts("ab")
    assert abstar.accepts("abb")
    abstarstar = abstar.star()
    assert abstarstar.accepts("a")
    assert not abstarstar.accepts("b")
    assert abstarstar.accepts("ab")
    assert not abstar.star().accepts("bb")


def test_star_advanced() -> None:
    # This is (a*ba)*. Naively connecting the final states to the initial state
    # gives the incorrect result here.
    starred = Fsm(
        alphabet={"a", "b"},
        states={0, 1, 2, "oblivion"},
        initial=0,
        finals={2},
        map={
            0: {"a": 0, "b": 1},
            1: {"a": 2, "b": "oblivion"},
            2: {"a": "oblivion", "b": "oblivion"},
            "oblivion": {"a": "oblivion", "b": "oblivion"},
        },
    ).star()
    assert starred.alphabet == frozenset(["a", "b"])
    assert starred.accepts("")
    assert not starred.accepts("a")
    assert not starred.accepts("b")
    assert not starred.accepts("aa")
    assert starred.accepts("ba")
    assert starred.accepts("aba")
    assert starred.accepts("aaba")
    assert not starred.accepts("aabb")
    assert starred.accepts("abababa")


def test_reduce() -> None:
    # FSM accepts no strings but has 3 states, needs only 1
    symbol = "x"
    asdf = Fsm(
        alphabet={symbol},
        states={0, 1, 2},
        initial=0,
        finals={1},
        map={
            0: {symbol: 2},
            1: {symbol: 2},
            2: {symbol: 2},
        },
    )
    asdf = asdf.reduce()
    assert len(asdf.states) == 1


def test_reverse_abc() -> None:
    abc = Fsm(
        alphabet={"a", "b", "c"},
        states={0, 1, 2, 3, None},
        initial=0,
        finals={3},
        map={
            0: {"a": 1, "b": None, "c": None},
            1: {"a": None, "b": 2, "c": None},
            2: {"a": None, "b": None, "c": 3},
            3: {"a": None, "b": None, "c": None},
            None: {"a": None, "b": None, "c": None},
        },
    )
    cba = abc.reversed()
    assert cba.accepts("cba")


def test_reverse_brzozowski() -> None:
    # This is (a|b)*a(a|b)
    brzozowski = Fsm(
        alphabet={"a", "b"},
        states={"A", "B", "C", "D", "E"},
        initial="A",
        finals={"C", "E"},
        map={
            "A": {"a": "B", "b": "D"},
            "B": {"a": "C", "b": "E"},
            "C": {"a": "C", "b": "E"},
            "D": {"a": "B", "b": "D"},
            "E": {"a": "B", "b": "D"},
        },
    )
    assert brzozowski.accepts("aa")
    assert brzozowski.accepts("ab")
    assert brzozowski.accepts("aab")
    assert brzozowski.accepts("bab")
    assert brzozowski.accepts("abbbbbbbab")
    assert not brzozowski.accepts("")
    assert not brzozowski.accepts("a")
    assert not brzozowski.accepts("b")
    assert not brzozowski.accepts("ba")
    assert not brzozowski.accepts("bb")
    assert not brzozowski.accepts("bbbbbbbbbbbb")

    # So this is (a|b)a(a|b)*
    b2 = brzozowski.reversed()
    assert b2.accepts("aa")
    assert b2.accepts("ba")
    assert b2.accepts("baa")
    assert b2.accepts("bab")
    assert b2.accepts("babbbbbbba")
    assert not b2.accepts("")
    assert not b2.accepts("a")
    assert not b2.accepts("b")
    assert not b2.accepts("ab")
    assert not b2.accepts("bb")
    assert not b2.accepts("bbbbbbbbbbbb")

    # Test string generator functionality.
    # Other tests require this helper function to work correctly, so we test
    # it here
    gen = strings(b2)
    assert next(gen) == ["a", "a"]
    assert next(gen) == ["b", "a"]
    assert next(gen) == ["a", "a", "a"]
    assert next(gen) == ["a", "a", "b"]
    assert next(gen) == ["b", "a", "a"]
    assert next(gen) == ["b", "a", "b"]
    assert next(gen) == ["a", "a", "a", "a"]


def test_reverse_epsilon() -> None:
    # epsilon reversed is epsilon
    assert epsilon("a").reversed().accepts("")


def test_binary_3() -> None:
    # Binary numbers divisible by 3.
    # Disallows the empty string
    # Allows "0" on its own, but not leading zeroes.
    div3 = Fsm(
        alphabet={"0", "1"},
        states={"initial", "zero", 0, 1, 2, None},
        initial="initial",
        finals={"zero", 0},
        map={
            "initial": {"0": "zero", "1": 1},
            "zero": {"0": None, "1": None},
            0: {"0": 0, "1": 1},
            1: {"0": 2, "1": 0},
            2: {"0": 1, "1": 2},
            None: {"0": None, "1": None},
        },
    )
    assert not div3.accepts("")
    assert div3.accepts("0")
    assert not div3.accepts("1")
    assert not div3.accepts("00")
    assert not div3.accepts("01")
    assert not div3.accepts("10")
    assert div3.accepts("11")
    assert not div3.accepts("000")
    assert not div3.accepts("001")
    assert not div3.accepts("010")
    assert not div3.accepts("011")
    assert not div3.accepts("100")
    assert not div3.accepts("101")
    assert div3.accepts("110")
    assert not div3.accepts("111")
    assert not div3.accepts("0000")
    assert not div3.accepts("0001")
    assert not div3.accepts("0010")
    assert not div3.accepts("0011")
    assert not div3.accepts("0100")
    assert not div3.accepts("0101")
    assert not div3.accepts("0110")
    assert not div3.accepts("0111")
    assert not div3.accepts("1000")
    assert div3.accepts("1001")


def test_invalid_fsms() -> None:
    # initial state 1 is not a state
    with pytest.raises(Exception, match="Initial state"):
        Fsm(alphabet={}, states={}, initial=1, finals=(), map={})

    # final state 2 not a state
    with pytest.raises(Exception, match="Final states"):
        Fsm(alphabet={}, states={1}, initial=1, finals={2}, map={})

    # invalid transition for state 1, symbol "a"
    with pytest.raises(Exception, match="Transition.+leads to.+not a state"):
        Fsm(alphabet={"a"}, states={1}, initial=1, finals=(), map={1: {"a": 2}})

    # invalid transition from unknown state
    with pytest.raises(Exception, match="Transition.+unknown state"):
        Fsm(alphabet={"a"}, states={1, 2}, initial=1, finals=(), map={3: {"a": 2}})

    # invalid transition table includes symbol outside of alphabet
    with pytest.raises(Exception, match="Invalid symbol"):
        Fsm(
            alphabet={"a"},
            states={1, 2},
            initial=1,
            finals=(),
            map={1: {"a": 2, "b": 2}},
        )


def test_bad_multiplier(a: FixtureA) -> None:
    with pytest.raises(Exception, match="Can't multiply"):
        a * -1


def test_anything_else_acceptance() -> None:
    a = Fsm(
        alphabet={"a", "b", "c", ANYTHING_ELSE},
        states={1},
        initial=1,
        finals={1},
        map={1: {"a": 1, "b": 1, "c": 1, ANYTHING_ELSE: 1}},
    )
    assert a.accepts("d")


def test_difference(a: FixtureA, b: FixtureB, a_sym: Symbol, b_sym: Symbol) -> None:
    aorb = Fsm(
        alphabet=a.alphabet,
        states={0, 1, None},
        initial=0,
        finals={1},
        map={
            0: {a_sym: 1, b_sym: 1},
            1: {a_sym: None, b_sym: None},
            None: {a_sym: None, b_sym: None},
        },
    )

    assert (a ^ a).empty()
    assert (b ^ b).empty()
    assert list(strings(a ^ b)) == [[a_sym], [b_sym]]
    assert list(strings(aorb ^ a)) == [[b_sym]]


def test_empty(a: FixtureA, b: FixtureB) -> None:
    assert not a.empty()
    assert not b.empty()

    assert Fsm(
        alphabet={},
        states={0, 1},
        initial=0,
        finals={1},
        map={0: {}, 1: {}},
    ).empty()

    assert not Fsm(
        alphabet={},
        states={0},
        initial=0,
        finals={0},
        map={0: {}},
    ).empty()

    assert Fsm(
        alphabet={"a", "b"},
        states={0, 1, None, 2},
        initial=0,
        finals={2},
        map={
            0: {"a": 1, "b": 1},
            1: {"a": None, "b": None},
            None: {"a": None, "b": None},
            2: {"a": None, "b": None},
        },
    ).empty()


def test_equivalent(a: FixtureA, b: FixtureB) -> None:
    assert (a | b).equivalent(b | a)


def test_eq_ne(a: FixtureA, b: FixtureB) -> None:
    # pylint: disable=comparison-with-itself

    assert a == a
    assert b == b
    assert a != b
    assert b != a
    assert (a | b) == (b | a)


@pytest.mark.parametrize(
    argnames="other",
    argvalues=(
        17,
        (14,),
        "blenny",
        "a",
        ("a",),
    ),
)
def test_eq_ne_het(a: FixtureA, other: object) -> None:
    # pylint: disable=comparison-with-itself

    # eq
    assert not a == other
    # eq, symmetric
    assert not other == a

    # neq
    assert a != other
    # neq, symmetric
    assert other != a


def test_dead_default() -> None:
    """
    You may now omit a transition, or even an entire state, from the map.
    This affects every usage of `Fsm.map`.
    """
    blockquote = Fsm(
        alphabet={"/", "*", ANYTHING_ELSE},
        states={0, 1, 2, 3, 4, 5},
        initial=0,
        finals={4},
        map={
            0: {"/": 1},
            1: {"*": 2},
            2: {"/": 2, ANYTHING_ELSE: 2, "*": 3},
            3: {"/": 4, ANYTHING_ELSE: 2, "*": 3},
        },
    )
    assert blockquote.accepts(["/", "*", "whatever", "*", "/"])
    assert not blockquote.accepts(["*", "*", "whatever", "*", "/"])
    assert (
        str(blockquote)
        == "  name final? * / ANYTHING_ELSE \n"
        + "--------------------------------\n"
        + "* 0    False    1               \n"
        + "  1    False  2                 \n"
        + "  2    False  3 2 2             \n"
        + "  3    False  3 4 2             \n"
        + "  4    True                     \n"
        + "  5    False                    \n"
    )
    blockquote | blockquote
    blockquote & blockquote
    blockquote ^ blockquote
    # Fsm does not support the `Reversible` protocol, because its
    # `__reversed__` implementation does not return an iterator.
    # Even if it did, it would not conform semantically because it returns an
    # iterable of the reversed strings, not a reversed iteration of those
    # strings.
    # reversed(blockquote)
    blockquote.reversed()
    assert not blockquote.everythingbut().accepts(["/", "*", "whatever", "*", "/"])

    # deliberately seek oblivion
    assert blockquote.everythingbut().accepts(["*"])

    assert blockquote.islive(3)
    assert blockquote.islive(4)
    assert not blockquote.islive(5)
    assert next(strings(blockquote)) == ["/", "*", "*", "/"]


def test_new_set_methods(
    a: FixtureA, b: FixtureB, a_sym: Symbol, b_sym: Symbol
) -> None:
    # A whole bunch of new methods were added to the FSM module to enable FSMs
    # to function exactly as if they were sets of strings (symbol lists), see:
    # https://docs.python.org/3/library/stdtypes.html#set-types-set-frozenset
    # But do they work?

    # "in"
    assert [a_sym] in a
    assert [a_sym] not in b

    # set.union() imitation
    assert Fsm.union(a, b) == a.union(b)
    assert Fsm.union().empty()
    assert Fsm.intersection(a, b) == a.intersection(b)

    # This takes a little explaining. In general, `a & b & c` is equivalent to
    # `EVERYTHING & a & b & c` where `EVERYTHING` is an FSM accepting every
    # possible string. Similarly `a` is equivalent to `EVERYTHING & a`, and the
    # intersection of no sets at all is... `EVERYTHING`. However, since we
    # compute the union of alphabets, and there are no alphabets, the union is
    # the empty set. So the only string which `EVERYTHING` actually recognises
    # is the empty string, [] (or "" if you prefer).
    int_none = Fsm.intersection()
    assert len(list(strings(int_none))) == 1
    assert [] in int_none

    assert (a | b).difference(a) == Fsm.difference((a | b), a) == (a | b) - a == b
    assert (
        (a | b).difference(a, b)
        == Fsm.difference((a | b), a, b)
        == (a | b) - a - b
        == null(a.alphabet)
    )
    assert a.symmetric_difference(b) == Fsm.symmetric_difference(a, b) == a ^ b
    assert a.isdisjoint(b)
    assert a <= (a | b)
    assert a < (a | b)
    assert a != (a | b)
    assert (a | b) > a
    assert (a | b) >= a

    assert list(strings(a.concatenate(a, a))) == [[a_sym, a_sym, a_sym]]
    assert list(strings(a.concatenate())) == [[a_sym]]
    assert list(strings(Fsm.concatenate(b, a, b))) == [[b_sym, a_sym, b_sym]]
    assert Fsm.concatenate().empty()


def test_copy(a: FixtureA) -> None:
    # fsm.copy() and frozenset().copy() both preserve identity, because they
    # are immutable. This is just showing that we give the same behaviour.
    copyables: tuple[Fsm | frozenset[str], ...] = (a, frozenset("abc"))
    for x in copyables:
        assert x.copy() is x

    # Same, via the `__copy__` method.
    for x in copyables:
        assert copy(x) is x


def test_oblivion_crawl(a: FixtureA) -> None:
    # When crawling a new FSM, we should avoid generating an oblivion state.
    # `abc` has no oblivion state... all the results should not as well!
    abc = Fsm(
        alphabet={"a", "b", "c"},
        states={0, 1, 2, 3},
        initial=0,
        finals={3},
        map={
            0: {"a": 1},
            1: {"b": 2},
            2: {"c": 3},
        },
    )
    assert len((abc + abc).states) == 7
    assert len(abc.star().states) == 3
    assert len((abc * 3).states) == 10
    assert len(abc.reversed().states) == 4
    assert len((abc | abc).states) == 4
    assert len((abc & abc).states) == 4
    assert len((abc ^ abc).states) == 1
    assert len((abc - abc).states) == 1


def test_concatenate_bug(a: FixtureA, a_sym: Symbol) -> None:
    # This exposes a defect in Fsm.concatenate.
    assert Fsm.concatenate(a, epsilon(a.alphabet), a).accepts("aa")
    assert Fsm.concatenate(a, epsilon(a.alphabet), epsilon(a.alphabet), a).accepts("aa")


def test_derive(a: FixtureA, b: FixtureB, a_sym: Symbol, b_sym: Symbol) -> None:
    # Just some basic tests because this is mainly a regex thing.
    assert a.derive([a_sym]) == epsilon(a.alphabet)
    assert a.derive([b_sym]) == null(a.alphabet)

    with pytest.raises(KeyError):
        a.derive("c")

    assert (a * 3).derive([a_sym]) == a * 2
    assert (a.star() - epsilon(a.alphabet)).derive([a_sym]) == a.star()


def test_anything_else_singleton() -> None:
    assert AnythingElse.TOKEN is ANYTHING_ELSE


def test_anything_else_self() -> None:
    """ANYTHING_ELSE is consistently equal to itself."""

    # pylint: disable=comparison-with-itself
    # pylint: disable=unneeded-not
    assert not ANYTHING_ELSE < ANYTHING_ELSE
    assert ANYTHING_ELSE <= ANYTHING_ELSE
    assert not ANYTHING_ELSE != ANYTHING_ELSE
    assert ANYTHING_ELSE == ANYTHING_ELSE
    assert ANYTHING_ELSE >= ANYTHING_ELSE
    assert not ANYTHING_ELSE > ANYTHING_ELSE


@pytest.mark.parametrize(
    argnames="val",
    argvalues=(
        float("-inf"),
        float("nan"),
        float("inf"),
        0,
        "abc",
        object(),
        str(ANYTHING_ELSE),
    ),
)
def test_anything_else_sorts_after(val: object) -> None:
    """ANYTHING_ELSE sorts strictly after anything."""

    # pylint: disable=unneeded-not
    assert not ANYTHING_ELSE < val
    assert not ANYTHING_ELSE <= val
    assert not ANYTHING_ELSE == val
    assert ANYTHING_ELSE != val
    assert ANYTHING_ELSE > val
    assert ANYTHING_ELSE >= val

    assert val < ANYTHING_ELSE
    assert val <= ANYTHING_ELSE
    assert val != ANYTHING_ELSE
    assert not val == ANYTHING_ELSE
    assert not val > ANYTHING_ELSE
    assert not val >= ANYTHING_ELSE


def test_anything_else_pickle() -> None:
    # [^z]
    fsm1 = Fsm(
        alphabet={"z", ANYTHING_ELSE},
        states={0, 1},
        initial=0,
        finals={1},
        map={0: {ANYTHING_ELSE: 1}},
    )

    fsm1_unpickled = pickle.loads(pickle.dumps(fsm1))

    # Newly-created instance.
    assert fsm1_unpickled is not fsm1

    # but equivalent.
    assert fsm1 == fsm1_unpickled

    # The first letter is "z" (since "anything else" always sorts last).
    letter_z, anything_else = sorted(fsm1_unpickled.alphabet)
    assert letter_z == "z"

    # Stronger singleton assertion:
    assert anything_else is ANYTHING_ELSE
