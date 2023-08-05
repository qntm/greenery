from __future__ import annotations

import pickle
from copy import copy

import pytest

from .fsm import Fsm, unify_alphabets, epsilon, from_charclass, null
from .charclass import Charclass

# pylint: disable=invalid-name

FixtureA = Fsm

FixtureB = Fsm


def test_addbug() -> None:
    # Odd bug with Fsm.__add__(), exposed by "[bc]*c"
    int5A = Fsm(
        alphabet={Charclass("a"), Charclass("b"), Charclass("c"), ~Charclass("abc")},
        states={0, 1},
        initial=1,
        finals={1},
        map={
            0: {~Charclass("abc"): 0, Charclass("a"): 0, Charclass("b"): 0, Charclass("c"): 0},
            1: {~Charclass("abc"): 0, Charclass("a"): 0, Charclass("b"): 1, Charclass("c"): 1},
        },
    )
    assert int5A.accepts("")

    int5B = Fsm(
        alphabet={Charclass("a"), Charclass("b"), Charclass("c"), ~Charclass("abc")},
        states={0, 1, 2},
        initial=1,
        finals={0},
        map={
            0: {~Charclass("abc"): 2, Charclass("a"): 2, Charclass("b"): 2, Charclass("c"): 2},
            1: {~Charclass("abc"): 2, Charclass("a"): 2, Charclass("b"): 2, Charclass("c"): 0},
            2: {~Charclass("abc"): 2, Charclass("a"): 2, Charclass("b"): 2, Charclass("c"): 2},
        },
    )
    assert int5B.accepts("c")

    int5C = int5A + int5B
    assert int5C.accepts("c")
    # assert int5C.initial == 0


def test_builtins() -> None:
    assert not null({Charclass("a"), ~Charclass("a")}).accepts("a")
    assert epsilon({Charclass("a"), ~Charclass("a")}).accepts("")
    assert not epsilon({Charclass("a"), ~Charclass("a")}).accepts("a")


@pytest.fixture(name="a")
def fixture_a() -> FixtureA:
    return Fsm(
        alphabet={Charclass("a"), Charclass("b"), ~Charclass("ab")},
        states={0, 1, "ob"},
        initial=0,
        finals={1},
        map={
            0: {Charclass("a"): 1, Charclass("b"): "ob", ~Charclass("ab"): "ob"},
            1: {Charclass("a"): "ob", Charclass("b"): "ob", ~Charclass("ab"): "ob"},
            "ob": {Charclass("a"): "ob", Charclass("b"): "ob", ~Charclass("ab"): "ob"},
        },
    )


def test_a(a: FixtureA) -> None:
    assert not a.accepts("")
    assert a.accepts("a")
    assert not a.accepts("b")


@pytest.fixture(name="b")
def fixture_b() -> FixtureB:
    return Fsm(
        alphabet={Charclass("a"), Charclass("b"), ~Charclass("ab")},
        states={0, 1, "ob"},
        initial=0,
        finals={1},
        map={
            0: {Charclass("a"): "ob", Charclass("b"): 1, ~Charclass("ab"): "ob"},
            1: {Charclass("a"): "ob", Charclass("b"): "ob", ~Charclass("ab"): "ob"},
            "ob": {Charclass("a"): "ob", Charclass("b"): "ob", ~Charclass("ab"): "ob"},
        },
    )


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

    concAA = epsilon({Charclass("a"), Charclass("b"), ~Charclass("ab")}) + a + a
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
    altA = a | null({Charclass("a"), Charclass("b"), ~Charclass("ab")})
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
    merged = Fsm(
        alphabet={Charclass("0"), Charclass("1"), ~Charclass("01")},
        states={1, 2, 3, 4, "oblivion"},
        initial=1,
        finals={4},
        map={
            1: {Charclass("0"): 2, Charclass("1"): 4, ~Charclass("01"): "oblivion"},
            2: {Charclass("0"): 3, Charclass("1"): 4, ~Charclass("01"): "oblivion"},
            3: {Charclass("0"): 3, Charclass("1"): 4, ~Charclass("01"): "oblivion"},
            4: {Charclass("0"): "oblivion", Charclass("1"): "oblivion", ~Charclass("01"): "oblivion"},
            "oblivion": {
                Charclass("0"): "oblivion",
                Charclass("1"): "oblivion",
                ~Charclass("01"): "oblivion",
            },
        },
    ).reduce()
    assert len(merged.states) == 3


def test_bug_28() -> None:
    # This is (ab*)* and it caused some defects.
    abstar = Fsm(
        alphabet={Charclass("a"), Charclass("b"), ~Charclass("ab")},
        states={0, 1, 2},
        initial=0,
        finals={1},
        map={
            0: {Charclass("a"): 1, Charclass("b"): 2, ~Charclass("ab"): 2},
            1: {Charclass("a"): 2, Charclass("b"): 1, ~Charclass("ab"): 2},
            2: {Charclass("a"): 2, Charclass("b"): 2, ~Charclass("ab"): 2},
        },
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
        alphabet={Charclass("a"), Charclass("b"), ~Charclass("ab")},
        states={0, 1, 2, "oblivion"},
        initial=0,
        finals={2},
        map={
            0: {Charclass("a"): 0, Charclass("b"): 1, ~Charclass("ab"): "oblivion"},
            1: {Charclass("a"): 2, Charclass("b"): "oblivion", ~Charclass("ab"): "oblivion"},
            2: {Charclass("a"): "oblivion", Charclass("b"): "oblivion", ~Charclass("ab"): "oblivion"},
            "oblivion": {
                Charclass("a"): "oblivion",
                Charclass("b"): "oblivion",
                ~Charclass("ab"): "oblivion",
            },
        },
    ).star()
    assert starred.alphabet == frozenset([
        Charclass("a"),
        Charclass("b"),
        ~Charclass("ab"),
    ])
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
    asdf = Fsm(
        alphabet={Charclass("x"), ~Charclass("x")},
        states={0, 1, 2},
        initial=0,
        finals={1},
        map={
            0: {Charclass("x"): 2, ~Charclass("x"): 2},
            1: {Charclass("x"): 2, ~Charclass("x"): 2},
            2: {Charclass("x"): 2, ~Charclass("x"): 2},
        },
    )
    asdf = asdf.reduce()
    assert len(asdf.states) == 1


def test_reverse_abc() -> None:
    abc = Fsm(
        alphabet={Charclass("a"), Charclass("b"), Charclass("c"), ~Charclass("abc")},
        states={0, 1, 2, 3, 4},
        initial=0,
        finals={3},
        map={
            0: {Charclass("a"): 1, Charclass("b"): 4, Charclass("c"): 4, ~Charclass("abc"): 4},
            1: {Charclass("a"): 4, Charclass("b"): 2, Charclass("c"): 4, ~Charclass("abc"): 4},
            2: {Charclass("a"): 4, Charclass("b"): 4, Charclass("c"): 3, ~Charclass("abc"): 4},
            3: {Charclass("a"): 4, Charclass("b"): 4, Charclass("c"): 4, ~Charclass("abc"): 4},
            4: {Charclass("a"): 4, Charclass("b"): 4, Charclass("c"): 4, ~Charclass("abc"): 4},
        },
    )
    cba = abc.reversed()
    assert cba.accepts("cba")


def test_reverse_brzozowski() -> None:
    # This is (a|b)*a(a|b)
    brzozowski = Fsm(
        alphabet={Charclass("a"), Charclass("b"), ~Charclass("ab")},
        states={"A", "B", "C", "D", "E", "F"},
        initial="A",
        finals={"C", "E"},
        map={
            "A": {Charclass("a"): "B", Charclass("b"): "D", ~Charclass("ab"): "F"},
            "B": {Charclass("a"): "C", Charclass("b"): "E", ~Charclass("ab"): "F"},
            "C": {Charclass("a"): "C", Charclass("b"): "E", ~Charclass("ab"): "F"},
            "D": {Charclass("a"): "B", Charclass("b"): "D", ~Charclass("ab"): "F"},
            "E": {Charclass("a"): "B", Charclass("b"): "D", ~Charclass("ab"): "F"},
            "F": {Charclass("a"): "F", Charclass("b"): "F", ~Charclass("ab"): "F"},
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
    gen = b2.strings()
    assert next(gen) == [Charclass("a"), Charclass("a")]
    assert next(gen) == [Charclass("b"), Charclass("a")]
    assert next(gen) == [Charclass("a"), Charclass("a"), Charclass("a")]
    assert next(gen) == [Charclass("a"), Charclass("a"), Charclass("b")]
    assert next(gen) == [Charclass("b"), Charclass("a"), Charclass("a")]
    assert next(gen) == [Charclass("b"), Charclass("a"), Charclass("b")]
    assert next(gen) == [Charclass("a"), Charclass("a"), Charclass("a"), Charclass("a")]


def test_reverse_epsilon() -> None:
    # epsilon reversed is epsilon
    assert epsilon({Charclass("a"), ~Charclass("a")}).reversed().accepts("")


def test_binary_3() -> None:
    # Binary numbers divisible by 3.
    # Disallows the empty string
    # Allows "0" on its own, but not leading zeroes.
    div3 = Fsm(
        alphabet={Charclass("0"), Charclass("1"), ~Charclass("01")},
        states={"initial", "zero", 0, 1, 2, 3},
        initial="initial",
        finals={"zero", 0},
        map={
            "initial": {Charclass("0"): "zero", Charclass("1"): 1, ~Charclass("01"): 3},
            "zero": {Charclass("0"): 3, Charclass("1"): 3, ~Charclass("01"): 3},
            0: {Charclass("0"): 0, Charclass("1"): 1, ~Charclass("01"): 3},
            1: {Charclass("0"): 2, Charclass("1"): 0, ~Charclass("01"): 3},
            2: {Charclass("0"): 1, Charclass("1"): 2, ~Charclass("01"): 3},
            3: {Charclass("0"): 3, Charclass("1"): 3, ~Charclass("01"): 3},
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
    with pytest.raises(ValueError, match="Initial state"):
        Fsm(alphabet={}, states={}, initial=1, finals=(), map={})

    # final state 2 not a state
    with pytest.raises(ValueError, match="Final states"):
        Fsm(alphabet={}, states={1}, initial=1, finals={2}, map={})

    # invalid transition for state 1, symbol "a"
    with pytest.raises(ValueError, match="Transition.+leads to.+not a state"):
        Fsm(alphabet={Charclass("a")}, states={1}, initial=1, finals=(), map={1: {Charclass("a"): 2}})

    # invalid transition from unknown state
    with pytest.raises(ValueError, match="Transition.+unknown state"):
        Fsm(alphabet={Charclass("a")}, states={1, 2}, initial=1, finals=(), map={3: {Charclass("a"): 2}})

    # invalid transition table includes symbol outside of alphabet
    with pytest.raises(ValueError, match="Invalid symbol"):
        Fsm(
            alphabet={Charclass("a")},
            states={1, 2},
            initial=1,
            finals=(),
            map={1: {Charclass("a"): 2, Charclass("b"): 2}},
        )


def test_bad_multiplier(a: FixtureA) -> None:
    with pytest.raises(ArithmeticError, match="Can't multiply"):
        _ = a * -1


def test_anything_else_acceptance() -> None:
    a = Fsm(
        alphabet={Charclass("a"), Charclass("b"), Charclass("c"), ~Charclass("abc")},
        states={1},
        initial=1,
        finals={1},
        map={1: {Charclass("a"): 1, Charclass("b"): 1, Charclass("c"): 1, ~Charclass("abc"): 1}},
    )
    assert a.accepts("d")


def test_difference(a: FixtureA, b: FixtureB) -> None:
    aorb = Fsm(
        alphabet={Charclass("a"), Charclass("b"), ~Charclass("ab")},
        states={0, 1, 2},
        initial=0,
        finals={1},
        map={
            0: {Charclass("a"): 1, Charclass("b"): 1, ~Charclass("ab"): 2},
            1: {Charclass("a"): 2, Charclass("b"): 2, ~Charclass("ab"): 2},
            2: {Charclass("a"): 2, Charclass("b"): 2, ~Charclass("ab"): 2},
        },
    )

    assert not list((a ^ a).strings())
    assert not list((b ^ b).strings())
    assert list((a ^ b).strings()) == [[Charclass("a")], [Charclass("b")]]
    assert list((aorb ^ a).strings()) == [[Charclass("b")]]


def test_empty(a: FixtureA, b: FixtureB) -> None:
    assert not a.empty()
    assert not b.empty()

    assert Fsm(
        alphabet={~Charclass()},
        states={0, 1},
        initial=0,
        finals={1},
        map={0: {~Charclass(): 0}, 1: {~Charclass(): 0}},
    ).empty()

    assert not Fsm(
        alphabet={~Charclass()},
        states={0},
        initial=0,
        finals={0},
        map={0: {~Charclass(): 0}},
    ).empty()

    assert Fsm(
        alphabet={Charclass("a"), Charclass("b"), ~Charclass("ab")},
        states={0, 1, 2, 3},
        initial=0,
        finals={3},
        map={
            0: {Charclass("a"): 1, Charclass("b"): 1, ~Charclass("ab"): 2},
            1: {Charclass("a"): 2, Charclass("b"): 2, ~Charclass("ab"): 2},
            2: {Charclass("a"): 2, Charclass("b"): 2, ~Charclass("ab"): 2},
            3: {Charclass("a"): 2, Charclass("b"): 2, ~Charclass("ab"): 2},
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
    Old test from when you used to be able to have sparse maps
    """
    blockquote = Fsm(
        alphabet={Charclass("/"), Charclass("*"), ~Charclass("/*")},
        states={0, 1, 2, 3, 4, 5},
        initial=0,
        finals={4},
        map={
            0: {Charclass("/"): 1, ~Charclass("/*"): 5, Charclass("*"): 5},
            1: {Charclass("/"): 5, ~Charclass("/*"): 5, Charclass("*"): 2},
            2: {Charclass("/"): 2, ~Charclass("/*"): 2, Charclass("*"): 3},
            3: {Charclass("/"): 4, ~Charclass("/*"): 2, Charclass("*"): 3},
            4: {Charclass("/"): 5, ~Charclass("/*"): 5, Charclass("*"): 5},
            5: {Charclass("/"): 5, ~Charclass("/*"): 5, Charclass("*"): 5},
        },
    )
    assert blockquote.accepts(["/", "*", "whatever", "*", "/"])
    assert not blockquote.accepts(["*", "*", "whatever", "*", "/"])
    print(str(blockquote))
    assert (
        str(blockquote)
        == "  name final? \\* / [^*/] \n"
        + "-------------------------\n"
        + "* 0    False  5  1 5     \n"
        + "  1    False  2  5 5     \n"
        + "  2    False  3  2 2     \n"
        + "  3    False  3  4 2     \n"
        + "  4    True   5  5 5     \n"
        + "  5    False  5  5 5     \n"
    )
    _ = blockquote | blockquote
    _ = blockquote & blockquote
    _ = blockquote ^ blockquote
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
    gen = blockquote.strings()
    assert next(gen) == [Charclass("/"), Charclass("*"), Charclass("*"), Charclass("/")]


def test_alphabet_unions() -> None:
    # It should now be possible to compute the union of
    # FSMs with disagreeing alphabets!
    a = Fsm(
        alphabet={Charclass("a"), ~Charclass("a")},
        states={0, 1, 2},
        initial=0,
        finals={1},
        map={
            0: {Charclass("a"): 1, ~Charclass("a"): 2},
            1: {Charclass("a"): 1, ~Charclass("a"): 2},
            2: {Charclass("a"): 2, ~Charclass("a"): 2},
        },
    )

    b = Fsm(
        alphabet={Charclass("b"), ~Charclass("b")},
        states={0, 1, 2},
        initial=0,
        finals={1},
        map={
            0: {Charclass("b"): 1, ~Charclass("b"): 2},
            1: {Charclass("b"): 1, ~Charclass("b"): 2},
            2: {Charclass("b"): 2, ~Charclass("b"): 2},
        },
    )

    assert (a | b).accepts(["a"])
    assert (a | b).accepts(["b"])
    assert (a & b).empty()
    assert (a + b).accepts(["a", "b"])
    assert (a ^ b).accepts(["a"])
    assert (a ^ b).accepts(["b"])


def test_new_set_methods(a: FixtureA, b: FixtureB) -> None:
    # A whole bunch of new methods were added to the FSM module to enable FSMs
    # to function exactly as if they were sets of strings (symbol lists), see:
    # https://docs.python.org/3/library/stdtypes.html#set-types-set-frozenset
    # But do they work?
    assert len(a) == 1
    assert len((a | b) * 4) == 16

    with pytest.raises(OverflowError):
        len(a.star())

    # "in"
    assert "a" in a
    assert "a" not in b

    # List comprehension!
    four = (a | b) * 2
    for string in four:
        assert string == [Charclass("a"), Charclass("a")]
        break
    assert tuple(four) == (
        [Charclass("a"), Charclass("a")],
        [Charclass("a"), Charclass("b")],
        [Charclass("b"), Charclass("a")],
        [Charclass("b"), Charclass("b")],
    )

    # set.union() imitation
    assert Fsm.union(a, b) == a.union(b)
    # pylint: disable-next=compare-to-zero
    assert len(Fsm.union()) == 0
    assert Fsm.intersection(a, b) == a.intersection(b)

    # This takes a little explaining. In general, `a & b & c` is equivalent to
    # `EVERYTHING & a & b & c` where `EVERYTHING` is an FSM accepting every
    # possible string. Similarly `a` is equivalent to `EVERYTHING & a`, and the
    # intersection of no sets at all is... `EVERYTHING`.
    int_none = Fsm.intersection()
    with pytest.raises(OverflowError):
        len(int_none)
    assert [] in int_none

    assert (a | b).difference(a) == Fsm.difference((a | b), a) == (a | b) - a == b
    assert (
        (a | b).difference(a, b)
        == Fsm.difference((a | b), a, b)
        == (a | b) - a - b
        == null({Charclass("a"), Charclass("b"), ~Charclass("ab")})
    )
    assert a.symmetric_difference(b) == Fsm.symmetric_difference(a, b) == a ^ b
    assert a.isdisjoint(b)
    assert a <= (a | b)
    assert a < (a | b)
    assert a != (a | b)
    assert (a | b) > a
    assert (a | b) >= a

    assert list(a.concatenate(a, a).strings()) == [[
        Charclass("a"),
        Charclass("a"),
        Charclass("a"),
    ]]
    assert list(a.concatenate().strings()) == [[Charclass("a")]]
    assert list(Fsm.concatenate(b, a, b).strings()) == [[
        Charclass("b"),
        Charclass("a"),
        Charclass("b"),
    ]]
    assert not list(Fsm.concatenate().strings())


def test_copy(a: FixtureA) -> None:
    # fsm.copy() and frozenset().copy() both preserve identity, because they
    # are immutable. This is just showing that we give the same behaviour.
    copyables: tuple[Fsm | frozenset[str], ...] = (a, frozenset("abc"))
    for x in copyables:
        assert x.copy() is x

    # Same, via the `__copy__` method.
    for x in copyables:
        assert copy(x) is x


def test_oblivion_crawl() -> None:
    # Old test from when we used to have a suppressed/secret "oblivion state"
    abc = Fsm(
        alphabet={Charclass("a"), Charclass("b"), Charclass("c"), ~Charclass("abc")},
        states={0, 1, 2, 3, 4},
        initial=0,
        finals={3},
        map={
            0: {Charclass("a"): 1, Charclass("b"): 2, Charclass("c"): 4, ~Charclass("abc"): 4},
            1: {Charclass("a"): 4, Charclass("b"): 2, Charclass("c"): 4, ~Charclass("abc"): 4},
            2: {Charclass("a"): 4, Charclass("b"): 4, Charclass("c"): 3, ~Charclass("abc"): 4},
            3: {Charclass("a"): 4, Charclass("b"): 4, Charclass("c"): 4, ~Charclass("abc"): 4},
            4: {Charclass("a"): 4, Charclass("b"): 4, Charclass("c"): 4, ~Charclass("abc"): 4},
        },
    )
    assert len((abc + abc).states) == 8
    assert len(abc.star().states) == 4
    assert len((abc * 3).states) == 11
    assert len(abc.reversed().states) == 5
    assert len((abc | abc).states) == 5
    assert len((abc & abc).states) == 5
    assert len((abc ^ abc).states) == 1
    assert len((abc - abc).states) == 1


def test_concatenate_bug(a: FixtureA) -> None:
    # This exposes a defect in Fsm.concatenate.
    assert Fsm.concatenate(a, epsilon({Charclass("a"), ~Charclass("a")}), a).accepts("aa")
    assert Fsm.concatenate(
        a,
        epsilon({Charclass("a"), ~Charclass("a")}),
        epsilon({Charclass("a"), ~Charclass("a")}),
        a
    ).accepts("aa")


def test_derive(a: FixtureA) -> None:
    # Just some basic tests because this is mainly a regex thing.
    assert a.derive([Charclass("a")]) == epsilon({Charclass("a"), Charclass("b"), ~Charclass("ab")})
    assert a.derive([Charclass("b")]) == null({Charclass("a"), Charclass("b"), ~Charclass("ab")})

    assert (a * 3).derive([Charclass("a")]) == a * 2
    assert (a.star() - epsilon({Charclass("a"), Charclass("b"), ~Charclass("ab")})).derive([Charclass("a")]) == a.star()


def test_bug_36() -> None:
    # This is /.*/
    etc1 = Fsm(
        alphabet={~Charclass()},
        states={0},
        initial=0,
        finals={0},
        map={
            0: {~Charclass(): 0},
        },
    )

    # This is /s.*/
    etc2 = Fsm(
        alphabet={Charclass("s"), ~Charclass("s")},
        states={0, 1, 2},
        initial=0,
        finals={1},
        map={
            0: {Charclass("s"): 1, ~Charclass("s"): 2},
            1: {Charclass("s"): 1, ~Charclass("s"): 1},
            2: {Charclass("s"): 2, ~Charclass("s"): 2},
        },
    )

    both = etc1 & etc2
    assert etc1.accepts([])
    assert etc1.accepts(["s"])
    assert etc1.accepts(["t", "s"])
    assert not etc2.accepts([])
    assert etc2.accepts(["s"])
    assert not etc2.accepts(["t", "s"])
    assert both.alphabet == {~Charclass("s"), Charclass("s")}
    assert both.accepts(["s"])


def test_add_anything_else() -> None:
    # [^a]
    fsm1 = Fsm(
        alphabet={Charclass("a"), ~Charclass("a")},
        states={0, 1, 2},
        initial=0,
        finals={1},
        map={
            0: {Charclass("a"): 2, ~Charclass("a"): 1},
            1: {Charclass("a"): 2, ~Charclass("a"): 1},
            2: {Charclass("a"): 2, ~Charclass("a"): 2},
        },
    )

    # [^b]
    fsm2 = Fsm(
        alphabet={Charclass("b"), ~Charclass("b")},
        states={0, 1, 2},
        initial=0,
        finals={1},
        map={
            0: {Charclass("b"): 2, ~Charclass("b"): 1},
            1: {Charclass("b"): 2, ~Charclass("b"): 1},
            2: {Charclass("b"): 2, ~Charclass("b"): 2},
        },
    )
    assert (fsm1 + fsm2).accepts("ba")


def test_anything_else_pickle() -> None:
    # [^z]*
    fsm1 = Fsm(
        alphabet={Charclass("z"), ~Charclass("z")},
        states={0, 1, 2},
        initial=0,
        finals={1},
        map={
            0: {Charclass("z"): 2, ~Charclass("z"): 1},
            1: {Charclass("z"): 2, ~Charclass("z"): 1},
            2: {Charclass("z"): 2, ~Charclass("z"): 2},
        },
    )

    fsm1_unpickled = pickle.loads(pickle.dumps(fsm1))

    # Newly-created instance.
    assert fsm1_unpickled is not fsm1

    # but equivalent.
    assert fsm1 == fsm1_unpickled

    assert fsm1_unpickled.alphabet == {Charclass("z"), ~Charclass("z")}


def test_replace_alphabet() -> None:
    # [^z]*
    fsm1 = Fsm(
        alphabet={Charclass("z"), ~Charclass("z")},
        states={0, 1, 2},
        initial=0,
        finals={1},
        map={
            0: {Charclass("z"): 2, ~Charclass("z"): 1},
            1: {Charclass("z"): 2, ~Charclass("z"): 1},
            2: {Charclass("z"): 2, ~Charclass("z"): 2},
        },
    )

    fsm2 = fsm1.replace_alphabet({
        Charclass("z"): [Charclass("a"), Charclass("b")],
        ~Charclass("z"): [Charclass("c"), ~Charclass("abc")]
    })

    assert fsm2.map == {
        0: {Charclass("a"): 2, Charclass("b"): 2, Charclass("c"): 1, ~Charclass("abc"): 1},
        1: {Charclass("a"): 2, Charclass("b"): 2, Charclass("c"): 1, ~Charclass("abc"): 1},
        2: {Charclass("a"): 2, Charclass("b"): 2, Charclass("c"): 2, ~Charclass("abc"): 2},
    }

def test_replace_alphabet_2() -> None:
    # [^z]*
    fsm1 = Fsm(
        alphabet={Charclass("z"), ~Charclass("z")},
        states={0, 1, 2},
        initial=0,
        finals={1},
        map={
            0: {Charclass("z"): 2, ~Charclass("z"): 1},
            1: {Charclass("z"): 2, ~Charclass("z"): 1},
            2: {Charclass("z"): 2, ~Charclass("z"): 2},
        },
    )

    fsm2 = fsm1.replace_alphabet({
        Charclass("z"): [~Charclass("z")],
        ~Charclass("z"): []
    })

    assert fsm2.map == {
        0: {~Charclass("z"): 2},
        1: {~Charclass("z"): 2},
        2: {~Charclass("z"): 2},
    }


def test_charclass_fsm() -> None:
    # "[^a]"
    nota = from_charclass(~Charclass("a"))
    assert nota.alphabet == {Charclass("a"), ~Charclass("a")}
    assert nota.accepts("b")
    assert nota.accepts(["b"])
    assert nota.accepts(["c"])


def test_charclass_fsm_2() -> None:
    bc = from_charclass(Charclass("bc"))
    assert bc.alphabet == {Charclass("b"), Charclass("c"), ~Charclass("bc")}
    assert bc.map == {
        0: {Charclass("b"): 1, Charclass("c"): 1, ~Charclass("bc"): 2},
        1: {Charclass("b"): 2, Charclass("c"): 2, ~Charclass("bc"): 2},
        2: {Charclass("b"): 2, Charclass("c"): 2, ~Charclass("bc"): 2},
    }


def test_charclass_fsm_3() -> None:
    notbc = from_charclass(~Charclass("bc"))
    assert notbc.alphabet == {Charclass("b"), Charclass("c"), ~Charclass("bc")}
    assert notbc.map == {
        0: {Charclass("b"): 2, Charclass("c"): 2, ~Charclass("bc"): 1},
        1: {Charclass("b"): 2, Charclass("c"): 2, ~Charclass("bc"): 2},
        2: {Charclass("b"): 2, Charclass("c"): 2, ~Charclass("bc"): 2},
    }


def test_unify_alphabets() -> None:
    a = Fsm(
        alphabet={Charclass("a"), ~Charclass("a")},
        states={0, 1, 2},
        initial=0,
        finals={1},
        map={
            0: {Charclass("a"): 1, ~Charclass("a"): 2},
            1: {Charclass("a"): 2, ~Charclass("a"): 2},
            2: {Charclass("a"): 2, ~Charclass("a"): 2},
        },
    )
    assert a.alphabet == {Charclass("a"), ~Charclass("a")}

    b = Fsm(
        alphabet={Charclass("b"), ~Charclass("b")},
        states={0, 1, 2},
        initial=0,
        finals={1},
        map={
            0: {Charclass("b"): 1, ~Charclass("b"): 2},
            1: {Charclass("b"): 2, ~Charclass("b"): 2},
            2: {Charclass("b"): 2, ~Charclass("b"): 2},
        },
    )
    assert b.alphabet == {Charclass("b"), ~Charclass("b")}

    [a2, b2] = unify_alphabets((a, b))
    assert a2.alphabet == {Charclass("a"), Charclass("b"), ~Charclass("ab")}
    assert a2.map == {
        0: {Charclass("a"): 1, Charclass("b"): 2, ~Charclass("ab"): 2},
        1: {Charclass("a"): 2, Charclass("b"): 2, ~Charclass("ab"): 2},
        2: {Charclass("a"): 2, Charclass("b"): 2, ~Charclass("ab"): 2},
    }
    assert b2.alphabet == {Charclass("a"), Charclass("b"), ~Charclass("ab")}
    assert b2.map == {
        0: {Charclass("a"): 2, Charclass("b"): 1, ~Charclass("ab"): 2},
        1: {Charclass("a"): 2, Charclass("b"): 2, ~Charclass("ab"): 2},
        2: {Charclass("a"): 2, Charclass("b"): 2, ~Charclass("ab"): 2},
    }
