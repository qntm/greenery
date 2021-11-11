from __future__ import annotations

import pickle
import time

import pytest

from .charclass import DIGIT, WORDCHAR
from .fsm import ANYTHING_ELSE, Fsm
from .parse import parse
from .rxelems import from_fsm

# mypy: allow-untyped-calls
# mypy: allow-untyped-defs
# mypy: no-check-untyped-defs

if __name__ == "__main__":
    raise Exception(
        "Test files can't be run directly. Use `python -m pytest greenery`"
    )


###############################################################################
# Stringification tests


def test_charclass_str():
    # Arbitrary ranges
    assert str(parse("[\\w:;<=>?@\\[\\\\\\]\\^`]")) == "[0-z]"
    # TODO: what if \d is a proper subset of `chars`?

    # escape sequences are not preserved
    assert str(parse("\\x09")) == "\\t"

    # Printing ASCII control characters? You should get hex escapes
    assert str(parse("\\x00")) == "\\x00"


def test_parse_str_round_trip():
    # not "a[ab]b"
    assert str(parse("a.b")) == "a.b"
    assert str(parse("\\d{4}")) == "\\d{4}"
    assert str(parse("a.b()()")) == "a.b()()"


###############################################################################
# Test to_fsm() and alphabet-related functionality


def test_alphabet():
    # `.alphabet()` should include `ANYTHING_ELSE`
    assert parse("").alphabet() == {ANYTHING_ELSE}


def test_pattern_fsm():
    # "a[^a]"
    anota = parse("a[^a]").to_fsm()
    assert len(anota.states) == 3
    assert not anota.accepts("a")
    assert not anota.accepts(["a"])
    assert not anota.accepts("b")
    assert not anota.accepts(["b"])
    assert not anota.accepts("aa")
    assert not anota.accepts(["a", "a"])
    assert anota.accepts("ab")
    assert anota.accepts(["a", "b"])
    assert anota.accepts(["a", ANYTHING_ELSE])
    assert not anota.accepts("ba")
    assert not anota.accepts("bb")

    # "0\\d"
    zeroD = parse("0\\d").to_fsm(DIGIT.chars)
    assert zeroD.accepts("01")
    assert not zeroD.accepts("10")

    # "\\d{2}"
    d2 = parse("\\d{2}").to_fsm(DIGIT.chars)
    assert not d2.accepts("")
    assert not d2.accepts("1")
    assert d2.accepts("11")
    assert not d2.accepts("111")

    # abc|def(ghi|jkl)
    conventional = parse("abc|def(ghi|jkl)").to_fsm(WORDCHAR.chars)
    assert not conventional.accepts("a")
    assert not conventional.accepts("ab")
    assert conventional.accepts("abc")
    assert not conventional.accepts("abcj")
    assert conventional.accepts("defghi")
    assert conventional.accepts("defjkl")


def test_fsm():
    # You should be able to to_fsm() a single regular expression element
    # without supplying a specific alphabet. That should be determinable from
    # context.
    assert parse("a.b").to_fsm().accepts("acb")

    bad = parse("0{2}|1{2}").to_fsm({"0", "1", ANYTHING_ELSE})
    assert bad.accepts("00")
    assert bad.accepts("11")
    assert not bad.accepts("01")

    bad = parse("0{2}|1{2}").to_fsm()
    assert bad.accepts("00")
    assert bad.accepts("11")
    assert not bad.accepts("01")


def test_bug_28():
    # Starification was broken in FSMs
    assert not parse("(ab*)").to_fsm().star().accepts("bb")
    assert not parse("(ab*)*").to_fsm().accepts("bb")


###############################################################################
# Test matches(). Quite sparse at the moment


def test_wildcards_in_charclasses():
    # Allow "\w", "\d" and "\s" in `Charclass`es
    assert parse("[\\w~]*").matches("a0~")
    assert parse("[\\da]*").matches("0129a")
    assert parse("[\\s]+").matches(" \t \t ")


def test_block_comment_regex():
    # I went through several incorrect regexes for C block comments. Here we
    # show why the first few attempts were incorrect
    a = parse("/\\*(([^*]|\\*+[^*/])*)\\*/")
    assert a.matches("/**/")
    assert not a.matches("/***/")
    assert not a.matches("/****/")

    b = parse("/\\*(([^*]|\\*[^/])*)\\*/")
    assert b.matches("/**/")
    assert not b.matches("/***/")
    assert b.matches("/****/")

    c = parse("/\\*(([^*]|\\*+[^*/])*)\\*+/")
    assert c.matches("/**/")
    assert c.matches("/***/")
    assert c.matches("/****/")


def test_named_groups():
    a = parse("(?P<ng1>abc)")
    assert a.matches("abc")


def test_in():
    assert "a" in parse("a")
    assert "abcdsasda" in parse("\\w{4,10}")
    assert "abc" in parse("abc|def(ghi|jkl)")


###############################################################################
# Test string generators


def test_charclass_gen():
    gen = parse("[xyz]").strings()
    assert next(gen) == "x"
    assert next(gen) == "y"
    assert next(gen) == "z"

    with pytest.raises(StopIteration):
        next(gen)


def test_mult_gen():
    # One term
    gen = parse("[ab]").strings()
    assert next(gen) == "a"
    assert next(gen) == "b"

    with pytest.raises(StopIteration):
        next(gen)

    # No terms
    gen = parse("[ab]{0}").strings()
    assert next(gen) == ""

    with pytest.raises(StopIteration):
        next(gen)

    # Many terms
    gen = parse("[ab]*").strings()
    assert next(gen) == ""
    assert next(gen) == "a"
    assert next(gen) == "b"
    assert next(gen) == "aa"
    assert next(gen) == "ab"
    assert next(gen) == "ba"
    assert next(gen) == "bb"
    assert next(gen) == "aaa"


def test_conc_generator():
    gen = parse("[ab][cd]").strings()
    assert next(gen) == "ac"
    assert next(gen) == "ad"
    assert next(gen) == "bc"
    assert next(gen) == "bd"

    with pytest.raises(StopIteration):
        next(gen)


def test_pattern_generator():
    gen = parse("[ab]|[cde]").strings()
    assert next(gen) == "a"
    assert next(gen) == "b"
    assert next(gen) == "c"
    assert next(gen) == "d"
    assert next(gen) == "e"

    with pytest.raises(StopIteration):
        next(gen)

    # more complex
    gen = parse("abc|def(ghi|jkl)").strings()
    assert next(gen) == "abc"
    assert next(gen) == "defghi"
    assert next(gen) == "defjkl"

    gen = parse("[0-9a-fA-F]{3,10}").strings()
    assert next(gen) == "000"
    assert next(gen) == "001"
    assert next(gen) == "002"


def test_infinite_generation():
    # Infinite generator, flummoxes both depth-first and breadth-first searches
    gen = parse("a*b*").strings()
    assert next(gen) == ""
    assert next(gen) == "a"
    assert next(gen) == "b"
    assert next(gen) == "aa"
    assert next(gen) == "ab"
    assert next(gen) == "bb"
    assert next(gen) == "aaa"
    assert next(gen) == "aab"
    assert next(gen) == "abb"
    assert next(gen) == "bbb"
    assert next(gen) == "aaaa"


def test_wildcard_generator():
    # Generator needs to handle wildcards as well. Wildcards come last.
    gen = parse("a.b").strings(otherchar="*")
    assert next(gen) == "aab"
    assert next(gen) == "abb"
    assert next(gen) == "a*b"

    with pytest.raises(StopIteration):
        next(gen)


def test_forin():
    assert [s for s in parse("abc|def(ghi|jkl)")] == ["abc", "defghi", "defjkl"]


###############################################################################
# Test cardinality() and len()


def test_cardinality():
    assert parse("[]").cardinality() == 0
    assert parse("[]?").cardinality() == 1
    assert parse("[]{0,6}").cardinality() == 1
    assert parse("[ab]{3}").cardinality() == 8
    assert parse("[ab]{2,3}").cardinality() == 12
    assert len(parse("abc|def(ghi|jkl)")) == 3

    with pytest.raises(OverflowError):
        len(parse(".*"))


###############################################################################


def test_copy():
    x = parse("abc|def(ghi|jkl)")
    assert x.copy() == x


###############################################################################
# Test from_fsm()


def test_dot():
    # not "a[ab]b"
    assert str(from_fsm(parse("a.b").to_fsm())) == "a.b"


def test_abstar():
    # Buggggs.
    abstar = Fsm(
        alphabet={"a", ANYTHING_ELSE, "b"},
        states={0, 1},
        initial=0,
        finals={0},
        map={
            0: {"a": 0, ANYTHING_ELSE: 1, "b": 0},
            1: {"a": 1, ANYTHING_ELSE: 1, "b": 1},
        },
    )
    assert str(from_fsm(abstar)) == "[ab]*"


def test_adotb():
    adotb = Fsm(
        alphabet={"a", ANYTHING_ELSE, "b"},
        states={0, 1, 2, 3, 4},
        initial=0,
        finals={4},
        map={
            0: {"a": 2, ANYTHING_ELSE: 1, "b": 1},
            1: {"a": 1, ANYTHING_ELSE: 1, "b": 1},
            2: {"a": 3, ANYTHING_ELSE: 3, "b": 3},
            3: {"a": 1, ANYTHING_ELSE: 1, "b": 4},
            4: {"a": 1, ANYTHING_ELSE: 1, "b": 1},
        },
    )
    assert str(from_fsm(adotb)) == "a.b"


def test_rxelems_recursion_error():
    # Catch a recursion error
    assert str(from_fsm(Fsm(
        alphabet={"0", "1"},
        states={0, 1, 2, 3},
        initial=3,
        finals={1},
        map={
            0: {"0": 1, "1": 1},
            1: {"0": 2, "1": 2},
            2: {"0": 2, "1": 2},
            3: {"0": 0, "1": 2},
        }
    ))) == "0[01]"


def test_even_star_bug1():
    # Bug fix. This is a(a{2})* (i.e. accepts an odd number of "a" chars in a
    # row), but when from_fsm() is called, the result is "a+". Turned out to be
    # a fault in the rxelems.multiplier.__mul__() routine
    elesscomplex = Fsm(
        alphabet={"a"},
        states={0, 1},
        initial=0,
        finals={1},
        map={
            0: {"a": 1},
            1: {"a": 0},
        },
    )
    assert not elesscomplex.accepts("")
    assert elesscomplex.accepts("a")
    assert not elesscomplex.accepts("aa")
    assert elesscomplex.accepts("aaa")
    elesscomplex = from_fsm(elesscomplex)
    assert str(elesscomplex) in {"a(a{2})*", "(a{2})*a"}
    elesscomplex = elesscomplex.to_fsm()
    assert not elesscomplex.accepts("")
    assert elesscomplex.accepts("a")
    assert not elesscomplex.accepts("aa")
    assert elesscomplex.accepts("aaa")
    gen = elesscomplex.strings()
    assert next(gen) == ["a"]
    assert next(gen) == ["a", "a", "a"]
    assert next(gen) == ["a", "a", "a", "a", "a"]
    assert next(gen) == ["a", "a", "a", "a", "a", "a", "a"]


def test_binary_3():
    # Binary numbers divisible by 3.
    # Disallows the empty string
    # Allows "0" on its own, but not leading zeroes.
    div3 = from_fsm(Fsm(
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
    ))
    assert str(parse("(0|1)").reduce()) == "[01]"
    assert str(parse("(0|12)").reduce()) == "0|12"
    assert str(parse("(0|1(01*0|10*1)*10*)").reduce()) == "0|1(01*0|10*1)*10*"
    assert str(div3) == "0|1(01*0|10*1)*10*"
    gen = div3.strings()
    assert next(gen) == "0"
    assert next(gen) == "11"
    assert next(gen) == "110"
    assert next(gen) == "1001"
    assert next(gen) == "1100"


def test_base_N():
    # Machine accepts only numbers in selected base (e.g. 2, 10) that are
    # divisible by N (e.g. 3, 7).
    # "0" alone is acceptable, but leading zeroes (e.g. "00", "07") are not
    base = 2
    N = 3
    assert base <= 10
    divN = from_fsm(Fsm(
        alphabet={str(i) for i in range(base)},
        states=set(range(N)) | {"initial", "zero", None},
        initial="initial",
        finals={"zero", 0},
        map=dict(
            [
                (
                    "initial",
                    dict(
                        [(str(j), j % N) for j in range(1, base)]
                        + [("0", "zero")]
                    )
                ),
                (
                    "zero",
                    dict(
                        [(str(j), None) for j in range(base)]
                    )
                ),
                (
                    None,
                    dict(
                        [(str(j), None) for j in range(base)]
                    )
                ),
            ] + [
                (
                    i,
                    dict(
                        [(str(j), (i * base + j) % N) for j in range(base)]
                    )
                )
                for i in range(N)
            ]
        ),
    ))
    gen = divN.strings()
    a = next(gen)
    assert a == "0"
    for i in range(7):
        b = next(gen)
        assert int(a, base) + N == int(b, base)
        a = b


def test_bad_alphabet():
    # You can use anything you like in your FSM alphabet, but if you try to
    # convert it to an `rxelems` object then the only acceptable symbols are
    # single characters or `ANYTHING_ELSE`.
    for bad_symbol in [None, (), 0, ("a",), "", "aa", "ab", True]:
        f = Fsm(
            alphabet={bad_symbol},
            states={0},
            initial=0,
            finals=(),
            map={0: {bad_symbol: 0}},
        )

        with pytest.raises(Exception, match="Symbol.*cannot be used"):
            from_fsm(f)


def test_dead_default():
    blockquote = from_fsm(Fsm(
        alphabet={"/", "*", ANYTHING_ELSE},
        states={0, 1, 2, 3, 4},
        initial=0,
        finals={4},
        map={
            0: {"/": 1},
            1: {"*": 2},
            2: {"/": 2, ANYTHING_ELSE: 2, "*": 3},
            3: {"/": 4, ANYTHING_ELSE: 2, "*": 3},
        },
    ))
    assert str(blockquote) == "/\\*([^*]|\\*+[^*/])*\\*+/"


###############################################################################
# charclass set operations


def test_charclass_union():
    assert (parse("[ab]") | parse("[bc]")).reduce() == parse("[abc]")
    assert (parse("[ab]") | parse("[^bc]")).reduce() == parse("[^c]")
    assert (parse("[^ab]") | parse("[bc]")).reduce() == parse("[^a]")
    assert (parse("[^ab]") | parse("[^bc]")).reduce() == parse("[^b]")


def test_charclass_intersection():
    assert parse("[ab]") & parse("[bc]") == parse("b")
    assert parse("[ab]") & parse("[^bc]") == parse("a")
    assert parse("[^ab]") & parse("[bc]") == parse("c")
    assert parse("[^ab]") & parse("[^bc]") == parse("[^abc]")


###############################################################################
# Emptiness detection


def test_empty():
    assert not parse("a{0}").empty()
    assert parse("[]").empty()
    assert not parse("[]?").empty()
    assert parse("a[]").empty()
    assert not parse("a[]?").empty()
    assert not parse("a{0}").empty()
    assert not parse("[]?").empty()


###############################################################################
# Test everythingbut()


def test_everythingbut():
    # Regexes are usually gibberish but we make a few claims
    a = parse("a")
    notA = a.everythingbut().to_fsm()
    assert notA.accepts("")
    assert not notA.accepts("a")
    assert notA.accepts("aa")

    # everythingbut(), called twice, should take us back to where we started.
    beer = parse("beer")
    notBeer = beer.everythingbut()
    beer2 = notBeer.everythingbut()
    assert str(beer2) == "be{2}r"

    # ".*" becomes "[]" and vice versa under this call.
    assert str(parse(".*").everythingbut()) == "[]"
    assert str(parse("[]").everythingbut()) == ".*"


def test_isinstance_bug():
    # Problem relating to isinstance(). The class `Mult` was occurring as both
    # rxelems.Mult and as __main__.Mult and apparently these count as different
    # classes for some reason, so isinstance(m, Mult) was returning false.
    var = str(parse("").everythingbut()) + "aa" + str(parse("").everythingbut())
    assert var == ".+aa.+"

    starfree = parse(var).everythingbut()
    assert str(starfree) == "(.(a?[^a])*a{0,2})?"


###############################################################################


def test_equivalence():
    assert parse("aa*").equivalent(parse("a*a"))
    assert parse("([ab]*a|[bc]*c)?b*").equivalent(parse("b*(a[ab]*|c[bc]*)?"))


###############################################################################
# Test reversed()


def test_regex_reversal():
    assert parse("b").reversed() == parse("b")
    assert parse("e*").reversed() == parse("e*")
    assert parse("bear").reversed() == parse("raeb")
    assert parse("beer").reversed() == parse("reeb")
    assert parse("abc|def|ghi").reversed() == parse("cba|fed|ihg")
    assert parse("(abc)*d").reversed() == parse("d(cba)*")


###############################################################################
# Tests for some more set operations


def test_set_ops():
    assert parse("[abcd]") - parse("a") == parse("[bcd]")
    assert parse("[abcd]") ^ parse("[cdef]") == parse("[abef]")


###############################################################################
# Test methods for finding common parts of regular expressions.


def test_pattern_commonconc():
    assert str(parse("aa|aa")._commonconc()) == "aa"
    assert str(parse("abc|aa")._commonconc()) == "a"
    assert str(parse("a|bc")._commonconc()) == ""
    assert str(parse("cf{1,2}|cf")._commonconc()) == "cf"
    assert str(parse("ZA|ZB|ZC")._commonconc()) == "Z"
    assert str(parse("Z+A|ZB|ZZC")._commonconc()) == "Z"
    assert str(parse("a{2}b|a+c")._commonconc()) == "a"


def test_pattern_commonconc_suffix():
    assert str(parse("a|bc")._commonconc(suffix=True)) == ""
    assert str(parse("aa|bca")._commonconc(suffix=True)) == "a"
    assert str(parse("xyza|abca|a")._commonconc(suffix=True)) == "a"
    assert str(parse("f{2,3}c|fc")._commonconc(suffix=True)) == "fc"
    assert str(parse("aa")._commonconc(suffix=True)) == "aa"


###############################################################################
# Basic concatenation reduction tests


def test_reduce_concatenations():
    assert str(parse("aa").reduce()) == "a{2}"
    assert str(parse("bb").reduce()) == "b{2}"
    assert str(parse("b*b").reduce()) == "b+"
    assert str(parse("aa{2,}").reduce()) == "a{3,}"
    assert str(parse("a*a{2}").reduce()) == "a{2,}"
    assert str(parse("aa{0,8}").reduce()) == "a{1,9}"
    assert str(parse("b{0,8}b").reduce()) == "b{1,9}"
    assert str(parse("aab").reduce()) == "a{2}b"
    assert str(parse("abb").reduce()) == "ab{2}"
    assert str(parse("abb*").reduce()) == "ab+"
    assert str(parse("abbc").reduce()) == "ab{2}c"
    assert str(parse("a?ab").reduce()) == "a{1,2}b"
    assert str(parse("(ac{2}|bc+)c").reduce()) == "(ac|bc*)c{2}"
    assert str(parse("a(a{2}b|a+c)").reduce()) == "a{2}(a*c|ab)"
    assert str(parse("a{2,3}(a{2}b|a+c)").reduce()) == "a{3,4}(a*c|ab)"
    assert str(parse("(ba{2}|ca+)a{2,3}").reduce()) == "(ba|ca*)a{3,4}"
    assert str(parse("za{2,3}(a{2}b|a+c)").reduce()) == "za{3,4}(a*c|ab)"
    assert str(parse("(ba{2}|ca+)a{2,3}z").reduce()) == "(ba|ca*)a{3,4}z"
    assert str(parse("(a|bc)(a|bc)").reduce()) == "(a|bc){2}"
    assert str(parse("a+[ab]+").reduce()) == "a[ab]+"
    assert str(parse("a{3,8}[ab]+").reduce()) == "a{3}[ab]+"
    assert str(parse("[ab]+b+").reduce()) == "[ab]+b"
    assert str(parse("[ab]+a{3,8}").reduce()) == "[ab]+a{3}"
    assert str(parse("\\d+\\w+").reduce()) == "\\d\\w+"
    assert str(parse("[ab]+a?").reduce()) == "[ab]+"


###############################################################################
# Multiplication tests


def test_mult_multiplication():
    assert parse("(a{2,3}){1,1}").reduce() == parse("a{2,3}").reduce()
    assert parse("(a{2,3}){1}").reduce() == parse("a{2,3}").reduce()
    assert parse("(a{2,3})").reduce() == parse("a{2,3}").reduce()
    assert parse("(a{2,3}){4,5}").reduce() == parse("a{8,15}").reduce()
    assert parse("(a{2,}){2,}").reduce() == parse("a{4,}").reduce()


def test_even_star_bug2():
    # Defect: (a{2})* should NOT reduce to a*
    assert parse("(a{2})*").reduce() != parse("a*").reduce()


def test_two_or_more_qm_bug():
    assert str(parse("(a{2,})?").reduce()) == "(a{2,})?"


def test_two_two_bug():
    assert str(parse("(a{2}){2}").reduce()) == "a{4}"


###############################################################################
# Test intersection (&)


def test_mult_intersection():
    assert str(parse("a") & parse("a")) == "a"
    assert str(parse("a*") & parse("a")) == "a"
    assert str(parse("a") & parse("a?")) == "a"
    assert str(parse("a{2}") & parse("a{2,}")) == "a{2}"
    assert str(parse("a*") & parse("a+")) == "a+"
    assert str(parse("a{2}") & parse("a{4}")) == "[]"
    assert str(parse("a{3,}") & parse("a{3,}")) == "a{3,}"


def test_parse_regex_intersection():
    assert str(parse("a*") & parse("b*")) == ""
    assert str(parse("a") & parse("b")) == "[]"
    assert str(parse("\\d") & parse(".")) == "\\d"
    assert str(parse("\\d{2}") & parse("0.")) == "0\\d"
    assert str(parse("\\d{2}") & parse("19.*")) == "19"
    assert str(parse("\\d{3}") & parse("19.*")) == "19\\d"
    assert str(parse("abc...") & parse("...def")) == "abcdef"
    assert str(parse("[bc]*[ab]*") & parse("[ab]*[bc]*")) in {
        "([ab]*a|[bc]*c)?b*",
        "b*(a[ab]*|c[bc]*)?",
    }
    assert str(parse("\\W*")) == "\\W*"
    assert str(parse("[a-g0-8$%\\^]+")) == "[$%0-8\\^a-g]+"
    assert str(parse("[^d]{2,8}")) == "[^d]{2,8}"
    assert str(parse("\\W*") & parse("[a-g0-8$%\\^]+")) == "[$%\\^]+"
    assert str(parse("[ab]{1,2}") & parse("[^a]{1,2}")) == "b{1,2}"
    assert str(parse("[ab]?") & parse("[^a]?")) == "b?"
    assert parse("a{0,2}").matches("")
    assert parse("[ab]{0,2}").matches("")
    assert parse("[^a]{0,2}").matches("")
    assert parse("b{0,2}").matches("")
    assert str(parse("[ab]{0,2}") & parse("[^a]{0,2}")) == "b{0,2}"
    assert str(parse("[ab]{0,4}") & parse("[^a]{0,4}")) == "b{0,4}"
    assert str(parse("[abc]{0,8}") & parse("[^a]{0,8}")) == "[bc]{0,8}"
    assert (
        str(parse("[a-g0-8$%\\^]{0,8}") & parse("[^d]{0,8}"))
        == "[$%0-8\\^abcefg]{0,8}"
    )
    assert (
        str(parse("[a-g0-8$%\\^]+") & parse("[^d]{0,8}"))
        == "[$%0-8\\^abcefg]{1,8}"
    )
    assert (
        str(parse("[a-g0-8$%\\^]+") & parse("[^d]{2,8}"))
        == "[$%0-8\\^abcefg]{2,8}"
    )
    assert (
        str(parse("\\W*") & parse("[a-g0-8$%\\^]+") & parse("[^d]{2,8}"))
        == "[$%\\^]{2,8}"
    )
    assert (
        str(parse("\\d{4}-\\d{2}-\\d{2}") & parse("19.*"))
        == "19\\d{2}-\\d{2}-\\d{2}"
    )


def test_complexify():
    # Complexify!
    gen = (parse("[bc]*[ab]*") & parse("[ab]*[bc]*")).strings()
    assert next(gen) == ""
    assert next(gen) == "a"
    assert next(gen) == "b"
    assert next(gen) == "c"
    assert next(gen) == "aa"
    assert next(gen) == "ab"
    # no "ac"
    assert next(gen) == "ba"
    assert next(gen) == "bb"
    assert next(gen) == "bc"
    # no "ca"
    assert next(gen) == "cb"
    assert next(gen) == "cc"
    assert next(gen) == "aaa"


def test_silly_reduction():
    # This one is horrendous and we have to jump through some hoops to get to
    # a sensible result. Probably not a good unit test actually.
    long = (
        "(aa|bb*aa)a*|((ab|bb*ab)|(aa|bb*aa)a*b)"
        + "((ab|bb*ab)|(aa|bb*aa)a*b)*"
        + "(aa|bb*aa)a*|((ab|bb*ab)|(aa|bb*aa)a*b)"
        + "((ab|bb*ab)|(aa|bb*aa)a*b)*"
    )
    long = parse(long)
    long = long.to_fsm().reversed()
    long = from_fsm(long).reversed()
    assert str(long) == "[ab]*a[ab]"
    short = "[ab]*a?b*|[ab]*b?a*"
    assert str(parse(".*") & parse(short)) == "[ab]*"


###############################################################################
# reduce() tests


def test_mult_reduction_easy():
    assert str(parse("a").reduce()) == "a"
    assert str(parse("a").reduce()) == "a"
    assert str(parse("a?").reduce()) == "a?"
    assert str(parse("a{0}").reduce()) == ""
    assert str(parse("[]").reduce()) == "[]"
    assert str(parse("[]?").reduce()) == ""
    assert str(parse("[]{0}").reduce()) == ""
    assert str(parse("[]{0,5}").reduce()) == ""


def test_conc_reduction_basic():
    assert str(parse("a").reduce()) == "a"
    assert str(parse("a{3,4}").reduce()) == "a{3,4}"
    assert str(parse("ab").reduce()) == "ab"
    assert str(parse("a[]b").reduce()) == "[]"


def test_pattern_reduce_basic():
    assert str(parse("ab|cd").reduce()) == "ab|cd"
    assert str(parse("a{2}b{2}").reduce()) == "a{2}b{2}"
    assert str(parse("a{2}").reduce()) == "a{2}"
    assert str(parse("a").reduce()) == "a"
    assert str(parse("(((a)))").reduce()) == "a"


def test_empty_conc_suppression():
    assert str(parse("[]0\\d").reduce()) == "[]"


def test_nested_pattern_reduction():
    # a(d(ab|a*c)) -> ad(ab|a*c)
    assert str(parse("a(d(ab|a*c))").reduce()) == "ad(a*c|ab)"


def test_mult_factor_out_qm():
    # `Mult` contains a `Pattern` containing an empty `Conc`? Pull the empty
    # part out where it's external
    assert str(parse("a|b*|").reduce()) == "a|b*"
    assert str(parse("(a|b*|)").reduce()) == "a|b*"
    assert str(parse("(a|b*|)c").reduce()) == "(a|b*)c"
    # This happens even if `EMPTYSTRING` is the only thing left inside the
    # `Mult`
    assert str(parse("()").reduce()) == ""
    assert str(parse("([$%\\^]|){1}").reduce()) == "[$%\\^]?"


def test_remove_unnecessary_parens():
    # `Mult` contains a `Pattern` containing a single `Conc` containing a
    # single `Mult`? That can be reduced greatly
    assert str(parse("(a){2}b").reduce()) == "a{2}b"
    assert str(parse("(a?)+b").reduce()) == "a*b"
    assert str(parse("([ab])*").reduce()) == "[ab]*"
    assert str(parse("(c{1,2}){3,4}").reduce()) == "c{3,8}"


def test_obvious_reduction():
    assert str(parse("(a|b)*").reduce()) == "[ab]*"


def test_mult_squoosh():
    # sequence squooshing of mults within a `Conc`
    assert (
        str(parse("[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]").reduce())
        == "[0-9A-Fa-f]{3}"
    )
    assert str(parse("[$%\\^]?[$%\\^]").reduce()) == "[$%\\^]{1,2}"
    assert (
        str(parse(
            "(|(|(|(|(|[$%\\^])[$%\\^])[$%\\^])[$%\\^])[$%\\^])[$%\\^][$%\\^]"
        ).reduce()) == "[$%\\^]{2,7}"
    )


def test_bad_reduction_bug():
    # DEFECT: "0{2}|1{2}" was erroneously reduced() to "[01]{2}"
    assert parse("0{2}|1{2}").reduce() != parse("[01]{2}")
    assert parse("0|[1-9]|ab").reduce() == parse("\\d|ab")
    assert parse("0|[1-9]|a{5,7}").reduce() == parse("\\d|a{5,7}")
    assert parse("0|(0|[1-9]|a{5,7})").reduce() == parse("0|(\\d|a{5,7})")
    # TODO: should do better than this! Merge that 0


def test_common_prefix_pattern_reduction():
    assert str(parse("a{2}b|a+c").reduce()) == "a(a*c|ab)"


def test_epsilon_reduction():
    assert str(parse("|(ab)*|def").reduce()) == "(ab)*|def"
    assert str(parse("|(ab)+|def").reduce()) == "(ab)*|def"
    assert str(parse("|.+").reduce()) == ".*"
    assert str(parse("|a+|b+").reduce()) in {"a+|b*", "a*|b+"}


def test_charclass_intersection_2():
    assert (
        (parse("[A-z]") & parse("[^g]")).reduce()
        == parse("[A-fh-z]").reduce()
    )


def test_reduce_boom():
    # make sure recursion problem in reduce() has gone away
    assert str(parse("([1-9]|0)").reduce()) == "\\d"


def test_new_reduce():
    # The @reduce_after decorator has been removed from many methods since it
    # takes unnecessary time which the user may not wish to spend.
    # This alters the behaviour of several methods and also exposes a new
    # opportunity for `Conc.reduce()`
    assert str(parse("a()").reduce()) == "a"
    assert str(parse("a()()").reduce()) == "a"
    assert str(parse("a.b()()").reduce()) == "a.b"


def test_main_bug():
    assert str(parse("a*").reduce()) == "a*"
    assert str(parse("a|a*").reduce()) == "a*"
    assert str(parse("a{1,2}|a{3,4}|bc").reduce()) == "a{1,4}|bc"
    assert str(parse("a{1,2}|bc|a{3,4}").reduce()) == "a{1,4}|bc"
    assert str(parse("a{1,2}|a{3,4}|a{5,6}|bc").reduce()) == "a{1,6}|bc"
    assert str(parse("a{1,2}|a{3}|a{5,6}").reduce()) == "a{1,2}(a?|a{4})"
    assert str(parse("a{1,2}|a{3}|a{5,6}|bc").reduce()) == "a{1,3}|a{5,6}|bc"
    assert str(parse("a{1,2}|a{4}|a{5,6}").reduce()) == "a{1,2}(a{3,4})?"
    assert str(parse("a{1,2}|a{4}|a{5,6}|bc").reduce()) == "a{1,2}|a{4,6}|bc"
    assert str((parse("a") | parse("a*")).reduce()) == "a*"


def test_bug_28_b():
    # Defect in rxelems.to_fsm()
    assert not parse("(ab*)*").to_fsm().accepts("bb")


def test_derive():
    assert str(parse("a+").derive("a")) == "a*"
    assert str(parse("a+|b+").derive("a")) == "a*"
    assert str(parse("abc|ade").derive("a")) == "bc|de"
    assert str(parse("abc|ade").derive("ab")) == "c"


def test_bug_36_1():
    etc1 = parse(".*").to_fsm()
    etc2 = parse("s.*").to_fsm()
    assert etc1.accepts("s")
    assert etc2.accepts("s")
    assert not etc1.isdisjoint(etc2)
    assert not etc2.isdisjoint(etc1)


def test_bug_36_2():
    etc1 = parse("/etc/.*").to_fsm()
    etc2 = parse("/etc/something.*").to_fsm()
    assert etc1.accepts("/etc/something")
    assert etc2.accepts("/etc/something")
    assert not etc1.isdisjoint(etc2)
    assert not etc2.isdisjoint(etc1)


def test_isdisjoint():
    xyzzy = parse("xyz(zy)?")
    xyz = parse("xyz")
    blippy = parse("blippy")
    assert xyzzy.isdisjoint(blippy)
    assert not xyzzy.isdisjoint(xyz)


def test_bug_slow():
    # issue #43
    m = Fsm(
        alphabet={"R", "L", "U", "D"},
        states={
            0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
            11, 12, 13, 14, 15, 16, 17, 18, 19, 20},
        initial=0,
        finals={20},
        map={
            0: {"D": 1, "U": 2},
            1: {"L": 3},
            2: {"L": 4},
            3: {"U": 5},
            4: {"D": 6},
            5: {"R": 7},
            6: {"R": 8},
            7: {"U": 9},
            8: {"D": 10},
            9: {"L": 11},
            10: {"L": 12},
            11: {"L": 13},
            12: {"L": 14},
            13: {"D": 15},
            14: {"U": 16},
            15: {"R": 17},
            16: {"R": 18},
            17: {"D": 19},
            18: {"U": 19},
            19: {"L": 20},
            20: {},
        },
    )
    t1 = time.time()
    ll = from_fsm(m)
    t2 = time.time()
    assert (t2 - t1) < 60  # should finish in way under 1s
    assert ll == parse("(DLURULLDRD|ULDRDLLURU)L").reduce()


def test_bug_48_simpler():
    assert str(from_fsm(Fsm(
        alphabet={"d"},
        states={0, 1},
        initial=0,
        finals={1},
        map={
            0: {"d": 1},
        },
    ))) == "d"


def test_bug_48():
    (
        S5, S26, S45, S63, S80, S97, S113, S127, S140, S152, S163, S175, S182
    ) = range(13)
    char0, char1, char2, char3, char4, char5, char6, char7, char8 = (
        "_",
        "a",
        "d",
        "e",
        "g",
        "m",
        "n",
        "o",
        "p",
    )

    machine = Fsm(
        alphabet={
            char0, char1, char2, char3, char4,
            char5, char6, char7, char8
        },
        states={
            S5, S26, S45, S63, S80, S97, S113,
            S127, S140, S152, S163, S175, S182
        },
        initial=S5,
        finals={S182},
        map={
            S113: {char0: S127},
            S127: {char7: S140},
            S140: {char6: S152},
            S152: {char0: S163},
            S163: {char5: S175},
            S175: {char8: S182},
            S26: {char1: S45},
            S45: {char5: S63},
            S5: {char2: S26},
            S63: {char1: S80},
            S80: {char4: S97},
            S97: {char3: S113},
        },
    )

    rex = from_fsm(machine)
    assert str(rex) == "damage_on_mp"


def test_pickle():
    f1 = parse("a{0,4}").to_fsm()
    f2 = parse("a{0,3}").to_fsm()

    assert f2 < f1

    f1_unpickled = pickle.loads(pickle.dumps(f1))
    f2_unpickled = pickle.loads(pickle.dumps(f2))

    assert f2_unpickled < f1_unpickled
