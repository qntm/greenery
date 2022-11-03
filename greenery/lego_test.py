# -*- coding: utf-8 -*-

if __name__ == "__main__":
    raise Exception("Test files can't be run directly. Use `python -m pytest greenery`")

import pickle
from greenery.lego import conc, mult, charclass, one, emptystring, star, \
    plus, nothing, pattern, qm, d, multiplier, bound, w, s, W, D, S, dot, \
    inf, zero, from_fsm
from greenery.parse import parse
from greenery import fsm

# In general the idea for unit tests is that every unit test relies only on
# functionality which has already been unit-tested. If this isn't possible, then
# additional tests are required!

###############################################################################
# Equality tests. No point in comparing different regular expression elements
# in tests unless this part works

def test_charclass_equality():
    assert charclass("a") == charclass("a")
    assert ~charclass("a") == ~charclass("a")
    assert ~charclass("a") != charclass("a")
    assert charclass("ab") == charclass("ba")

def test_mult_equality():
    assert mult(charclass("a"), one) == mult(charclass("a"), one)
    assert mult(charclass("a"), one) != mult(charclass("b"), one)
    assert mult(charclass("a"), one) != mult(charclass("a"), qm)
    assert mult(charclass("a"), one) != mult(charclass("a"), multiplier(bound(1), bound(2)))
    assert mult(charclass("a"), one) != charclass("a")

def test_conc_equality():
    assert conc(mult(charclass("a"), one)) == conc(mult(charclass("a"), one))
    assert conc(mult(charclass("a"), one)) != conc(mult(charclass("b"), one))
    assert conc(mult(charclass("a"), one)) != conc(mult(charclass("a"), qm))
    assert conc(mult(charclass("a"), one)) != conc(mult(charclass("a"), multiplier(bound(1), bound(2))))
    assert conc(mult(charclass("a"), one)) != emptystring

def test_pattern_equality():
    assert pattern(
        conc(mult(charclass("a"), one)),
        conc(mult(charclass("b"), one)),
    ) == pattern(
        conc(mult(charclass("b"), one)),
        conc(mult(charclass("a"), one)),
    )
    assert pattern(
        conc(mult(charclass("a"), one)),
        conc(mult(charclass("a"), one)),
    ) == pattern(
        conc(mult(charclass("a"), one)),
    )

###############################################################################
# repr() tests

def test_repr():
    assert repr(~charclass("a")) == "~charclass('a')"

###############################################################################
# Stringification tests

def test_charclass_str():
    assert str(w) == "\\w"
    assert str(d) == "\\d"
    assert str(s) == "\\s"
    assert str(charclass("a")) == "a"
    assert str(charclass("{")) == "\\{"
    assert str(charclass("\t")) == "\\t"
    assert str(charclass("ab")) == "[ab]"
    assert str(charclass("a{")) == "[a{]"
    assert str(charclass("a\t")) == "[\\ta]"
    assert str(charclass("a-")) == "[\\-a]"
    assert str(charclass("a[")) == "[\\[a]"
    assert str(charclass("a]")) == "[\\]a]"
    assert str(charclass("ab")) == "[ab]"
    assert str(charclass("abc")) == "[abc]"
    assert str(charclass("abcd")) == "[a-d]"
    assert str(charclass("abcdfghi")) == "[a-df-i]"
    assert str(charclass("^")) == "^"
    assert str(charclass("\\")) == "\\\\"
    assert str(charclass("a^")) == "[\\^a]"
    assert str(charclass("0123456789a")) == "[0-9a]"
    assert str(charclass("\t\v\r A")) == "[\\t\\v\\r A]"
    assert str(charclass("\n\f A")) == "[\\n\\f A]"
    assert str(charclass("\t\n\v\f\r A")) == "[\\t-\\r A]"
    assert str(charclass("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz|")) == "[0-9A-Z_a-z|]"
    assert str(W) == "\\W"
    assert str(D) == "\\D"
    assert str(S) == "\\S"
    assert str(dot) == "."
    assert str(~charclass("")) == "."
    assert str(~charclass("a")) == "[^a]"
    assert str(~charclass("{")) == "[^{]"
    assert str(~charclass("\t")) == "[^\\t]"
    assert str(~charclass("^")) == "[^\\^]"

    # Arbitrary ranges
    assert str(parse("[\\w:;<=>?@\\[\\\\\\]\\^`]")) == "[0-z]"
    # TODO: what if \d is a proper subset of `chars`?

    # escape sequences are not preserved
    assert str(parse("\\x09")) == "\\t"

    # Printing ASCII control characters? You should get hex escapes
    assert str(parse("\\x00")) == "\\x00"

def test_mult_str():
    assert str(bound(2)) == "2"
    assert str(inf) == ""
    assert str(multiplier(bound(2), inf)) == "{2,}"

    a = charclass("a")
    assert str(mult(a, one)) == "a"
    assert str(mult(a, multiplier(bound(2), bound(2)))) == "a{2}"
    assert str(mult(a, multiplier(bound(3), bound(3)))) == "a{3}"
    assert str(mult(a, multiplier(bound(4), bound(4)))) == "a{4}"
    assert str(mult(a, multiplier(bound(5), bound(5)))) == "a{5}"
    assert str(mult(a, qm)) == "a?"
    assert str(mult(a, star)) == "a*"
    assert str(mult(a, plus)) == "a+"
    assert str(mult(a, multiplier(bound(2), bound(5)))) == "a{2,5}"
    assert str(mult(a, multiplier(bound(2), inf))) == "a{2,}"
    assert str(mult(d, one)) == "\\d"
    assert str(mult(d, multiplier(bound(2), bound(2)))) == "\\d{2}"
    assert str(mult(d, multiplier(bound(3), bound(3)))) == "\\d{3}"

def test_conc_str():
    assert str(conc(
        mult(charclass("a"), one),
        mult(charclass("b"), one),
        mult(charclass("c"), one),
        mult(charclass("d"), one),
        mult(charclass("e"), one),
        mult(~charclass("fg"), star),
        mult(charclass("h"), multiplier(bound(5), bound(5))),
        mult(charclass("abcdefghijklmnopqrstuvwxyz"), plus),
    )) == "abcde[^fg]*h{5}[a-z]+"

def test_pattern_str():
    assert str(pattern(
        conc(mult(charclass("a"), one)),
        conc(mult(charclass("b"), one)),
    )) == "a|b"
    assert str(pattern(
        conc(mult(charclass("a"), one)),
        conc(mult(charclass("a"), one)),
    )) == "a"
    assert str(pattern(
        conc(
            mult(charclass("a"), one),
            mult(charclass("b"), one),
            mult(charclass("c"), one),
        ),
        conc(
            mult(charclass("d"), one),
            mult(charclass("e"), one),
            mult(charclass("f"), one),
            mult(
                pattern(
                    conc(
                        mult(charclass("g"), one),
                        mult(charclass("h"), one),
                        mult(charclass("i"), one),
                    ),
                    conc(
                        mult(charclass("j"), one),
                        mult(charclass("k"), one),
                        mult(charclass("l"), one),
                    ),
                ), one
            ),
        ),
    )) == "abc|def(ghi|jkl)"

def test_parse_str_round_trip():
    assert str(parse("a.b")) == "a.b" # not "a[ab]b"
    assert str(parse("\\d{4}")) == "\\d{4}"
    assert str(parse("a.b()()")) == "a.b()()"

###############################################################################
# Test to_fsm() and alphabet-related functionality

def test_alphabet():
    # rxelems.alphabet() should include `fsm.anything_else`
    assert parse("").alphabet() == {fsm.anything_else}

def test_charclass_fsm():
    # "[^a]"
    nota = (~charclass("a")).to_fsm()
    assert nota.alphabet == {"a", fsm.anything_else}
    assert nota.accepts("b")
    assert nota.accepts(["b"])
    assert nota.accepts([fsm.anything_else])

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
    assert anota.accepts(["a", fsm.anything_else])
    assert not anota.accepts("ba")
    assert not anota.accepts("bb")

    # "0\\d"
    zeroD = parse("0\\d").to_fsm(d.chars)
    assert zeroD.accepts("01")
    assert not zeroD.accepts("10")

    # "\\d{2}"
    d2 = parse("\\d{2}").to_fsm(d.chars)
    assert not d2.accepts("")
    assert not d2.accepts("1")
    assert d2.accepts("11")
    assert not d2.accepts("111")

    # abc|def(ghi|jkl)
    conventional = parse("abc|def(ghi|jkl)").to_fsm(w.chars)
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

    bad = parse("0{2}|1{2}").to_fsm({"0", "1", fsm.anything_else})
    assert bad.accepts("00")
    assert bad.accepts("11")
    assert not bad.accepts("01")

    bad = parse("0{2}|1{2}").to_fsm()
    assert bad.accepts("00")
    assert bad.accepts("11")
    assert not bad.accepts("01")

def test_odd_bug():
    # Odd bug with ([bc]*c)?[ab]*
    int5A = mult(charclass("bc"), star).to_fsm({"a", "b", "c", fsm.anything_else})
    assert int5A.accepts([])
    assert int5A.accepts("")
    int5B = mult(charclass("c"), one).to_fsm({"a", "b", "c", fsm.anything_else})
    assert int5B.accepts("c")
    assert int5B.accepts(["c"])
    int5C = int5A + int5B
    assert int5C.accepts("c")
    assert int5C.accepts(["c"])

def test_bug_28():
    # Starification is broken in FSMs
    assert not parse("(ab*)").to_fsm().star().accepts("bb")
    assert not parse("(ab*)*").to_fsm().accepts("bb")

###############################################################################
# Test matches(). Quite sparse at the moment

def test_wildcards_in_charclasses():
    # Allow "\w", "\d" and "\s" in charclasses
    assert parse("[\\w~]*").matches("a0~")
    assert parse("[\\da]*").matches("0129a")
    assert parse("[\\s]+").matches(" \t \t ")

def test_block_comment_regex():
    # I went through several incorrect regexes for C block comments. Here we show
    # why the first few attempts were incorrect
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
    gen = charclass("xyz").strings()
    assert next(gen) == "x"
    assert next(gen) == "y"
    assert next(gen) == "z"
    try:
        next(gen)
        assert False
    except StopIteration:
        assert True

def test_mult_gen():
    # One term
    gen = parse("[ab]").strings()
    assert next(gen) == "a"
    assert next(gen) == "b"
    try:
        next(gen)
        assert False
    except StopIteration:
        assert True

    # No terms
    gen = parse("[ab]{0}").strings()
    assert next(gen) == ""
    try:
        next(gen)
        assert False
    except StopIteration:
        assert True

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
    try:
        next(gen)
        assert False
    except StopIteration:
        assert True

def test_pattern_generator():
    gen = parse("[ab]|[cde]").strings()
    assert next(gen) == "a"
    assert next(gen) == "b"
    assert next(gen) == "c"
    assert next(gen) == "d"
    assert next(gen) == "e"
    try:
        next(gen)
        assert False
    except StopIteration:
        assert True

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
    try:
        next(gen)
        assert False
    except StopIteration:
        assert True

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
    try:
        len(parse(".*"))
        assert False
    except OverflowError:
        assert True

###############################################################################

def test_copy():
    x = parse("abc|def(ghi|jkl)")
    assert x.copy() == x

###############################################################################
# Test from_fsm()

def test_dot():
    assert str(from_fsm(parse("a.b").to_fsm())) == "a.b" # not "a[ab]b"

def test_abstar():
    # Buggggs.
    abstar = fsm.fsm(
        alphabet={'a', fsm.anything_else, 'b'},
        states={0, 1},
        initial=0,
        finals={0},
        map={
            0: {'a': 0, fsm.anything_else: 1, 'b': 0},
            1: {'a': 1, fsm.anything_else: 1, 'b': 1}
        }
    )
    assert str(from_fsm(abstar)) == "[ab]*"

def test_adotb():
    adotb = fsm.fsm(
        alphabet={'a', fsm.anything_else, 'b'},
        states={0, 1, 2, 3, 4},
        initial=0,
        finals={4},
        map={
            0: {'a': 2, fsm.anything_else: 1, 'b': 1},
            1: {'a': 1, fsm.anything_else: 1, 'b': 1},
            2: {'a': 3, fsm.anything_else: 3, 'b': 3},
            3: {'a': 1, fsm.anything_else: 1, 'b': 4},
            4: {'a': 1, fsm.anything_else: 1, 'b': 1}
        }
    )
    assert str(from_fsm(adotb)) == "a.b"

def test_rxelems_recursion_error():
    # Catch a recursion error
    assert str(from_fsm(fsm.fsm(
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

def test_even_star_bug():
    # Bug fix. This is a(a{2})* (i.e. accepts an odd number of "a" chars in a
    # row), but when from_fsm() is called, the result is "a+". Turned out to be
    # a fault in the rxelems.multiplier.__mul__() routine
    elesscomplex = fsm.fsm(
        alphabet={"a"},
        states={0, 1},
        initial=0,
        finals={1},
        map={
            0 : {"a" : 1},
            1 : {"a" : 0},
        },
    )
    assert not elesscomplex.accepts("")
    assert elesscomplex.accepts("a")
    assert not elesscomplex.accepts("aa")
    assert elesscomplex.accepts("aaa")
    elesscomplex = from_fsm(elesscomplex)
    assert str(elesscomplex) in {"a(aa)*", "(aa)*a"}
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
    div3 = from_fsm(fsm.fsm(
        alphabet={"0", "1"},
        states={"initial", "zero", 0, 1, 2, None},
        initial="initial",
        finals={"zero", 0},
        map={
            "initial" : {"0" : "zero", "1" : 1},
            "zero"    : {"0" : None  , "1" : None},
            0         : {"0" : 0     , "1" : 1},
            1         : {"0" : 2     , "1" : 0},
            2         : {"0" : 1     , "1" : 2},
            None      : {"0" : None  , "1" : None},
        },
    ))
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
    divN = from_fsm(fsm.fsm(
        alphabet=set(str(i) for i in range(base)),
        states=set(range(N)) | {"initial", "zero", None},
        initial="initial",
        finals={"zero", 0},
        map=dict(
            [
                ("initial", dict([(str(j), j              % N) for j in range(1, base)] + [("0", "zero")])),
                ("zero"   , dict([(str(j), None              ) for j in range(   base)]                  )),
                (None     , dict([(str(j), None              ) for j in range(   base)]                  )),
            ] + [
                (i        , dict([(str(j), (i * base + j) % N) for j in range(   base)]                  ))
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
    # single characters or `fsm.anything_else`.
    for bad_symbol in [None, (), 0, ("a",), "", "aa", "ab", True]:
        f = fsm.fsm(
            alphabet={bad_symbol},
            states={0},
            initial=0,
            finals=set(),
            map={
                0 : {bad_symbol : 0}
            },
        )
        try:
            from_fsm(f)
            assert False
        except AssertionError as e:
            raise Exception("Accepted bad symbol: " + repr(bad_symbol))
        except Exception as e:
            pass

def test_dead_default():
    blockquote = from_fsm(fsm.fsm(
        alphabet={"/", "*", fsm.anything_else},
        states={0, 1, 2, 3, 4},
        initial=0,
        finals={4},
        map={
                0    : {"/" : 1},
                1    : {"*" : 2},
                2    : {"/" : 2, fsm.anything_else : 2, "*" : 3},
                3    : {"/" : 4, fsm.anything_else : 2, "*" : 3},
        }
    ))

###############################################################################
# charclass set operations

def test_charclass_negation():
    assert ~~charclass("a") == charclass("a")
    assert charclass("a") == ~~charclass("a")

def test_charclass_union():
    # [ab] u [bc] = [abc]
    assert charclass("ab") | charclass("bc") == charclass("abc")
    # [ab] u [^bc] = [^c]
    assert charclass("ab") | ~charclass("bc") == ~charclass("c")
    # [^a] u [bc] = [^a]
    assert ~charclass("ab") | charclass("bc") == ~charclass("a")
    # [^ab] u [^bc] = [^b]
    assert ~charclass("ab") | ~charclass("bc") == ~charclass("b")

def test_charclass_intersection():
    # [ab] n [bc] = [b]
    assert charclass("ab") & charclass("bc") == charclass("b")
    # [ab] n [^bc] = [a]
    assert charclass("ab") & ~charclass("bc") == charclass("a")
    # [^ab] n [bc] = [c]
    assert ~charclass("ab") & charclass("bc") == charclass("c")
    # [^ab] n [^bc] = [^abc]
    assert ~charclass("ab") & ~charclass("bc") == ~charclass("abc")

###############################################################################
# Emptiness detection

def test_empty():
    assert nothing.empty()
    assert charclass("").empty()
    assert not dot.empty()
    assert not parse("a{0}").empty()
    assert parse("[]").empty()
    assert not parse("[]?").empty()
    assert parse("a[]").empty()
    assert not parse("a[]?").empty()
    assert pattern().empty()
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
    everything = parse(".*")
    assert str(everything.everythingbut()) == str(nothing)
    assert str(nothing.everythingbut()) == str(everything)

def test_isinstance_bug():
    # Problem relating to isinstance(). The class "mult" was occurring as both
    # rxelems.mult and as __main__.mult and apparently these count as different
    # classes for some reason, so isinstance(m, mult) was returning false.
    starfree = (parse("").everythingbut() + parse("aa") + parse("").everythingbut()).everythingbut()

###############################################################################

def test_equivalence():
    assert parse("aa*").equivalent(parse("a*a"))
    assert parse("([ab]*a|[bc]*c)?b*").equivalent(parse("b*(a[ab]*|c[bc]*)?"))

###############################################################################
# Test reversed()

def test_regex_reversal():
    assert reversed(parse("b")) == parse("b")
    assert reversed(parse("e*")) == parse("e*")
    assert reversed(parse("bear")) == parse("raeb")
    assert reversed(parse("beer")) == parse("reeb")
    assert reversed(parse("abc|def|ghi")) == parse("cba|fed|ihg")
    assert reversed(parse("(abc)*d")) == parse("d(cba)*")

###############################################################################
# Bound and multiplier operation tests

def test_bound():
    assert min(bound(0), inf) == bound(0)
    assert min(bound(1), inf) == bound(1)
    assert qm.mandatory == bound(0)
    assert qm.optional == bound(1)

def test_multiplier_common():
    assert zero.common(zero) == zero
    assert zero.common(qm  ) == zero
    assert zero.common(one ) == zero
    assert zero.common(star) == zero
    assert zero.common(plus) == zero
    assert qm  .common(zero) == zero
    assert qm  .common(qm  ) == qm
    assert qm  .common(one ) == zero
    assert qm  .common(star) == qm
    assert qm  .common(plus) == qm
    assert one .common(zero) == zero
    assert one .common(qm  ) == zero
    assert one .common(one ) == one
    assert one .common(star) == zero
    assert one .common(plus) == one
    assert star.common(zero) == zero
    assert star.common(qm  ) == qm
    assert star.common(one ) == zero
    assert star.common(star) == star
    assert star.common(plus) == star
    assert plus.common(zero) == zero
    assert plus.common(qm  ) == qm
    assert plus.common(one ) == one
    assert plus.common(star) == star
    assert plus.common(plus) == plus

def test_multiplier_subtraction():
    # a{3,4}, a{2,5} -> a{2,3} (with a{1,1}, a{0,2} left over)
    assert multiplier(bound(3), bound(4)).common(multiplier(bound(2), bound(5))) == multiplier(bound(2), bound(3))
    assert multiplier(bound(3), bound(4)) - multiplier(bound(2), bound(3)) == one
    assert multiplier(bound(2), bound(5)) - multiplier(bound(2), bound(3)) == multiplier(bound(0), bound(2))

    # a{2,}, a{1,5} -> a{1,5} (with a{1,}, a{0,0} left over)
    assert multiplier(bound(2), inf).common(multiplier(bound(1), bound(5))) == multiplier(bound(1), bound(5))
    assert multiplier(bound(2), inf) - multiplier(bound(1), bound(5)) == plus
    assert multiplier(bound(1), bound(5)) - multiplier(bound(1), bound(5)) == zero

    # a{3,}, a{2,} -> a{2,} (with a, epsilon left over)
    assert multiplier(bound(3), inf).common(multiplier(bound(2), inf)) == multiplier(bound(2), inf)
    assert multiplier(bound(3), inf) - multiplier(bound(2), inf) == one
    assert multiplier(bound(2), inf) - multiplier(bound(2), inf) == zero

    # a{3,}, a{3,} -> a{3,} (with zero, zero left over)
    assert multiplier(bound(3), inf).common(multiplier(bound(3), inf)) == multiplier(bound(3), inf)
    assert multiplier(bound(3), inf) - multiplier(bound(3), inf) == zero

def test_multiplier_union():
    assert zero | zero == zero
    assert zero | qm   == qm
    assert zero | one  == qm
    assert zero | star == star
    assert zero | plus == star
    assert qm   | zero == qm
    assert qm   | qm   == qm
    assert qm   | one  == qm
    assert qm   | star == star
    assert qm   | plus == star
    assert one  | zero == qm
    assert one  | qm   == qm
    assert one  | one  == one
    assert one  | star == star
    assert one  | plus == plus
    assert star | zero == star
    assert star | qm   == star
    assert star | one  == star
    assert star | star == star
    assert star | plus == star
    assert plus | zero == star
    assert plus | qm   == star
    assert plus | one  == plus
    assert plus | star == star
    assert plus | plus == plus
    assert not zero.canunion(multiplier(bound(2), inf))
    assert not one.canunion(multiplier(bound(3), bound(4)))
    assert not multiplier(bound(8), inf).canunion(multiplier(bound(3), bound(4)))
    try:
        zero | multiplier(bound(7), bound(8))
        assert False
    except AssertionError:
        assert False
    except Exception:
        pass

###############################################################################
# Tests for some more set operations

def test_set_ops():
    assert parse("[abcd]") - parse("a") == charclass("bcd")
    assert parse("[abcd]") ^ parse("[cdef]") == charclass("abef")

###############################################################################
# Concatenation tests (+)

def test_concatenation():
    assert str((parse("a") + parse("b")).reduce()) == "ab"
    assert str((parse("a") + parse("b{0,8}")).reduce()) == "ab{0,8}"
    assert str((parse("a") + parse("bc")).reduce()) == "abc"
    assert str((parse("a") + parse("b|cd")).reduce()) == "a(b|cd)"
    assert str((parse("b{0,8}") + parse("c")).reduce()) == "b{0,8}c"
    assert str((parse("a{3,4}") + parse("b?")).reduce()) == "a{3,4}b?"
    assert str((parse("a{2}") + parse("bc")).reduce()) == "a{2}bc"
    assert str((parse("a{2,3}") + parse("b|cd")).reduce()) == "a{2,3}(b|cd)"
    assert str((parse("ab") + parse("c")).reduce()) == "abc"
    assert str((parse("ab") + parse("c*")).reduce()) == "abc*"
    assert str((parse("") + parse("")).reduce()) == ""
    assert str((parse("ab") + parse("cd")).reduce()) == "abcd"
    assert str((parse("za{2,3}") + parse("b|cd")).reduce()) == "za{2,3}(b|cd)"
    assert str((parse("a|bd") + parse("c")).reduce()) == "(a|bd)c"
    assert str((parse("b|cd") + parse("a{2,3}")).reduce()) == "(b|cd)a{2,3}"
    assert str((parse("b|cd") + parse("za{2,3}")).reduce()) == "(b|cd)za{2,3}"
    assert str((parse("a|bc") + parse("c|de")).reduce()) == "(a|bc)(c|de)"

###############################################################################
# Test methods for finding common parts of regular expressions.

def test_mult_common():
    assert mult(charclass("a"), multiplier(bound(3), bound(4))) \
        .common(mult(charclass("a"), multiplier(bound(2), bound(5)))) == \
        mult(charclass("a"), multiplier(bound(2), bound(3)))
    assert mult(charclass("a"), multiplier(bound(2), inf)) \
        .common(mult(charclass("a"), multiplier(bound(1), bound(5)))) == \
        mult(charclass("a"), multiplier(bound(1), bound(5)))
    assert mult(charclass("a"), multiplier(bound(3), inf)) \
        .common(mult(charclass("a"), multiplier(bound(2), inf))) == \
        mult(charclass("a"), multiplier(bound(2), inf))

def test_conc_common():
    a = mult(charclass("A"), one)
    b = mult(charclass("B"), one)
    c = mult(charclass("C"), one)
    y = mult(charclass("y"), one)
    z = mult(charclass("Z"), one)
    zstar = mult(charclass("Z"), star)

    assert conc(a, a, z, y).common(conc(b, b, z, y), suffix=True) == conc(z, y)
    assert conc(c, z).common(conc(c, z), suffix=True) == conc(c, z)
    assert conc(c, y).common(conc(c, z), suffix=True) == conc()
    assert conc(a, z).common(conc(b, z), suffix=True) == conc(z)
    assert conc(a, zstar).common(conc(b, z), suffix=True) == conc()
    assert conc(a).common(conc(b), suffix=True) == conc()

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
# Inverse concatenation tests: behead() and dock()

def test_mult_dock():
    assert mult(charclass("a"), multiplier(bound(4), bound(5))) \
        .dock(mult(charclass("a"), multiplier(bound(3), bound(3)))) == \
        mult(charclass("a"), multiplier(bound(1), bound(2)))

def test_conc_dock():
    a = mult(charclass("A"), one)
    b = mult(charclass("B"), one)
    x = mult(charclass("X"), one)
    x2 = mult(charclass("X"), multiplier(bound(2), bound(2)))
    yplus = mult(charclass("y"), plus)
    z = mult(charclass("Z"), one)

    assert conc(a, z).dock(conc(z)) == conc(a)
    assert conc(a, b, x, yplus, z).dock(conc(x, yplus, z)) == conc(a, b)
    assert conc(a, b, x, yplus, z).behead(conc(a, b, x, yplus)) == conc(z)
    assert conc(a).dock(conc()) == conc(a)

    try:
        conc(x2, yplus, z).behead(conc(x, yplus))
        assert False
    except AssertionError:
        assert False
    except Exception:
        pass

def test_pattern_dock():
    a = mult(charclass("a"), one)
    c = mult(charclass("c"), one)
    f = mult(charclass("f"), one)

    assert parse("a|bc").dock(conc()) == parse("a|bc")
    assert parse("aa|bca").dock(conc(a)) == parse("a|bc")
    assert parse("xyza|abca|a").dock(conc(a)) == parse("xyz|abc|")
    assert parse("f{2,3}c|fc").dock(conc(f, c)) == parse("f{1,2}|")
    assert parse("aa").dock(conc(a, a)) == parse("")

def test_pattern_beheading():
    a = mult(charclass("a"), one)
    c = mult(charclass("c"), one)
    f = mult(charclass("f"), one)
    z = mult(charclass("Z"), one)

    assert parse("aa").behead(conc(a)) == parse("a")
    assert parse("abc|aa").behead(conc(a)) == parse("a|bc")
    assert parse("cf{1,2}|cf").behead(conc(c)) == parse("f{1,2}|f")
    assert parse("aa|aa").behead(conc(a, a)) == parse("")
    assert parse("abc|aa").behead(conc(a)) == parse("a|bc")
    assert parse("a|bc").behead(conc()) == parse("a|bc")
    assert parse("cf{1,2}|cf").behead(conc(c, f)) == parse("f?|")
    assert parse("ZA|ZB|ZC").behead(conc(z)) == parse("A|B|C")
    assert parse("Z+A|ZB|ZZC").behead(conc(z)) == parse("Z*A|B|ZC")
    assert parse("a{2}b|a+c").behead(conc(a)) == parse("ab|a*c")

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

def test_charclass_multiplication():
    assert (parse("a") * one).reduce() == parse("a").reduce()
    assert (parse("a") * multiplier(bound(1), bound(3))).reduce() == parse("a{1,3}").reduce()
    assert (parse("a") * multiplier(bound(4), inf)).reduce() == parse("a{4,}").reduce()

def test_mult_multiplication():
    assert (parse("a{2,3}") * one).reduce() == \
        parse("a{2,3}").reduce()
    assert (parse("a{2,3}") * multiplier(bound(4), bound(5))).reduce() == \
        parse("a{8,15}").reduce()
    assert (parse("a{2,}") * multiplier(bound(2), inf)).reduce() == \
        parse("a{4,}").reduce()

def test_conc_multiplication():
    assert (parse("ab?") * qm).reduce() == parse("(ab?)?").reduce()

def test_pattern_multiplication():
    assert (parse("ab?|ba?") * multiplier(bound(2), bound(3))).reduce() == \
        parse("(ab?|ba?){2,3}").reduce()
    # TODO
    #assert (parse("(ab?|ba?)") * multiplier(bound(2), bound(3))).reduce() == \
    #    parse("(ab?|ba?){2,3}").reduce()

def test_even_star_bug():
    # Defect: (a{2})* should NOT reduce to a*
    a2 = mult(charclass("a"), multiplier(bound(2), bound(2)))
    a2star = a2 * star
    assert str(a2star) == "(a{2})*"

def test_two_or_more_qm_bug():
    assert str(mult(charclass("a"), multiplier(bound(2), inf)) * qm) == "(a{2,})?"

def test_two_two_bug():
    assert str(mult(charclass("a"), multiplier(bound(2), bound(2))) * multiplier(bound(2), bound(2))) == "a{4}"

###############################################################################
# Test intersection (&)

def test_mult_intersection():
    assert mult(charclass("a"), one) & mult(charclass("b"), qm) == charclass("")
    assert mult(charclass("a"), one) & mult(charclass("b"), qm) == nothing
    assert mult(charclass("a"), one) & mult(charclass("a"), qm) == charclass("a")
    assert mult(charclass("a"), multiplier(bound(2), bound(2))) & mult(charclass("a"), multiplier(bound(2), inf)) == mult(charclass("a"), multiplier(bound(2), bound(2)))
    assert mult(charclass("a"), one) & mult(charclass("b"), one) == charclass("")
    assert mult(charclass("a"), one) & mult(charclass("a"), one) == charclass("a")
    assert mult(charclass("a"), star) & mult(charclass("a"), one) == charclass("a")
    assert mult(charclass("a"), star) & mult(charclass("b"), star) == conc()
    assert mult(charclass("a"), star) & mult(charclass("a"), plus) == mult(charclass("a"), plus)
    assert mult(charclass("a"), multiplier(bound(2), bound(2))) & mult(charclass("a"), multiplier(bound(4), bound(4))) == charclass("")
    assert mult(charclass("a"), multiplier(bound(3), inf)) & mult(charclass("a"), multiplier(bound(3), inf)) == mult(charclass("a"), multiplier(bound(3), inf))

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
        "b*(a[ab]*|c[bc]*)?"
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
    assert str(parse("[ab]{0,2}") & parse("[^a]{0,2}")) == \
        "b{0,2}"
    assert str(parse("[ab]{0,4}") & parse("[^a]{0,4}")) == \
        "b{0,4}"
    assert str(parse("[abc]{0,8}") & parse("[^a]{0,8}")) == \
        "[bc]{0,8}"
    assert str(parse("[a-g0-8$%\\^]{0,8}") & parse("[^d]{0,8}")) == \
        "[$%0-8\\^abcefg]{0,8}"
    assert str(parse("[a-g0-8$%\\^]+") & parse("[^d]{0,8}")) == \
        "[$%0-8\\^abcefg]{1,8}"
    assert str(parse("[a-g0-8$%\\^]+") & parse("[^d]{2,8}")) == \
        "[$%0-8\\^abcefg]{2,8}"
    assert str(
        parse("\\W*") &
        parse("[a-g0-8$%\\^]+") &
        parse("[^d]{2,8}")
    ) == \
        "[$%\\^]{2,8}"
    assert str(parse("\\d{4}-\\d{2}-\\d{2}") & parse("19.*")) == \
        "19\\d{2}-\\d{2}-\\d{2}"

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
    long = \
    "(aa|bb*aa)a*|((ab|bb*ab)|(aa|bb*aa)a*b)((ab|bb*ab)|(aa|bb*aa)a*b)*" + \
    "(aa|bb*aa)a*|((ab|bb*ab)|(aa|bb*aa)a*b)((ab|bb*ab)|(aa|bb*aa)a*b)*"
    long = parse(long)
    long = reversed(long.to_fsm())
    long = reversed(from_fsm(long))
    assert str(long) == "[ab]*a[ab]"
    short = "[ab]*a?b*|[ab]*b?a*"
    assert str(parse(".*") & parse(short)) == "[ab]*"

###############################################################################
# reduce() tests

def test_mult_reduction_easy():
    assert parse("a").reduce() == charclass("a")
    assert parse("a").reduce() == charclass("a")
    assert parse("a?").reduce() == mult(charclass("a"), qm)
    assert parse("a{0}").reduce() == emptystring
    assert parse("[]").reduce() == nothing
    assert parse("[]?").reduce() == emptystring
    assert parse("[]{0}").reduce() == emptystring
    assert parse("[]{0,5}").reduce() == emptystring
    assert mult(pattern(), one).reduce() == nothing
    assert mult(pattern(), qm).reduce() == emptystring
    assert mult(pattern(), zero).reduce() == emptystring
    assert mult(pattern(), multiplier(bound(0), bound(5))).reduce() == emptystring


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


def test_empty_pattern_reduction():
    assert pattern().reduce() == charclass("")


def test_empty_mult_suppression():
    assert str(parse("[]0\\d").reduce()) == "[]"
    assert conc(
        mult(pattern(), one), # this can never actually match anything
        mult(charclass("0"), one),
        mult(charclass("0123456789"), one),
    ).reduce() == charclass("")


def test_empty_conc_suppression():
    assert parse("[]0\\d").reduce() == charclass("")
    assert pattern(
        conc(
            mult(pattern(), one),  # this can never actually match anything
            mult(charclass("0"), one),
            mult(charclass("0123456789"), one),
        ) # so neither can this conc
    ).reduce() == charclass("")


def test_nested_pattern_reduction():
    # a(d(ab|a*c)) -> ad(ab|a*c)
    assert str(parse("a(d(ab|a*c))").reduce()) == "ad(a*c|ab)"


def test_mult_factor_out_qm():
    # mult contains a pattern containing an empty conc? Pull the empty
    # part out where it's external
    assert str(parse("(a|b*|)").reduce()) == "(a|b*)?"
    assert str(parse("(a|b*|)c").reduce()) == "(a|b*)?c"
    # This happens even if emptystring is the only thing left inside the mult
    assert str(parse("()").reduce()) == ""
    assert str(parse("([$%\\^]|){1}").reduce()) == "[$%\\^]?"


def test_remove_unnecessary_parens():
    # mult contains a pattern containing a single conc containing a single
    # mult? That can be reduced greatly
    assert str(parse("(a){2}b").reduce()) == "a{2}b"
    assert str(parse("(a?)+b").reduce()) == "a*b"
    assert str(parse("([ab])*").reduce()) == "[ab]*"
    assert str(parse("(c{1,2}){3,4}").reduce()) == "c{3,8}"


def test_obvious_reduction():
    assert str(parse("(a|b)*").reduce()) == "[ab]*"


def test_mult_squoosh():
    # sequence squooshing of mults within a conc
    assert str(parse("[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]").reduce()) == \
        "[0-9A-Fa-f]{3}"
    assert str(parse("[$%\\^]?[$%\\^]").reduce()) == \
        "[$%\\^]{1,2}"
    assert str(parse(
        "(|(|(|(|(|[$%\\^])[$%\\^])[$%\\^])[$%\\^])[$%\\^])[$%\\^][$%\\^]"
    ).reduce()) == \
        "[$%\\^]{2,7}"


def test_bad_reduction_bug():
    # DEFECT: "0{2}|1{2}" was erroneously reduced() to "[01]{2}"
    assert parse("0{2}|1{2}").reduce() != \
        parse("[01]{2}")
    assert parse("0|[1-9]|ab").reduce() == \
        parse("\\d|ab")
    assert parse("0|[1-9]|a{5,7}").reduce() == \
        parse("\\d|a{5,7}")
    assert parse("0|(0|[1-9]|a{5,7})").reduce() == \
        parse("0|(\\d|a{5,7})")
    # TODO: should do better than this! Merge that 0


def test_common_prefix_pattern_reduction():
    assert str(parse("a{2}b|a+c").reduce()) == "a(a*c|ab)"


def test_epsilon_reduction():
    assert str(parse("|(ab)*|def").reduce()) == "(ab)*|def"
    assert str(parse("|(ab)+|def").reduce()) == "(ab)*|def"
    assert str(parse("|.+").reduce()) == ".*"
    assert str(parse("|a+|b+").reduce()) in {
        "a+|b*",
        "a*|b+"
    }


def test_charclass_intersection_2():
    assert (parse("[A-z]") & parse("[^g]")).reduce() == \
        parse("[A-fh-z]").reduce()


def test_reduce_boom():
    # make sure recursion problem in reduce() has gone away
    assert str(parse("([1-9]|0)").reduce()) == "\\d"


def test_new_reduce():
    # The @reduce_after decorator has been removed from many methods since it
    # takes unnecessary time which the user may not wish to spend.
    # This alters the behaviour of several methods and also exposes a new
    # opportunity for conc.reduce()
    assert str(parse("a()").reduce()) == "a"
    assert str(parse("a()()").reduce()) == "a"
    assert str(parse("a.b()()").reduce()) == "a.b"


def test_main_bug():
    assert str(parse("a*").reduce()) == \
        "a*"
    assert str(parse("a|a*").reduce()) == \
        "a*"
    assert str(parse("a{1,2}|a{3,4}|bc").reduce()) == \
        "a{1,4}|bc"
    assert str(parse("a{1,2}|bc|a{3,4}").reduce()) == \
        "a{1,4}|bc"
    assert str(parse("a{1,2}|a{3,4}|a{5,6}|bc").reduce()) == \
        "a{1,6}|bc"
    assert str(parse("a{1,2}|a{3}|a{5,6}").reduce()) == \
        "a{1,2}(a?|a{4})"
    assert str(parse("a{1,2}|a{3}|a{5,6}|bc").reduce()) == \
        "a{1,3}|a{5,6}|bc"
    assert str(parse("a{1,2}|a{4}|a{5,6}").reduce()) == \
        "a{1,2}(a{3,4})?"
    assert str(parse("a{1,2}|a{4}|a{5,6}|bc").reduce()) == \
        "a{1,2}|a{4,6}|bc"
    assert str((parse("a") | parse("a*")).reduce()) == \
        "a*"


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


def test_bug_slow():
    # issue #43
    import time
    m = fsm.fsm(
        alphabet={'R', 'L', 'U', 'D'},
        states={
            0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
            11, 12, 13, 14, 15, 16, 17, 18, 19, 20},
        initial=0,
        finals={20},
        map={
            0: {'D': 1, 'U': 2},
            1: {'L': 3},
            2: {'L': 4},
            3: {'U': 5},
            4: {'D': 6},
            5: {'R': 7},
            6: {'R': 8},
            7: {'U': 9},
            8: {'D': 10},
            9: {'L': 11},
            10: {'L': 12},
            11: {'L': 13},
            12: {'L': 14},
            13: {'D': 15},
            14: {'U': 16},
            15: {'R': 17},
            16: {'R': 18},
            17: {'D': 19},
            18: {'U': 19},
            19: {'L': 20},
            20: {}
        }
    )
    t1 = time.time()
    ll = from_fsm(m)
    t2 = time.time()
    assert (t2 - t1) < 60  # should finish in way under 1s
    assert ll == parse("(DLURULLDRD|ULDRDLLURU)L").reduce()


def test_bug_48_simpler():
    assert str(from_fsm(fsm.fsm(
        alphabet={'d'},
        states={0, 1},
        initial=0,
        finals={1},
        map={
            0: {'d': 1},
        },
    ))) == 'd'


def test_bug_48():
    S5, S26, S45, S63, S80, S97, S113, S127, S140, S152, S163, S175, S182 = \
        range(13)
    char0, char1, char2, char3, char4, char5, char6, char7, char8 = \
        '_', 'a', 'd', 'e', 'g', 'm', 'n', 'o', 'p'

    machine = fsm.fsm(
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
    assert str(rex) == 'damage_on_mp'


def test_pickle():
    f1 = parse("a{0,4}").to_fsm()
    f2 = parse("a{0,3}").to_fsm()

    assert f2 < f1

    f1_unpickled = pickle.loads(pickle.dumps(f1))
    f2_unpickled = pickle.loads(pickle.dumps(f2))

    assert f2_unpickled < f1_unpickled
