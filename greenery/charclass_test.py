# -*- coding: utf-8 -*-

from .charclass import Charclass, WORDCHAR, DIGIT, SPACECHAR, W, D, S, DOT, NULLCHARCLASS
from .fsm import ANYTHING_ELSE

def test_charclass_equality():
    assert Charclass("a") == Charclass("a")
    assert ~Charclass("a") == ~Charclass("a")
    assert ~Charclass("a") != Charclass("a")
    assert Charclass("ab") == Charclass("ba")

def test_repr():
    assert repr(~Charclass("a")) == "~Charclass('a')"

def test_charclass_str():
    assert str(WORDCHAR) == "\\w"
    assert str(DIGIT) == "\\d"
    assert str(SPACECHAR) == "\\s"
    assert str(Charclass("a")) == "a"
    assert str(Charclass("{")) == "\\{"
    assert str(Charclass("\t")) == "\\t"
    assert str(Charclass("ab")) == "[ab]"
    assert str(Charclass("a{")) == "[a{]"
    assert str(Charclass("a\t")) == "[\\ta]"
    assert str(Charclass("a-")) == "[\\-a]"
    assert str(Charclass("a[")) == "[\\[a]"
    assert str(Charclass("a]")) == "[\\]a]"
    assert str(Charclass("ab")) == "[ab]"
    assert str(Charclass("abc")) == "[abc]"
    assert str(Charclass("abcd")) == "[a-d]"
    assert str(Charclass("abcdfghi")) == "[a-df-i]"
    assert str(Charclass("^")) == "^"
    assert str(Charclass("\\")) == "\\\\"
    assert str(Charclass("a^")) == "[\\^a]"
    assert str(Charclass("0123456789a")) == "[0-9a]"
    assert str(Charclass("\t\v\r A")) == "[\\t\\v\\r A]"
    assert str(Charclass("\n\f A")) == "[\\n\\f A]"
    assert str(Charclass("\t\n\v\f\r A")) == "[\\t-\\r A]"
    assert str(Charclass("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz|")) == "[0-9A-Z_a-z|]"
    assert str(W) == "\\W"
    assert str(D) == "\\D"
    assert str(S) == "\\S"
    assert str(DOT) == "."
    assert str(~Charclass("")) == "."
    assert str(~Charclass("a")) == "[^a]"
    assert str(~Charclass("{")) == "[^{]"
    assert str(~Charclass("\t")) == "[^\\t]"
    assert str(~Charclass("^")) == "[^\\^]"

def test_charclass_fsm():
    # "[^a]"
    nota = (~Charclass("a")).to_fsm()
    assert nota.alphabet == {"a", ANYTHING_ELSE}
    assert nota.accepts("b")
    assert nota.accepts(["b"])
    assert nota.accepts([ANYTHING_ELSE])

def test_charclass_negation():
    assert ~~Charclass("a") == Charclass("a")
    assert Charclass("a") == ~~Charclass("a")

def test_empty():
    assert NULLCHARCLASS.empty()
    assert not DOT.empty()
