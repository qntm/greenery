# -*- coding: utf-8 -*-

from .charclass import charclass, w, d, s, W, D, S, dot, nothing
from greenery import fsm

def test_charclass_equality():
    assert charclass("a") == charclass("a")
    assert ~charclass("a") == ~charclass("a")
    assert ~charclass("a") != charclass("a")
    assert charclass("ab") == charclass("ba")

def test_repr():
    assert repr(~charclass("a")) == "~charclass('a')"

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

def test_charclass_fsm():
    # "[^a]"
    nota = (~charclass("a")).to_fsm()
    assert nota.alphabet == {"a", fsm.anything_else}
    assert nota.accepts("b")
    assert nota.accepts(["b"])
    assert nota.accepts([fsm.anything_else])

def test_charclass_negation():
    assert ~~charclass("a") == charclass("a")
    assert charclass("a") == ~~charclass("a")

def test_empty():
    assert nothing.empty()
    assert not dot.empty()
