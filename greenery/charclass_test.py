from __future__ import annotations

import pytest

from .charclass import (
    DIGIT,
    DOT,
    NONDIGITCHAR,
    NONSPACECHAR,
    NONWORDCHAR,
    NULLCHARCLASS,
    SPACECHAR,
    WORDCHAR,
    Charclass,
)
from .fsm import ANYTHING_ELSE


def test_charclass_equality() -> None:
    assert Charclass("a") == Charclass("a")
    assert ~Charclass("a") == ~Charclass("a")
    assert ~Charclass("a") != Charclass("a")
    assert Charclass("ab") == Charclass("ba")


def test_charclass_ctor() -> None:
    with pytest.raises(TypeError):
        Charclass(frozenset({"a", ANYTHING_ELSE}))  # type: ignore

    with pytest.raises(ValueError):
        Charclass(frozenset({"a", "aa"}))

    assert Charclass("ab") == Charclass(frozenset({"a", "b"}))

    assert not Charclass("ab").negated
    assert not Charclass("ab", negated=False).negated
    assert Charclass("ab", negated=True).negated


def test_repr() -> None:
    assert repr(~Charclass("a")) == "~Charclass('a')"


def test_charclass_str() -> None:
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
    assert str(Charclass(
        "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz|"
    )) == "[0-9A-Z_a-z|]"
    assert str(NONWORDCHAR) == "\\W"
    assert str(NONDIGITCHAR) == "\\D"
    assert str(NONSPACECHAR) == "\\S"
    assert str(DOT) == "."
    assert str(~Charclass("")) == "."
    assert str(~Charclass("a")) == "[^a]"
    assert str(~Charclass("{")) == "[^{]"
    assert str(~Charclass("\t")) == "[^\\t]"
    assert str(~Charclass("^")) == "[^\\^]"


def test_charclass_fsm() -> None:
    # "[^a]"
    nota = (~Charclass("a")).to_fsm()
    assert nota.alphabet == {"a", ANYTHING_ELSE}
    assert nota.accepts("b")
    assert nota.accepts(["b"])
    assert nota.accepts([ANYTHING_ELSE])


def test_charclass_negation() -> None:
    assert ~~Charclass("a") == Charclass("a")
    assert Charclass("a") == ~~Charclass("a")


def test_charclass_union() -> None:
    # [ab] ∪ [bc] = [abc]
    assert Charclass("ab") | Charclass("bc") == Charclass("abc")
    # [ab] ∪ [^bc] = [^c]
    assert Charclass("ab") | ~Charclass("bc") == ~Charclass("c")
    # [^a] ∪ [bc] = [^a]
    assert ~Charclass("ab") | Charclass("bc") == ~Charclass("a")
    # [^ab] ∪ [^bc] = [^b]
    assert ~Charclass("ab") | ~Charclass("bc") == ~Charclass("b")

    assert Charclass.union() == NULLCHARCLASS

    assert Charclass.union(
        Charclass("ab"),
        Charclass("a"),
        Charclass("cd"),
    ) == Charclass("abcd")

    assert Charclass.union(
        Charclass("ab"),
        ~Charclass("abc"),
    ) == ~Charclass("c")


def test_charclass_intersection() -> None:
    # [ab] ∩ [bc] = [b]
    assert Charclass("ab") & Charclass("bc") == Charclass("b")
    # [ab] ∩ [^bc] = [a]
    assert Charclass("ab") & ~Charclass("bc") == Charclass("a")
    # [^ab] ∩ [bc] = [c]
    assert ~Charclass("ab") & Charclass("bc") == Charclass("c")
    # [^ab] ∩ [^bc] = [^abc]
    assert ~Charclass("ab") & ~Charclass("bc") == ~Charclass("abc")

    assert Charclass.intersection(
        Charclass("ab"),
        Charclass("bcd"),
        Charclass("abcde"),
    ) == Charclass("b")

    assert Charclass.intersection() == ~NULLCHARCLASS


def test_empty() -> None:
    assert NULLCHARCLASS.empty()
    assert not DOT.empty()
