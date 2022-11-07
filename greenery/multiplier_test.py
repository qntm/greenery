# -*- coding: utf-8 -*-

from .bound import Bound, INF
from .multiplier import Multiplier, ZERO, ONE, QM, STAR, PLUS


def test_multiplier_str():
    assert str(Multiplier(Bound(2), INF)) == "{2,}"


def test_bound_qm():
    assert QM.mandatory == Bound(0)
    assert QM.optional == Bound(1)


def test_multiplier_common():
    assert ZERO.common(ZERO) == ZERO
    assert ZERO.common(QM) == ZERO
    assert ZERO.common(ONE) == ZERO
    assert ZERO.common(STAR) == ZERO
    assert ZERO.common(PLUS) == ZERO
    assert QM.common(ZERO) == ZERO
    assert QM.common(QM) == QM
    assert QM.common(ONE) == ZERO
    assert QM.common(STAR) == QM
    assert QM.common(PLUS) == QM
    assert ONE.common(ZERO) == ZERO
    assert ONE.common(QM) == ZERO
    assert ONE.common(ONE) == ONE
    assert ONE.common(STAR) == ZERO
    assert ONE.common(PLUS) == ONE
    assert STAR.common(ZERO) == ZERO
    assert STAR.common(QM) == QM
    assert STAR.common(ONE) == ZERO
    assert STAR.common(STAR) == STAR
    assert STAR.common(PLUS) == STAR
    assert PLUS.common(ZERO) == ZERO
    assert PLUS.common(QM) == QM
    assert PLUS.common(ONE) == ONE
    assert PLUS.common(STAR) == STAR
    assert PLUS.common(PLUS) == PLUS


def test_multiplier_subtraction():
    # a{3,4}, a{2,5} -> a{2,3} (with a{1,1}, a{0,2} left over)
    assert Multiplier(Bound(3), Bound(4)) \
        .common(Multiplier(Bound(2), Bound(5))) \
        == Multiplier(Bound(2), Bound(3))
    assert Multiplier(Bound(3), Bound(4)) - Multiplier(Bound(2), Bound(3)) \
        == ONE
    assert Multiplier(Bound(2), Bound(5)) - Multiplier(Bound(2), Bound(3)) \
        == Multiplier(Bound(0), Bound(2))

    # a{2,}, a{1,5} -> a{1,5} (with a{1,}, a{0,0} left over)
    assert Multiplier(Bound(2), INF).common(Multiplier(Bound(1), Bound(5))) \
        == Multiplier(Bound(1), Bound(5))
    assert Multiplier(Bound(2), INF) - Multiplier(Bound(1), Bound(5)) \
        == PLUS
    assert Multiplier(Bound(1), Bound(5)) - Multiplier(Bound(1), Bound(5)) \
        == ZERO

    # a{3,}, a{2,} -> a{2,} (with a, epsilon left over)
    assert Multiplier(Bound(3), INF).common(Multiplier(Bound(2), INF)) \
        == Multiplier(Bound(2), INF)
    assert Multiplier(Bound(3), INF) - Multiplier(Bound(2), INF) == ONE
    assert Multiplier(Bound(2), INF) - Multiplier(Bound(2), INF) == ZERO

    # a{3,}, a{3,} -> a{3,} (with ZERO, ZERO left over)
    assert Multiplier(Bound(3), INF).common(Multiplier(Bound(3), INF)) \
        == Multiplier(Bound(3), INF)
    assert Multiplier(Bound(3), INF) - Multiplier(Bound(3), INF) == ZERO


def test_multiplier_union():
    assert ZERO | ZERO == ZERO
    assert ZERO | QM == QM
    assert ZERO | ONE == QM
    assert ZERO | STAR == STAR
    assert ZERO | PLUS == STAR
    assert QM | ZERO == QM
    assert QM | QM == QM
    assert QM | ONE == QM
    assert QM | STAR == STAR
    assert QM | PLUS == STAR
    assert ONE | ZERO == QM
    assert ONE | QM == QM
    assert ONE | ONE == ONE
    assert ONE | STAR == STAR
    assert ONE | PLUS == PLUS
    assert STAR | ZERO == STAR
    assert STAR | QM == STAR
    assert STAR | ONE == STAR
    assert STAR | STAR == STAR
    assert STAR | PLUS == STAR
    assert PLUS | ZERO == STAR
    assert PLUS | QM == STAR
    assert PLUS | ONE == PLUS
    assert PLUS | STAR == STAR
    assert PLUS | PLUS == PLUS
    assert not ZERO.canunion(Multiplier(Bound(2), INF))
    assert not ONE.canunion(Multiplier(Bound(3), Bound(4)))
    assert not Multiplier(
        Bound(8),
        INF
    ).canunion(
        Multiplier(
            Bound(3),
            Bound(4)
        )
    )
    try:
        ZERO | Multiplier(Bound(7), Bound(8))
        assert False
    except AssertionError:
        assert False
    except Exception:
        pass
