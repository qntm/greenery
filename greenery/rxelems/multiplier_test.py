# -*- coding: utf-8 -*-

from .bound import bound, inf
from .multiplier import multiplier, zero, one, qm, star, plus

def test_multiplier_str():
    assert str(multiplier(bound(2), inf)) == "{2,}"

def test_bound_qm():
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
