# -*- coding: utf-8 -*-

if __name__ == "__main__":
	import os
	import sys
	# If you run tests in-place (instead of using py.test), ensure local version is tested!
	sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import greenery.v1 as greenery

def test_v1():
	regex = greenery.parse( "a*b" )
	assert isinstance( regex, greenery.lego )
	machine = regex.fsm()
	assert isinstance( machine, greenery.fsm )
	regexstr = str( regex )
	assert regexstr == "a*b"
	assert machine.accepts( "aaab" )
	gen = regex.strings()
	assert next(gen) == "b"
	assert next(gen) == "ab"

	# ensure that the full lego interface has been imported under v1
	astarb = greenery.conc(greenery.mult(greenery.charclass("a"), greenery.star),
			       greenery.mult(greenery.charclass("b"), greenery.one))
	gen = astarb.strings()
	assert next(gen) == "b"
	assert next(gen) == "ab"

if __name__ == "__main__":
	test_v1()
