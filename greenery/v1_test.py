# -*- coding: utf-8 -*-
# Copyright (C) 2012 by Sam Hughes

# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

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
