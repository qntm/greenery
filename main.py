# This code is in the public domain.

# http://qntm.org/greenery

from __future__ import print_function

# Allow import of package from executable within its directory
import sys, os
sys.path.insert( 0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from greenery import parse

regexes = sys.argv[1:]

if len(regexes) < 2:
	print("Please supply several regexes to compute their intersection, union and concatenation.")
	print("E.g. \"19.*\" \"\\d{4}-\\d{2}-\\d{2}\"")

else:
	p = parse(regexes[0])
	for regex in regexes[1:]:
		p &= parse(regex)
	print("Intersection:  %s" % ( p ))

	p = parse(regexes[0])
	for regex in regexes[1:]:
		p |= parse(regex)
	print("Union:         %s" % ( p ))

	p = parse(regexes[0])
	for regex in regexes[1:]:
		p += parse(regex)
	print("Concatenation: %s" % ( p ))

