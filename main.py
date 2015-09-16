import sys
from greenery.lego import parse

regexes = sys.argv[1:]

if len(regexes) < 2:
	print("Please supply several regexes to compute their intersection, union and concatenation.")
	print("E.g. \"19.*\" \"\\d{4}-\\d{2}-\\d{2}\"")

else:
	p = parse(regexes[0])
	for regex in regexes[1:]:
		p &= parse(regex)
	print("Intersection:  %s" % ( p.reduce() ))

	p = parse(regexes[0])
	for regex in regexes[1:]:
		p |= parse(regex)
	print("Union:         %s" % ( p.reduce() ))

	p = parse(regexes[0])
	for regex in regexes[1:]:
		p += parse(regex)
	print("Concatenation: %s" % ( p.reduce() ))

