import sys
from greenery.lego import lego, parse

regexes = sys.argv[1:]

if len(regexes) < 2:
	print("Please supply several regexes to compute their intersection, union and concatenation.")
	print("E.g. \"19.*\" \"\\d{4}-\\d{2}-\\d{2}\"")

else:
	regexes = [parse(regex) for regex in regexes]
	print("Intersection:  %s" % ( lego.intersection(*regexes).reduce() ))
	print("Union:         %s" % ( lego.union(*regexes).reduce() ))
	print("Concatenation: %s" % ( lego.concatenate(*regexes).reduce() ))

