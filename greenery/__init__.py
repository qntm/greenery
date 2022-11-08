# -*- coding: utf-8 -*-

# In theory this should just pull in the `parse` function, but it turns out
# that the act of performing any relative import suddenly makes EVERY MODULE
# AVAILABLE IN SCOPE?
from .parse import parse
from .bound import Bound, INF
from .multiplier import Multiplier, QM, STAR, PLUS

# WHY ARE THESE MODULE OBJECTS IN SCOPE SUDDENLY? I DIDN'T ASK FOR THEM
# print(bound, fsm, charclass, rxelems)

# This, however, is the *function*, *not* its containing module object?
# print(parse)
