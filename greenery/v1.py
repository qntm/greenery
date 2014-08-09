# -*- coding: utf-8 -*-

'''
greenery.v1	-- backwards compatibility shim for greenery version 2

The greenery version 1.x interface:
    import greenery

To retain greenery version 1.x interface under greenery version 2.x:
    from greenery import v1 as greenery
or
    import greenery.v1 as greenery
'''
from greenery._version import __version__
from greenery.fsm      import *                 # fsm
from greenery.lego     import *                 # parse, lego, ...
