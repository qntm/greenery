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

# http://qntm.org/greenery

import sys
from lego import pattern, NoRegexException

def doit(*strings):

	p = pattern.parse(".*")
	for s in strings:
		p &= pattern.parse(s)

	return p.regex()

# AND DO IT
strings = sys.argv[1:]

if len(strings) > 0:
	print(doit(*strings))

# no strings supplied? run unit tests
else:
	try:
		doit("a", "b")
		assert(False)
	except NoRegexException:
		pass
	assert doit("a.b") == "a.b" # not "a[ab]b"
	assert doit("a*", "b*") == ""
	assert doit("\\d{4}") == "\\d{4}"
	assert doit("\\d", ".") == "\\d"
	assert doit("\\d{2}", "0.") == "0\\d"
	assert doit("\\d{2}", "19.*") == "19"
	assert doit("\\d{3}", "19.*") == "19\\d"
	assert doit("abc...", "...def") == "abcdef"
	assert doit("[ab]*a?b*|[ab]*b?a*") == "[ab]*"
	assert doit("[bc]*[ab]*", "[ab]*[bc]*") == "([ab]*a|[bc]*c)?b*"
	assert doit("\\W*", "[a-g0-8$%\\^]+", "[^d]{2,8}") == "[$%\\^]{2,8}"
	assert doit("\\d{4}-\\d{2}-\\d{2}", "19.*") == "19\\d\\d-\\d\\d-\\d\\d"
	assert doit("[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]") == "[0-9A-Fa-f]{5}"
	assert doit(
		"(aa|bb*aa)a*|((ab|bb*ab)|(aa|bb*aa)a*b)((ab|bb*ab)|(aa|bb*aa)a*b)*" + \
		"(aa|bb*aa)a*|((ab|bb*ab)|(aa|bb*aa)a*b)((ab|bb*ab)|(aa|bb*aa)a*b)*"
	) == "[ab]*a[ab]"
	print("OK")
