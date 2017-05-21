from setuptools import setup

from greenery import __version__

setup(
	name = "greenery",
	version = __version__,
	tests_require = [ "pytest" ],
	packages = [ "greenery" ],
	package_dir = { "greenery": "greenery" },
	author = "qntm",
	author_email = "qntm <qntm@users.noreply.github.com>",
	description = "Greenery allows manipulation of regular expressions as finite state machines",
	license = "MIT License",
	keywords = "re regex regexp regular expression deterministic finite state machine automaton fsm dfsm fsa dfsa greenery",
	url = "https://github.com/qntm/greenery",
	classifiers = [
		"License :: OSI Approved :: MIT License",
		"Programming Language :: Python :: 3.3",
	],
)
