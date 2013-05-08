from distutils.core import setup

from _version import __version__

setup(
    name = "greenery",
    version = __version__,
    packages = [''],
    package_dir = {'': '.'},
    extra_path = 'greenery',
    author = "Sam Hughes",
    description = "Greenery allows manipulation of Regular Expressions as Finite State Machines",
    license = "MIT License",
    keywords = "greenery regex fsm",
    url = "https://github.com/ferno/greenery",
    classifiers = [
        "License :: OSI Approved :: MIT License",
        "Programming Language :: Python :: 2.7",
        "Programming Language :: Python :: 3.3",
    ],
)
