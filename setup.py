from setuptools import setup

setup(
    name="greenery",
    version="4.0.0",
    tests_require=["pytest"],
    packages=["greenery"],
    package_dir={
        "greenery": "greenery"
    },
    author="qntm",
    author_email="qntm <qntm@users.noreply.github.com>",
    description="Greenery allows manipulation of regular expressions",
    license="MIT License",
    keywords=" ".join([
        "re", "regex", "regexp", "regular", "expression", "deterministic",
        "finite", "state", "machine", "automaton", "fsm", "dfsm", "fsa",
        "dfsa", "greenery",
    ]),
    url="https://github.com/qntm/greenery",
    classifiers=[
        "License :: OSI Approved :: MIT License",
        "Programming Language :: Python :: 3.8",
    ],
)
