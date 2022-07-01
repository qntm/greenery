# greenery

Tools for parsing and manipulating regular expressions (`greenery.lego`), for producing finite-state machines (`greenery.fsm`), and for freely converting between the two. Python 3 only.

This project was undertaken because I wanted to be able to **compute the intersection between two regular expressions**. The "intersection" is the set of strings which both regexes will accept, represented as a third regular expression.

## Installation

```sh
pip install greenery
```

## Example

    >>> from greenery.lego import parse
    >>> print(parse("abc...") & parse("...def"))
    abcdef
    >>> print(parse("\d{4}-\d{2}-\d{2}") & parse("19.*"))
    19\d{2}-\d{2}-\d{2}
    >>> print(parse("\W*") & parse("[a-g0-8$%\^]+") & parse("[^d]{2,8}"))
    [$%\^]{2,8}
    >>> print(parse("[bc]*[ab]*") & parse("[ab]*[bc]*"))
    ([ab]*a|[bc]*c)?b*
    >>> print(parse("a*") & parse("b*"))

    >>> print(parse("a") & parse("b"))
    []

In the penultimate example, the empty string is returned, because only the empty string is in both of the regular languages `a*` and `b*`. In the final example, an empty character class has been returned. An empty character class can never match anything, which means that this is the smallest representation of a regular expression which matches no strings at all. (Note that this is different from only matching the empty string.)

`greenery` works by converting both regexes to finite state machines, computing the intersection of the two FSMs as a third FSM, and converting the third FSM back to a regex.

As such, `greenery` is divided into two libraries:

## greenery.fsm

This module provides for the creation and manipulation of **deterministic** finite state machines.

### Example

*To do: a slightly more impressive example.*

    >>> from greenery import fsm
    >>> a = fsm.fsm(
    ...     alphabet = {"a", "b"},
    ...     states   = {0, 1},
    ...     initial  = 0,
    ...     finals   = {1},
    ...     map      = {
    ...             0 : {"a" : 1},
    ...     },
    ... )
    >>> print(a)
      name final? a b
    ------------------
    * 0    False  1
      1    True
    >>> a.accepts([])
    False
    >>> a.accepts(["a"])
    True
    >>> a.accepts(["b"])
    False
    >>> print(a.accepts(["c"]))
    Traceback (most recent call last):
      File "<stdin>", line 1, in <module>
      File "fsm.py", line 68, in accepts
        state = self.map[state][symbol]
    KeyError: 'c'

### Functions in this module

#### `fsm(alphabet, states, initial, finals, map)`

Constructor for an `fsm` object, as demonstrated above. `fsm` objects are intended to be immutable.

`map` may be sparse. If a transition is missing from `map`, then it is assumed that this transition leads to an undocumented "oblivion state" which is not final. This oblivion state does not appear when the FSM is printed out.

Ordinarily, you may only feed known alphabet symbols into the FSM. Any other symbol will result in an exception, as seen above. However, if you add the special symbol `fsm.anything_else` to your alphabet, then any unrecognised symbol will be automatically converted into `fsm.anything_else` before following whatever transition you have specified for this symbol.

#### `crawl(alphabet, initial, final, follow)`

Crawl what is assumed to be an FSM and return a new `fsm` object representing it. Starts at state `initial`. At any given state, `crawl` calls `final(state)` to determine whether it is final. Then, for each symbol in `alphabet`, it calls `follow(state, symbol)` to try to discover new states. Obviously this procedure could go on for ever if your implementation of `follow` is faulty. `follow` may also throw an `OblivionError` to indicate that you have reached an inescapable, non-final "oblivion state"; in this case, the transition will be omitted from the resulting FSM.

#### `null(alphabet)`

Returns an FSM over the supplied alphabet which accepts no strings at all.

#### `epsilon(alphabet)`

Returns an FSM over the supplied alphabet which accepts only the empty string, `""`.

### Methods on class `fsm`

An FSM accepts a possibly-infinite set of strings. With this in mind, `fsm` implements numerous [methods like those on `frozenset`](https://docs.python.org/3.5/library/stdtypes.html#frozenset), as well as many FSM-specific methods. FSMs are immutable.

Method | Behaviour
---|---
`fsm1.accepts("a")` <br/> `"a" in fsm1` | Returns `True` or `False` or throws an exception if the string contains a symbol which is not in the FSM's alphabet. The string should be an iterable of symbols.
`fsm1.strings()` <br/> `for string in fsm1` | Returns a generator of all the strings that this FSM accepts.
`fsm1.empty()` | Returns `True` if this FSM accepts no strings, otherwise `False`.
`fsm1.cardinality()` <br/> `len(fsm1)` | Returns the number of strings which the FSM accepts. Throws an `OverflowError` if this number is infinite.
`fsm1.equivalent(fsm2)` <br/> `fsm1 == fsm2` | Returns `True` if the two FSMs accept exactly the same strings, otherwise `False`.
`fsm1.different(fsm2)` <br/> `fsm1 != fsm2` | Returns `True` if the FSMs accept different strings, otherwise `False`.
`fsm1.issubset(fsm2)` <br/> `fsm1 <= fsm2` | Returns `True` if the set of strings accepted by `fsm1` is a subset of those accepted by `fsm2`, otherwise `False`.
`fsm1.ispropersubset(fsm2)` <br/> `fsm1 < fsm2` | Returns `True` if the set of strings accepted by `fsm1` is a proper subset of those accepted by `fsm2`, otherwise `False`.
`fsm1.issuperset(fsm2)` <br/> `fsm1 >= fsm2` | Returns `True` if the set of strings accepted by `fsm1` is a superset of those accepted by `fsm2`, otherwise `False`.
`fsm1.ispropersuperset(fsm2)` <br/> `fsm1 > fsm2` | Returns `True` if the set of strings accepted by `fsm1` is a proper superset of those accepted by `fsm2`, otherwise `False`.
`fsm1.isdisjoint(fsm2)` | Returns `True` if the set of strings accepted by `fsm1` is disjoint from those accepted by `fsm2`, otherwise `False`.
`fsm1.copy()` | Returns a copy of `fsm1`.
`fsm1.reduce()` | Returns an FSM which accepts exactly the same strings as `fsm1` but has a minimal number of states.
`fsm1.star()` | Returns a new FSM which is the *[Kleene star closure](https://en.wikipedia.org/wiki/Kleene_star)* of the original. For example, if `fsm1` accepts only `"asdf"`, `fsm1.star()` accepts `""`, `"asdf"`, `"asdfasdf"`, `"asdfasdfasdf"`, and so on.
`fsm1.everythingbut()` | Returns an FSM which accepts every string not accepted by the original. `x.everythingbut().everythingbut()` accepts the same strings as `x` for all `fsm` objects `x`, but is not necessarily mechanically identical.
`fsm1.reversed()` <br/> `reversed(fsm1)` | Returns a reversed FSM. For each string that `fsm1` accepted, `reversed(fsm1)` will accept the reversed string. `reversed(reversed(x))` accepts the same strings as `x` for all `fsm` objects `x`, but is not necessarily mechanically identical.
`fsm1.times(7)` <br/> `fsm1 * 7` | Essentially, this is repeated self-concatenation. If `fsm1` only accepts `"z"`, `fsm2` only accepts `"zzzzzzz"`.
`fsm1.concatenate(fsm2, ...)` <br/> `fsm1 + fsm2 + ...` | Returns the concatenation of the FSMs. If `fsm1` accepts all strings in *A* and `fsm2` accepts all strings in *B*, then `fsm1 + fsm2` accepts all strings of the form *a·b* where *a* is in *A* and *b* is in *B*.
`fsm1.union(fsm2, ...)` <br/> `fsm1 \| fsm2 \| ...` | Returns an FSM accepting any string accepted by any input FSM. This is also called *alternation*.
`fsm1.intersection(fsm2, ...)` <br/> `fsm1 & fsm2 & ...` | Returns an FSM accepting any string accepted by all input FSMs.
`fsm1.difference(fsm2, ...)` <br/> `fsm1 - fsm2 - ...` | Subtract the set of strings accepted by `fsm2` onwards from those accepted by `fsm1` and return the resulting new FSM.
`fsm1.symmetric_difference(fsm2, ...)` <br/> `fsm1 ^ fsm2 ^ ...` | Returns an FSM accepting any string accepted by `fsm1` or `fsm2` but not both.
`fsm1.derive("a")` | Return the [Brzozowski derivative](https://en.wikipedia.org/wiki/Brzozowski_derivative) of the original FSM with respect to the input string. E.g. if `fsm1` only accepts `"ab"` or `"ac+"`, returns an FSM only accepting `"b"` or `"c+"`.

## greenery.lego

This module provides methods for parsing a regular expression (i.e. a string) into a manipulable nested data structure, and for manipulating that data structure.

Note that this is an entirely different concept from that of simply creating and using those regexes, functionality which is present in basically every programming language in the world, [Python included](http://docs.python.org/library/re.html).

This module requires `greenery.fsm` in order to carry out many of its most important functions. (`greenery.fsm`, in comparison, is completely standalone.)

### Classes in this module

#### `lego.bound`

A non-negative integer, or `inf`, plus a bunch of arithmetic methods which make it possible to compare, add and multiply them.

##### Special bounds

* `inf`

#### `lego.multiplier`

A combination of a finite lower `bound` and a possibly-infinite upper `bound`, plus a bunch of methods which make it possible to compare, add and multiply them.

##### Special multipliers

* `zero` (`multiplier(bound(0), bound(0)`) (has some occasional uses internally)
* `qm` (`multiplier(bound(0), bound(1))`)
* `star` (`multiplier(bound(0), inf)`)
* `one` (`multiplier(bound(1), bound(1))`)
* `plus` (`multiplier(bound(1), inf)`)

#### `lego.lego`

Parent class for `charclass`, `mult`, `conc` and `pattern`. In general, this represents a regular expression object.

#### `lego.charclass`

Represents a character class, e.g `a`, `[abc]`, `[^xyz]`, `\d`.

##### Special character classes

* `w` (`charclass("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz")`)
* `d` (`charclass("0123456789")`)
* `s` (`charclass("\t\n\v\f\r ")`)
* `W` (any character except those matched by `w`)
* `D` (any character except those matched by `d`)
* `S` (any character except those matched by `s`)
* `dot` (any character)
* `nothing` (empty character class, no matches possible)

#### `lego.mult`

Represents a `charclass` or `pattern` combined with a `multiplier`, e.g. `[abc]*` or `(a|bc)*`.

#### `lego.conc`

Represents a sequence of zero or more `mult`s, e.g. `ab`, `[abc]*d`.

##### Special concatenations

* `emptystring`, the regular expression which only matches the empty string (`conc()`)

#### `lego.pattern`

Represents an alternation between one or more `conc`s, e.g. `[abc]*d|e`.

### Methods in this module

#### `lego.from_fsm()`

Uses the Brzozowski algebraic method to convert a `greenery.fsm` object into a `lego` object, which is a regular expression.

#### `lego.parse(string)`

Returns a `lego` object representing the regular expression in the string.

The following metacharacters and formations have their usual meanings: `.`, `*`, `+`, `?`, `{m}`, `{m,}`, `{m,n}`, `()`, `|`, `[]`, `^` within `[]` character ranges only, `-` within `[]` character ranges only, and `\` to escape any of the preceding characters or itself.

These character escapes are possible: `\t`, `\r`, `\n`, `\f`, `\v`.

These predefined character sets also have their usual meanings: `\w`, `\d`, `\s` and their negations `\W`, `\D`, `\S`. `.` matches any character, including new line characters and carriage returns.

An empty charclass `[]` is legal and matches no characters: when used in a regex, the regex may match no strings.

##### Unsupported constructs

* This method is intentionally rigorously simple, and tolerates no ambiguity. For example, a hyphen must be escaped in a character class even if it appears first or last. `[-abc]` is a syntax error, write `[\-abc]`. Escaping something which doesn't need it is a syntax error too: `[\ab]` resolves to neither `[\\ab]` nor `[ab]`.

* The `^` and `$` metacharacters are not supported. By default, `greenery` assumes that all regexes are anchored at the start and end of any input string. Carets and dollar signs will be parsed as themselves. If you want to *not* anchor at the start or end of the string, put `.*` at the start or end of your regex respectively.

 This is because computing the intersection between `.*a.*` and `.*b.*` (1) is largely pointless and (2) usually results in gibberish coming out of the program.

* The greedy operators `*?`, `+?`, `??` and `{m,n}?` are not supported, since they do not alter the regular language.

* Parentheses are used to alternate between multiple possibilities e.g. `(a|bc)` only, not for capture grouping. Here's why:

        >>> print(parse("(ab)c") & parse("a(bc)"))
        abc

 The `(?:...)` syntax for non-capturing groups is permitted, but does nothing.

* Other `(?...)` constructs are not supported (and most are not [regular in the computer science sense](http://en.wikipedia.org/wiki/Regular_language)).

*  Back-references, such as `([aeiou])\1`, are not regular.

### Methods on the `lego` class

All objects of class `lego` (`charclass`, `mult`, `conc` and `pattern`) share these methods.

Method | Behaviour
---|---
`lego1.to_fsm()` | Returns an `fsm` object, a finite state machine which recognises exactly the strings that the original regular expression can match. The majority of the other methods employ this one.
`lego1.matches("a")` <br/> `"a" in lego1` | Returns `True` if the regular expression matches the string or `False` if not.
`lego1.strings()` <br/> `for string in lego1` | Returns a generator of all the strings that this regular expression matches.
`lego1.empty()` | Returns `True` if this regular expression matches no strings, otherwise `False`.
`lego1.cardinality()` <br/> `len(lego1)` | Returns the number of strings which the regular expression matches. Throws an `OverflowError` if this number is infinite.
`lego1.equivalent(lego2)` | Returns `True` if the two regular expressions match exactly the same strings, otherwise `False`.
`lego1.copy()` | Returns a copy of `lego1`.
`lego1.everythingbut()` | Returns a regular expression which matches every string not matched by the original. `x.everythingbut().everythingbut()` matches the same strings as `x` for all `lego` objects `x`, but is not necessarily identical.
`lego1.reversed()` <br/> `reversed(lego1)` | Returns a reversed regular expression. For each string that `lego1` matched, `reversed(lego1)` will match the reversed string. `reversed(reversed(x))` matches the same strings as `x` for all `lego` objects `x`, but is not necessarily identical.
`lego1.times(star)` <br/> `lego1 * star` | Returns the input regular expression multiplied  by any `multiplier`.
`lego1.concatenate(lego2, ...)` <br/> `lego1 + lego2 + ...` | Returns the concatenation of the regular expressions.
`lego1.union(lego2, ...)` <br/> `lego1 \| lego2 \| ...` | Returns the alternation of the two regular expressions.
`lego1.intersection(lego2, ...)` <br/> `lego1 & lego2 & ...` | Returns a regular expression matching any string matched by all input regular expressions. The successful implementation of this method was the ultimate goal of this entire project.
`lego1.difference(lego2, ...)` <br/> `lego1 - lego2 - ...` | Subtract the set of strings matched by `lego2` onwards from those matched by `lego1` and return the resulting regular expression.
`lego1.symmetric_difference(lego2, ...)` <br/> `lego1 ^ lego2 ^ ...` | Returns a regular expression matching any string accepted by `lego1` or `lego2` but not both.
`lego1.reduce()` | Returns a regular expression which matches exactly the same strings as `lego1` but is simplified as far as possible. See dedicated section below.
`lego1.derive("a")` | Return the [Brzozowski derivative](https://en.wikipedia.org/wiki/Brzozowski_derivative) of the input regular expression with respect to "a".

#### `reduce()`

Call this method to try to simplify the regular expression object, according to the following patterns:

* `(ab|cd|ef|)g` to `(ab|cd|ef)?g`
* `([ab])*` to `[ab]*`
* `ab?b?c` to `ab{0,2}c`
* `a(d(ab|a*c))` to `ad(ab|a*c)`
* `0|[2-9]` to `[02-9]`
* `abc|ade` to `a(bc|de)`
* `xyz|stz` to `(xy|st)z`
* `abc()def` to `abcdef`
* `a{1,2}|a{3,4}` to `a{1,4}`

The various `reduce()` methods are extensible.

Note that in a few cases this did *not* result in a shorter regular expression.

### Name

I spent a long time trying to find an appropriate metaphor for what I was trying to do: "I need an X such that lots of Xs go together to make a Y, but lots of Ys go together to make an X". Unfortunately the real world doesn't seem to be recursive in this way so I plumped for "lego" as a basic catchall term for the various components that go together to make up a data structure.

This was a dumb idea in retrospect and it will be changed to `greenery.re` or `greenery.rx` in the near future. Vote now if you have an opinion.

### Development

* Update the version in `./setup.py`
* Trash `./dist`
* `python -m build` - creates a `./dist` directory with some stuff in it
* `python -m twine pload dist/*`
