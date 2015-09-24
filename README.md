# greenery

Tools for parsing and manipulating regular expressions (`greenery.lego`), for producing finite-state machines (`greenery.fsm`), and for freely converting between the two.

This project was undertaken because I wanted to be able to **compute the intersection between two regular expressions**. The "intersection" is the set of strings which both regexes will accept, represented as a third regular expression.

## Example

    >>> from greenery.lego import parse
    >>> print(parse("abc...") & parse("...def"))
    abcdef
    >>> print(parse("\d{4}-\d{2}-\d{2}") & parse("19.*"))
    19\d\d-\d\d-\d\d
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

Crawl what is assumed to be an FSM and return a new `fsm` object representing it. Starts at state `initial`. At any given state, `crawl` calls `final(state)` to determine whether it is final. Then, for each symbol in `alphabet`, it calls `follow(state, symbol)` to try to discover new states. Obviously this procedure could go on for ever if your implementation of `follow` is faulty.

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
`fsm1.cardinality()` <br/> `len(fsm1)` | Returns the number of strings which the FSM accepts. Throws a `ValueError` if this number is infinite.
`fsm1.equivalent(fsm2)` <br/> `fsm1 == fsm2` | Returns `True` if the two FSMs accept exactly the same strings, otherwise `False`.
`fsm1.different(fsm2)` <br/> `fsm1 != fsm2` | Returns `True` if the FSMs accept different strings, otherwise `False`.
`fsm1.issubset(fsm2)` <br/> `fsm1 <= fsm2` | Returns `True` if the set of strings accepted by `fsm1` is a subset of those accepted by `fsm2`, otherwise `False`.
`fsm1.ispropersubset(fsm2)` <br/> `fsm1 < fsm2` | Returns `True` if the set of strings accepted by `fsm1` is a proper subset of those accepted by `fsm2`, otherwise `False`.
`fsm1.issuperset(fsm2)` <br/> `fsm1 >= fsm2` | Returns `True` if the set of strings accepted by `fsm1` is a superset of those accepted by `fsm2`, otherwise `False`.
`fsm1.ispropersuperset(fsm2)` <br/> `fsm1 > fsm2` | Returns `True` if the set of strings accepted by `fsm1` is a proper superset of those accepted by `fsm2`, otherwise `False.
`fsm1.isdisjoint(fsm2)` | Returns `True` if the set of strings accepted by `fsm1` is disjoint from those accepted by `fsm2`, otherwise `False`.
`fsm1.copy()` | Returns a copy of `fsm1`.
`fsm1.reduce()` | Returns an FSM which accepts exactly the same strings as `fsm1` but has a minimal number of states.
`fsm1.star()` | Returns a new FSM which is the *[Kleene star closure](https://en.wikipedia.org/wiki/Kleene_star)* of the original. For example, if `fsm1` accepts only `"asdf"`, `fsm1.star()` accepts `""`, `"asdf"`, `"asdfasdf"`, `"asdfasdfasdf"`, and so on.
`fsm1.everythingbut()` | Returns an FSM which accepts every string not accepted by the original. `x.everythingbut().everythingbut()` accepts the same strings as `x` for all `fsm` objects `x`, but is not necessarily mechanically identical.
`fsm1.reversed()` <br/> `reversed(fsm1)` | Returns a reversed FSM. For each string that `fsm1` accepted, `reversed(fsm1)` will accept the reversed string. `reversed(reversed(x))` accepts the same strings as `x` for all `fsm` objects `x`, but is not necessarily mechanically identical.
`fsm1.times(7)` <br/> `fsm1 * 7` | Essentially, this is repeated self-concatenation. If `fsm1` only accepts `"z"`, `fsm2` only accepts `"zzzzzzz"`.
`fsm1.concatenate(fsm2, ...)` <br/> `fsm1 + fsm2 + ...` | Returns the concatenation of the FSMs. If `fsm1` accepts all strings in *A* and `fsm2` accepts all strings in *B*, then `fsm1 + fsm2` accepts all strings of the form *a·b* where *a* is in *A* and *b* is in *B*.
`fsm1.union(fsm2, ...)` <br/> `fsm1 | fsm2 | ...` | Returns an FSM accepting any string accepted by any input FSM. This is also called *alternation*.
`fsm1.intersection(fsm2, ...)` <br/> `fsm1 & fsm2 & ...` | Returns an FSM accepting any string accepted by all input FSMs.
`fsm1.difference(fsm2, ...)` <br/> `fsm1 - fsm2 - ...` | Subtract the set of strings accepted by `fsm2` onwards from those accepted by `fsm1` and return the resulting new FSM.
`fsm1.symmetric_difference(fsm2, ...)` <br/> `fsm1 ^ fsm2 ^ ...` | Returns an FSM accepting any string accepted by `fsm1` or `fsm2` but not both.

## greenery.lego

This module provides methods for parsing a regular expression (i.e. a string) into a manipulable nested data structure, and for manipulating that data structure.

Note that this is an entirely different concept from that of simply creating and using those regexes, functionality which is present in basically every programming language in the world, [Python included](http://docs.python.org/library/re.html).

This module requires `greenery.fsm` in order to carry out many of its most important functions. (`greenery.fsm`, in comparison, is completely standalone.)

### Classes in this module

#### `lego.bound`

A non-negative integer, or `inf`, plus a bunch of arithmetic methods which make it possible to compare, add and multiply them.

#### `lego.multiplier`

A combination of a finite lower `bound` and a possibly-infinite upper `bound`, plus a bunch of methods which make it possible to compare, add and multiply them.

#### `lego.lego`

Parent class for `charclass`, `mult`, `conc` and `pattern`. In general, this represents a regular expression object.

#### `lego.charclass`

Represents a character class, e.g `a`, `[abc]`, `[^xyz]`, `\d`.

#### `lego.mult`

Represents a `charclass` combined with a `multiplier`, e.g. `[abc]*`.

A `mult` may contain a `pattern` instead of a `charclass`, e.g. `(a|bc)*`.

#### `lego.conc`

Represents a sequence of zero or more `mult`s, e.g. `ab`, `[abc]*d`.

#### `lego.pattern`

Represents an alternation between one or more `conc`s, e.g. `[abc]*d|e`.

### Constants in this module

* the `bound` object `inf`
* multiplier `qm` (`multiplier(bound(0), bound(1))`)
* multiplier `star` (`multiplier(bound(0), inf)`)
* multiplier `plus` (`multiplier(bound(1), inf)`)
* the character classes `w`, `W`, `s`, `S`, `d`, `D` and `dot`
* `emptystring`, the regular expression which only matches the empty string (`conc()`)
* `nothing`, a regular expression which matches no strings (`charclass()`)

### Methods in this module

#### `lego.from_fsm()`

Uses the Brzozowski algebraic method to convert a `greenery.fsm` object into a `lego` object, which is a regular expression.

#### `lego.parse(string)`

Returns a `lego` object, representing the regular expression in the string.

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

#### `lego.__add__()` (e.g. `lego3 = lego1 + lego2`)

Return the concatenation of two regular expressions.

#### `lego.__or__()` (e.g. `lego3 = lego1 | lego2`)

Return the alternation of two regular expressions.

#### `lego.__and__()` (e.g. `lego3 = lego1 & lego2`)

Return the intersection of two regular expressions. The successful implementation of this method was the ultimate goal of this entire project.

#### `lego.__mul__(multiplier)`

Return the current regular expression after it has been multiplied by the supplied `multiplier` object. A `multiplier` object has a lower `bound` and an upper `bound`. The upper bound may be `inf`. For example:

    x = parse("abc")
    x = x * multiplier(bound(0), inf)
    print(x) # "(abc)*"

#### `lego.strings()`

Returns a generator of all the strings that this regular expression accepts.

#### `lego.to_fsm()`

Returns an `fsm` object, a finite state machine which recognises exactly the strings that the original regular expression can match.

#### `lego.reduce()`

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

### Name

I spent a long time trying to find an appropriate metaphor for what I was trying to do: "I need an X such that lots of Xs go together to make a Y, but lots of Ys go together to make an X". Unfortunately the real world doesn't seem to be recursive in this way so I plumped for "lego" as a basic catchall term for the various components that go together to make up a data structure.

This was a dumb idea in retrospect and it will be changed to `greenery.re` or `greenery.rx` in the near future. Vote now if you have an opinion.
