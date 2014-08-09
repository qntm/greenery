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
    ...     states   = {0, 1, 2},
    ...     initial  = 0,
    ...     finals   = {1},
    ...     map      = {
    ...             0 : {"a" : 1, "b" : 2},
    ...             1 : {"a" : 2, "b" : 2},
    ...             2 : {"a" : 2, "b" : 2},
    ...     },
    ... )
    >>> print(a)
      name final? a b
    ------------------
    * 0    False  1 2
      1    True   2 2
      2    False  2 2
    >>> a.accepts("")
    False
    >>> a.accepts("a")
    True
    >>> a.accepts("b")
    False
    >>> print(a.accepts("c"))
    Traceback (most recent call last):
      File "<stdin>", line 1, in <module>
      File "fsm.py", line 68, in accepts
        state = self.map[state][symbol]
    KeyError: 'c'
    >>> print(repr(a.lego()))
    charclass("a")
    >>> print(a.lego())
    a










### Functions in this module

#### `fsm(alphabet, states, initial, finals, map)`

Constructor for an `fsm` object, as demonstrated above. `fsm` objects are intended to be immutable.

#### `crawl(alphabet, initial, final, follow)`

Crawl what is assumed to be an FSM and return a new `fsm` object representing it. Starts at state `initial`. At any given state, `crawl` calls `final(state)` to determine whether it is final. Then, for each symbol in `alphabet`, it calls `follow(state, symbol)` to try to discover new states. Obviously this procedure could go on for ever if your implementation of `follow` is faulty.

#### `null(alphabet)`

Returns an FSM over the supplied alphabet which accepts no strings at all.

#### `epsilon(alphabet)`

Returns an FSM over the supplied alphabet which accepts only the empty string, `""`.

### Methods on class `fsm`

#### `fsm.accepts(string)`

Returns `True` or `False`, or throws an exception if the string contains a character which is not in `fsm`'s alphabet.

#### `fsm.strings()`

Returns a generator of all the strings that this FSM accepts.

#### `fsm.__str__()`

Means you can pretty-print an FSM using `print(fsm1)`.

#### `fsm.__add__()` (e.g. `fsm3 = fsm1 + fsm2`)

Returns the concatenation of two FSMs. If the first FSM accepts all strings in *A* and the second FSM accepts all strings in *B*, then the resulting FSM will accept all strings of the form *a·b* where *a* is in *A* and *b* is in *B*.

#### `fsm.__and__()` (e.g. `fsm3 = fsm1 & fsm2`)

Returns an FSM accepting any string which is in both *A* and *B*. This is called *intersection*.

#### `fsm.__or__()` (e.g. `fsm3 = fsm1 | fsm2`)

Returns an FSM accepting any string which is in *A* or in *B* or both. This is called *alternation*.

#### `fsm.star()`

Returns a new FSM which is the *[Kleene star closure](https://en.wikipedia.org/wiki/Kleene_star)* of the original. For example, if `fsm` accepts only `"asdf"`, `fsm.star()` accepts `""`, `"asdf"`, `"asdfasdf"`, `"asdfasdfasdf"`, and so on.

#### `fsm.__mul__()` (e.g. `fsm2 = fsm1 * 7`)

Essentially, this is repeated self-concatenation. In this example, if `fsm1` only accepts `"z"`, `fsm2` only accepts `"zzzzzzz"`.

#### `fsm.__reversed__()` (e.g. `fsm2 = reversed(fsm1)`)

Returns a reversed FSM. For each string that `fsm1` accepted, `fsm2` will accept the reversed string. `reversed(reversed(x))` accepts the same strings as `x` for all `fsm` objects `x`, but is not necessarily mechanically identical.

#### `fsm.everythingbut()`

Returns an FSM which accepts every string not accepted by the original. `x.everythingbut().everythingbut()` accepts the same strings as `x` for all `fsm` objects `x`, but is not necessarily mechanically identical.

#### `fsm.lego()`

Uses the Brzozowski algebraic method to convert an `fsm` into a `lego` object, which is a regular expression (see `greenery.lego`, below).

**Caution.** Every finite state machine has an alphabet associated with it. Typically this alphabet will be a set of characters, such as `{"a", "b", "c"}`. If you have an FSM over this alphabet which is capable of recognising "any string", then upon conversion into a regex you might expect an expression like `.*`, with `.` standing for "any character". Instead, you will get an expression like `[abc]*`.

To solve this, add `greenery.lego.otherchars` to your alphabet. This character is taken to stand for "every possible character not explicitly mentioned in the rest of the alphabet". Thus, an FSM over the alphabet `{"a", "b", "c", otherchars}`, which can recognise `"a"` or `"b"` or `"c"`, will be turned into the regular expression `[abc]`, whereas an FSM over that same alphabet which can recognise `"a"` or `"b"` or `"c"` or `lego.otherchars` will be turned into `.` as you would expect.

#### `fsm.reduce()`

Returns an FSM accepting the same strings as the original, but with a minimal number of states.








## greenery.lego

This module provides methods for parsing a regular expression (i.e. a string) into a manipulable nested data structure, and for manipulating that data structure.

Note that this is an entirely different concept from that of simply creating and using those regexes, functionality which is present in basically every programming language in the world, [Python included](http://docs.python.org/library/re.html).

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
* `otherchars`, which you can use in an `fsm`'s alphabet (see `fsm.lego()`).

### Methods in this module

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

* The greedy operators `*?`, `+?`, `??` and `{m,n}?` are not supported, since they do not alter the regular language

* Parentheses are used to alternate between multiple possibilities e.g. `(a|bc)` only, not for capture grouping. Here's why:

        >>> print(parse("(ab)c") & parse("a(bc)"))
        abc

* `(?...)` constructs are not supported (and most are not [regular in the computer science sense](http://en.wikipedia.org/wiki/Regular_language)).

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

#### `lego.fsm()`

Returns an `fsm` object, a finite state machine which recognises exactly the strings that the original regular expression can match.














### Regular expression reduction

`lego` objects are automatically simplified at creation time, using the various implementations  of `lego.reduce()`, which can recognise and simplify the following patterns:

* `(ab|cd|ef|)g` to `(ab|cd|ef)?g`
* `([ab])*` to `[ab]*`
* `ab?b?c` to `ab{0,2}c`
* `a(d(ab|a*c))` to `ad(ab|a*c)`
* `0|[2-9]` to `[02-9]`
* `abc|ade` to `a(bc|de)`
* `xyz|stz` to `(xy|st)z`

The various `reduce()` methods are extensible.

### Name

I spent a long time trying to find an appropriate metaphor for what I was trying to do: "I need an X such that lots of Xs go together to make a Y, but lots of Ys go together to make an X". Unfortunately the real world doesn't seem to be recursive in this way so I plumped for "lego" as a basic catchall term for the various components that go together to make up a data structure.









## Backward compatibility

`greenery.fsm` and `greenery.lego` are separated in this version, which is 2.x. In the 1.x versions, executing:

    import greenery
    regex = greenery.parse( "a*b" )

would import all of `greenery.fsm.*` and `greenery.lego.*` into the `greenery` namespace. To restore version 1.0 behavior for legacy code, use the `greenery.v1` compatibility interface:

    import greenery.v1 as greenery
    regex = greenery.parse( "a*b" )

To make version 1.x style code work with either version 1.x or version 2.x, use:

    try:
        import greenery.v1 as greenery
    except ImportError:
        import greenery
    
    # V1 code
