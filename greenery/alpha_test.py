from .alphabet import Alphabet, ANYTHING_ELSE


def test_distinct():
    alpha = Alphabet.distinct({'a', 'b', 'c', ANYTHING_ELSE})
    assert len(alpha) == 4
    assert len(alpha.events) == 4
    seen = set()
    for letter in {'a', 'b', 'c', ANYTHING_ELSE}:
        assert alpha[letter] not in seen
        seen.add(alpha[letter])


def test_groupings():
    groups = tuple(map(tuple, ("abc", "def", "ghi", ("j", ANYTHING_ELSE))))
    alpha = Alphabet.groups(*groups)
    assert len(alpha) == (3 + 3 + 3 + 2)
    assert len(alpha.events) == 4
    for event, symbols in alpha.events.items():
        assert tuple(sorted(symbols)) in groups
        for sym in symbols:
            assert alpha[sym] == event


def test_union():
    a = Alphabet.groups("abc", "def")
    b = Alphabet.groups("ab", "cd", "ef")
    u, new_to_olds = a.union(b)
    assert len(u.events) == 4
    assert len(u) == 6
    for group in ("ab", "c", "d", "ef"):
        expected = u[group[0]]
        for sym in group:
            assert expected == u[sym]
            for old, new_to_old in zip((a, b), new_to_olds):
                assert new_to_old[u[sym]] == old[sym]


def test_union_incomplete():
    a = Alphabet.groups("abc", "d")
    b = Alphabet.groups("cd", "ef")
    u, new_to_olds = a.union(b)
    assert len(u.events) == 4
    assert len(u) == 6
    for group in ("ab", "c", "d", "ef"):
        expected = u[group[0]]
        for sym in group:
            assert expected == u[sym]
            for old, new_to_old in zip((a, b), new_to_olds):
                if sym in old:
                    assert new_to_old[u[sym]] == old[sym]


def test_union_anything_else():
    a = Alphabet.groups("abc", "d", (ANYTHING_ELSE,))
    b = Alphabet.groups("cd", "ef")
    u, new_to_olds = a.union(b)
    assert len(u.events) == 5
    assert len(u) == 7
    for group in ("ab", "c", "d", "ef", (ANYTHING_ELSE,), '?'):
        expected = u[group[0]]
        for sym in group:
            assert expected == u[sym]
            for old, new_to_old in zip((a, b), new_to_olds):
                if sym in old:
                    assert new_to_old[u[sym]] == old[sym]


def test_union_anything_else_2():
    a = Alphabet.groups("abc", "d", (ANYTHING_ELSE,))
    b = Alphabet.groups("cd", "ef", (ANYTHING_ELSE,))
    u, new_to_olds = a.union(b)
    assert len(u.events) == 5
    assert len(u) == 7
    for group in ("ab", "c", "d", "ef", (ANYTHING_ELSE,), '?'):
        expected = u[group[0]]
        for sym in group:
            assert expected == u[sym]
            for old, new_to_old in zip((a, b), new_to_olds):
                if sym in old:
                    assert new_to_old[u[sym]] == old[sym]
