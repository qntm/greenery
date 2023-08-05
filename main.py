from __future__ import annotations

from greenery import INF, PLUS, QM, STAR, Bound, Multiplier, parse

print(repr(parse("[\t\n\r -\uD7FF\uE000-\uFFFD]*")))

pattern = parse("a")
print(pattern)  # "a"

pattern = pattern * PLUS * QM * STAR * Multiplier(Bound(3), INF)
print(pattern)  # "((((a)+)?)*){3,}"

pattern = pattern.reduce()
print(pattern)  # "a*"
