from __future__ import annotations

from greenery import INF, PLUS, QM, STAR, Bound, Multiplier, parse

pattern = parse("a")
print(pattern)  # "a"

pattern = pattern * PLUS * QM * STAR * Multiplier(Bound(3), INF)
print(pattern)  # "((((a)+)?)*){3,}"

pattern = pattern.reduce()
print(pattern)  # "a*"
