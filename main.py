from greenery import Bound, INF, Multiplier, PLUS, QM, STAR, parse

pattern = parse("a")
print(pattern)  # "a"

pattern = pattern * PLUS * QM * STAR * Multiplier(Bound(3), INF)
print(pattern)  # "((((a)+)?)*){3,}"

pattern = pattern.reduce()
print(pattern)  # "a*"
