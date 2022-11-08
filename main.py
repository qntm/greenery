from greenery import Bound, INF, Multiplier, PLUS, QM, STAR, parse

a = parse("a")
print(a)  # "a"

a = a * PLUS * QM * STAR * Multiplier(Bound(3), INF)
print(a)  # "((((a)+)?)*){3,}"

a = a.reduce()
print(a)  # "a*"
