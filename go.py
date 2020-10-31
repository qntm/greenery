from greenery import fsm, lego

machine = fsm.fsm(
    alphabet = { 'd' },
    states = { 0, 1 },
    initial = 0,
    finals = {1},
    map = {
        0: {'d': 1},
    },
)

print(machine)
rex = lego.from_fsm(machine)
print(rex)
