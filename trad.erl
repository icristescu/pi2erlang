-module(trad).
-compile(export_all).

start() -> register(p0, spawn(chan, channel, [0, 0])),
(p0 ! {output, x}),
(p0 ! {input, p0, {trad,f10}}) .
f10() -> done .
