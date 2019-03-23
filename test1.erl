% translating &a.(a(x)|-a(x))

-module(test1).
-compile(export_all).

start() -> register(p0, spawn(chan, channel, [0, 0])),
(p0 ! {output, x}),
(p0 ! {input, p0, {test1, f10}}) . 
f10() -> done .
 