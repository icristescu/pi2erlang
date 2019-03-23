# pi2erlang
A translator from a fragment of (typed) pi-calculus into erlang processes.


# How to run it
start erlang

load the file produced by the compilation:

1> c(trad).
trad.erl:2: Warning: export_all flag enabled - all functions will be exported
{ok,trad}

load the module chan:

2> c(chan).
chan.erl:2: Warning: export_all flag enabled - all functions will be exported
{ok,chan}

start the system

3> trad:start().
{input,p0,{trad,f10}}