-module(trad).
-compile(export_all).

channel(N, M) ->
   receive
      {output, Msg} ->
       put(N, Msg), channel(N+1, M+1);

      {input, Me, Msg} ->
            if
	       M == 0 -> %outputs are late
	       	 receive {output, L} ->
		    put(N, L),
		    io:format("~w received ~w~n", [Me,L])
    	 	 end,
	 	 apply(trad, Msg, []),
		 channel(N+1, 0);
 	       M > 0 -> %inputs are late
	       	 L = get(N-1),
 		 io:format("~w received ~w~n", [Me,L]),
 		 apply(trad, Msg, []),
		 channel(N-1, M-1)
	    end
   end.

start() -> register(p0, spawn(trad, channel, [0, 0])),
(p0 ! {output, x}),
(p0 ! {input, p0, f10}) .
f10() -> done .
