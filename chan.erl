-module(chan).
-compile(export_all).

channel(N, M) ->
   receive
      {output, Msg} ->
       put(N, Msg), channel(N+1, M+1);

      {input, Me, {mod, fn}} ->
            if
	       M == 0 -> %outputs are late
	       	 receive {output, L} ->
		    put(N, L),
		    io:format("~w received ~w~n", [Me,L])
    	 	 end,
	 	 apply(mod, fn, []),
		 channel(N+1, 0);
 	       M > 0 -> %inputs are late
	       	 L = get(N-1),
 		 io:format("~w received ~w~n", [Me,L]),
 		 apply(mod, fn, []),
		 channel(N-1, M-1)
	    end
   end.