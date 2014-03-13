-module(msg). 

-export([new/0, push/2]).
new() ->
    spawn(fun() -> loop({fifo, [], []}) end).

loop(Fifo) ->
    receive 
        {push, Value, PID} ->
            Tmp = fifo:push(Fifo, Value),
            PID ! Value,
            loop(Tmp)
end.

push(Fifo, Value) ->
    Fifo ! {push, Value, self()},
    receive
        %% {fifo, In, Out} ->
        %%     {fifo, In, Out}
        X -> X
                
    end.
		 
    
    
    
    
    
    
    
