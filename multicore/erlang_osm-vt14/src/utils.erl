%% @author Karl Marklund <karl.marklund@it.uu.se>

%% @doc A small collection of utility functions. 


-module(utils). 

-export([seqs/1, filter/2, split/2, add_all/2]).

%% To use EUnit we must include this.
-include_lib("eunit/include/eunit.hrl").

-compile(export_all). 

%% add_all([],[]) ->
%%     [];
%% add_all([HeadA|A],[HeadB|B]) ->
%%     Tmp = add_values(HeadA,HeadB,0),
%%     [Tmp|add_all(A,B)].

add_all([[]],[[]]) ->
    [[]];
add_all(A,B) ->
    My_Pid = self(),
    spawn(fun()->spawn_worker(My_Pid,A,B)end),    
    receive
        {X,0} ->
            X;
        {X,1} ->
            lists:concat([[1],X])
    end.
    
spawn_worker(PID, [A|[]],[B|[]]) ->
    X = add_values((A),(B),0),         
    PID ! X;    

spawn_worker(PID,[HeadA|A],[HeadB|B]) ->
    MyPid = self(),
    spawn(fun()->spawn_worker(MyPid,A,B)end),
    Sum0 = add_values(HeadA,HeadB,0),
    Sum1 = add_values(HeadA,HeadB,1),
    receive
        {X,0} ->
            PID ! {lists:concat([element(1,Sum0),X]),element(2,Sum0)};
        {X,1} ->
            PID ! {lists:concat([element(1,Sum1),X]),element(2,Sum1)}
        end.

list_to_int([]) ->
    0;
list_to_int(L) ->
    Tmp = lists:map(fun(X) -> X+48 end, L),
    list_to_integer(Tmp).

%% add_help(ListA, ListB) ->
%%     Sum = list_to_int(ListA) + list_to_int(ListB),
%%     SumList = intlist(Sum),
%%     [H|T] = intlist(Sum),
%%     if
%%         length([H|T]) > length(ListA) ->
%%             {T,1};
%%         true ->
%%             {SumList, 0}
%%     end.
   
make_same(A,B) ->
    if
        length(A) > length(B) ->
            make_same(A,[0|B]);
            true ->
            B
    end.

add_values(A,B,C) ->
    Tmp=list_to_int(A)+list_to_int(B)+C,
    Len_sum = intlist(Tmp),

    if
        (length(Len_sum)>length(A)) ->
            [_Xx|Tail]=Len_sum,
            list_to_int(Tail),
            %%{Tmp2,1};
            {Tail,1};
        (length(Len_sum)<length(A)) ->
            {make_same(A,Len_sum),0};
        true ->
            {Len_sum,0}
    end.

to_base_10(A,Base) ->
    Tmp = lists:mapfoldr(fun(X,Sum) -> {X*Sum,Sum*Base} end, 1, A),
    Tmp2 = element(1,Tmp),
    Tmp3 = lists:foldl(fun(Y, Sum2) -> Y + Sum2 end, 0, Tmp2),
    intlist(Tmp3).

intlist(A) ->
    Tmp = integer_to_list(A),
    lists:map(fun(X)-> X-48 end,Tmp).
    
fulfill(A,B) ->
    if 
        length(A)>length(B) ->
            fulfill(A, [0|B]);
        length(B)>length(A) ->
            fulfill([0|A] ,B);
        true ->
            {A,B}
    end.

%% split(L, N) when length(L) < N ->
%%     L;
%% split(L, N) ->
%%     Len=length(L),
%%     Q=(Len div N),
%%     R= Len rem N, 
%%     if R == 0 ->
%%             split(L,1,[]);
%%        true ->
%%             split(L, (Q+1),[])
%%     end.
%% split(L, N, Lists) ->
%%     {L1, L2} = lists:split(N, L),
%%     if length(L2) > N ->
%%             split(L2, N, [L1|Lists]);
%%        true ->
%%             lists:reverse([L2, L1|Lists])
%%     end. 

split(L, N) when length(L) < N ->
    L;
split(L, N) when length(L) == N ->
    lists:map(fun(X) -> [X] end, L);
                   
split(L, N) ->
    split(L,N,[]).

split(L, N, Lists) ->
    Len=length(L),
    Kvot=(Len div N),
    
    {L1, L2} = lists:split(Kvot, L),
    if (length(L2) >= N) ->
            split(L2, N-1, [L1|Lists]);
       true ->
            lists:reverse([L1|Lists])
    end. 
%%_______________________________________________________________________________________________________________________________________________________

%% @doc Generates a list of lists of increasing sequences of integers
%% starting with the empty list and ending with [1,2, ..., N].
%% === Example ===
%% <div class="example">```
%% > utils:seqs(5).
%% [[],[1],[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5]]'''
%% </div>
-spec seqs(N::integer()) -> [[integer()]].

seqs(N) ->
    %% NOTE: Simply using a list comprehension such as [[]] ++
    %% [lists:seq(1,M) || M <- lists:seq(1,N)] will be quite slow
    %% since each sequence is generated from scratch. Hence, lets
    %% re-use the last sequnece and add a new element when
    %% constructing the next sequence.
    
    F = fun(X,[H|T]) -> [[X|H],H|T] end,
    lists:foldl(F, [[]], lists:seq(1,N)),
    lists:reverse([lists:reverse(L) || L <- lists:foldl(F, [[]], lists:seq(1,N))]).

		
%% @doc Each list in List2 contains the elements Elem in List1 for
%% which one of the Pred(Elem) returns true. The order of the lists in
%% List2 is the same as the order of the predicates. In each list in
%% List2, the relative order of the elements are the same as in the
%% original List1. 
%% 
%% === Example ===
%% <div class="example">```
%% 1> L = [1,2,3,4,5,6,7,8,9,10].
%% [1,2,3,4,5,6,7,8,9,10]
%% 2> P1 = fun(X) -> X rem 2 == 1 end.
%% #Fun<erl_eval.6.111823515>  
%% 3> P2 = fun(X) -> not P1(X) end. 
%% #Fun<erl_eval.6.111823515>
%% 4> P3 = fun(X) -> X > 3 andalso X < 7 end. 
%% #Fun<erl_eval.6.111823515>
%% 5> utils:filter([P1,P2,P3], L).
%% [[1,3,5,7,9],[2,4,6,8,10],[4,5,6]]'''
%% </div>
-spec filter(Preds, List1) -> List2 when
      Preds :: [Pred],
      Pred :: fun((Elem :: T) -> boolean()),
      List1 :: [T],
      List2 :: [[T]],
      T :: term().

filter(Predicates, List) ->
    Collect = self(),
    [spawn(fun() -> Collect!{I,lists:filter(P,List)} end) ||
	{I, P} <- lists:zip(lists:seq(1, length(Predicates)), Predicates)],
    
    filter_collect(length(Predicates), []).

filter_collect(0,R) ->
    [L || {_,L} <- lists:sort(R)];
filter_collect(N,R) ->
    receive
	{I, L} -> filter_collect(N-1, [{I,L}|R])
    end.

lqr(L, N) ->
    Len = length(L),

    %% Quotient
    Q = Len div N, 
    
    %% Reminder
    R = Len rem N, 
    
    {Len, Q, R}. 

%% @doc Split List into N Lists such that all Lists have approximately the same number of elements. 
%% 
%% Let Len = length(List), Q = Len div N and R = Len rem N. 
%% 
%% If R = 0, then all of the lists in Lists will be of length Q. 
%% 
%% If R =/= 0, then R of the lists in Lists will have
%% lenght Q + 1. 
%% 
%% === Example ===
%% 
%% <div class="example">```
%% 1> L = [1,2,3,4,5,6,7,8,9,10].
%% [1,2,3,4,5,6,7,8,9,10]
%% 2> utils:split(L, 4).
%% [[1,2],[3,4],[5,6,7],[8,9,10]]
%% 3> lists:concat(utils:split(L,3)).
%% [1,2,3,4,5,6,7,8,9,10]'''
%% </div>

%% -spec split(List, N) -> Lists when
%%       List :: [T],
%%       Lists :: [List],
%%       T :: term(),
%%       N :: integer().


%% split(L, N) ->
%%     tbi.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                          %%
%%			   EUnit Test Cases                                 %%
%%                                                                          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

seqs_length_test_() ->
    %% The list [[], [1], [1,2], ..., [1,2, ..., N]] will allways have
    %% length N+1.

    [?_assertEqual(N+1, length(seqs(N))) || N <- lists:seq(1, 55)].

seqs_test_() ->
    %% A small collection of expected results {N, seqs(N)}.
    
    Data = [{0, [[]]}, {1, [[], [1]]}, {2, [[], [1], [1,2]]}, 
	    {7, [[],
		 [1],
		 [1,2],
		 [1,2,3],
		 [1,2,3,4],
		 [1,2,3,4,5],
		 [1,2,3,4,5,6],
		 [1,2,3,4,5,6,7]]}
	   ],
    
    [?_assertEqual(L, seqs(N)) || {N, L} <- Data].
    
filter_test_() ->
    [?_assertEqual([], filter([], L)) || L <- seqs(10)].
    
filter_true_false_test_() ->
    P1 = fun(_) -> false end,
    P2 = fun(_) -> true end,
    P3 = fun(X) -> X rem 2 == 0 end,
    
    Expected = fun(L) -> [lists:filter(P,L) || P <- [P1,P2,P3]] end,

    [?_assertEqual(Expected(L), filter([P1,P2,P3], L) ) || L <- seqs(10) ].
				       
filter_test() ->
    L = lists:seq(1,10),

    P1 = fun(X) -> X rem 2 == 0 end,
    P2 = fun(X) -> X rem 2 == 1 end,
    P3 = fun(X) -> X > 3 end,

    %%E = [[2,4,6,8,10],[1,3,5,7,9],[4,5,6,7,8,9,10]],
    E = [lists:filter(P,L) || P <- [P1,P2,P3]],
    
    ?assertEqual(E, filter([P1,P2,P3], L)).
    
split_concat_test_() ->
    %% Make sure the result of concatenating the sublists equals the
    %% original list.
    
    L = lists:seq(1,99),
    [?_assertEqual(L, lists:concat(split(L,N))) || N <- lists:seq(1,133)].

split_n_test_() ->
    %% Make sure the correct number of sublists are generated. 
    
    M = 99,
    L = lists:seq(1,M),
    Num_of_lists = fun(List, N) when N =< length(List) ->
			   N;
		      (List, _) ->
			   length(List)
		   end,
    [?_assertEqual(Num_of_lists(L,N), length(split(L,N))) || N <- L].    


expected_stat(L, N) when N =< length(L) ->
    %% When spliting a list L into N sublists, we know there will only by two possible
    %% lengths of the sublists.

    
    %% Quotient and reminder when dividing length of L with N. 
    {_, Q, R} = lqr(L, N),

    %% There will allways be R sublists of length Q+1 and N-R sublists
    %% of length Q.
    
    {{R, Q+1}, {N-R, Q}};

expected_stat(L, _N) ->
    %% N greater than the length of L, hence all sublists will have
    %% length 1.

    {{length(L), 1}, {0,0}}.

stat(N, M, LL) ->
    %% Return a tuple {{Num_N, N}, {Num_M, M}} where Num_N is the
    %% number of lists of length N in LL and Num_M is the number of
    %% lists of length M in LL.
    
    S = filter([fun(X) -> X == N end, fun(X) -> X == M end], [length(L) || L <- LL]),

    [Num_N, Num_M] = [length(L) || L <- S],
    
    {{Num_N, N}, {Num_M, M}}.

split_stat_test_() ->
    %% Assure the list of sublists contains the correct number of
    %% lists of the two expected lengths.
	
    Assert = fun(L,N) ->
		     {_, Q, _} = lqr(L,N), 
		     ?_assertEqual(expected_stat(L,N), stat(Q+1, Q, split(L,N))) 
	     end,
	
    %% Generators can depend on other generator expressions, here N
    %% depends on the length of L.
    
    [Assert(L,N) ||  L <- seqs(33), N <- lists:seq(1,length(L)+5)].
    
