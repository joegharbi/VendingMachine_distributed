-module(machine).

%% ------ Staring the server --------
-export([start/1, stop/0]).
%% ------ Vending machine interface --------
-export([refill_machine/1, print_content/0, print_price/0, change_price/1]).
%% ------ Client interface --------
-export([insert_coins/1, select_beverage/1, cancel/0]).
-export([student/1, queuing/1, queuing/2]).
-export([init/1, loop/1]).

%% My host is : DESKTOP-2R1VPHD
-define(VM, {vending_machine, 'vm@DESKTOP-2R1VPHD'}).

start(Args) ->
    register(vending_machine, spawn_link(?MODULE, init, [Args])),
    ok.

stop() ->
    ?VM ! stop.

init(Price) ->
    InitState = {[], Price, 0},
    loop(InitState).

%% ------ Vending machine interface --------
refill_machine(L) ->
    ?VM ! {refill, L, self()},
    receive
        {newlist, A} ->
            A
    end.

change_price(P) ->
    ?VM ! {cprice, P, self()},
    receive
        A ->
            io:format("new price : ~p ~n", [A]),
            A
    end.

print_content() ->
    ?VM ! {pcontent, self()},
    receive
        {content, L} ->
            L
    end.

print_price() ->
    ?VM ! {pprice, self()},
    receive
        {price, P} ->
            io:format("Price is : ~p ~n", [P])
    end.

remove_from_list(L, K) ->
    E = lists:keyfind(K, 1, L),
    {_, N} = E,
    if N == 1 ->
           lists:keydelete(K, 1, L);
       true ->
           lists:keyreplace(K, 1, L, {K, N - 1})
    end.

%% ------ SERVER --------
loop(State) ->
    receive
        stop ->
            io:format("Stoping the server!");
        {refill, A, Pid} ->
            {OldList, Price, Coins} = State,
            NewList = lists:keymerge(1, OldList, A),
            Pid ! {newlist, NewList},
            NewState = {NewList, Price, Coins},
            loop(NewState);
        {cprice, NewPrice, Pid} ->
            {L, _, C} = State,
            NewState = {L, NewPrice, C},
            Pid ! NewPrice,
            loop(NewState);
        {pcontent, Pid} ->
            {L, _, _} = State,
            Pid ! {content, L},
            loop(State);
        {pprice, Pid} ->
            {_, P, _} = State,
            Pid ! {price, P},
            loop(State);
        {insertCoins, Coi, CPid} ->
            {List, Price, OldC} = State,
            NewCoins = Coi + OldC,
            if NewCoins < Price ->
                   CPid ! insufficient_founds,
                   NewState = {List, Price, NewCoins},
                   loop(NewState);
               true ->
                   X = lists:unzip(
                           lists:ukeysort(1, List)),
                   {BevList, _} = X,
                   CPid ! {unique_list, BevList},
                   receive
                       {givemebev, Bev, CPid} ->
                           M = lists:member(Bev, BevList),
                           if M == false ->
                                  CPid ! {unavailable},
                                  loop(State);
                              true ->
                                  ReturnCoins = NewCoins - Price,
                                  io:format("Success order from : ~p ~nMoney returned: ~p ~n!",
                                            [CPid, ReturnCoins]),
                                  NewList = remove_from_list(List, Bev),
                                  CPid ! {successorder, Bev, ReturnCoins},
                                  NewState = {NewList, Price, 0},
                                  loop(NewState)
                           end;
                       {cancel, CPid} ->
                           CPid ! {your_coins, NewCoins},
                           NewState = {List, Price, 0},
                           loop(NewState)
                   after 10000 ->
                       CPid ! {your_coins, NewCoins},
                       NewState = {List, Price, 0},
                       loop(NewState)
                   end
            end;
        {givemebev, Bev, CPid} ->
            {List, Price, Coi} = State,
            ReturnCoins = Coi - Price,
            M = lists:member(Bev, List),
            if Coi == 0 ->
                   CPid ! {insufficient_founds, Price},
                   loop(State);
               M == false ->
                   CPid ! {unavailable},
                   loop(State);
               true ->
                   NewList = remove_from_list(List, Bev),
                   CPid ! {successorder, Bev, ReturnCoins},
                   io:format("Success order from : ~p ~nMoney returned: ~p ~n!",
                             [CPid, ReturnCoins]),
                   NewState = {NewList, Price, 0},
                   loop(NewState)
            end;
        {cancel, CPid} ->
            {L, P, C} = State,
            CPid ! {your_coins, C},
            NewState = {L, P, 0},
            loop(NewState);
        _Other ->
            an_unhandled_message
    end.

%% ------ Client interface --------
insert_coins(C) ->
    ?VM ! {insertCoins, C, self()},
    receive
        insufficient_founds ->
            insufficient_founds;
        {unique_list, L} ->
            L
    end.

select_beverage(B) ->
    ?VM ! {givemebev, B, self()},
    receive
        {successorder, C, P} ->
            io:format("Success order of : ~p ~nMoney returned: ~p ~n!", [C, P]);
        {insufficient_founds, _} ->
            insufficient_founds;
        {unavailable} ->
            unavailable
    end.

cancel() ->
    ?VM ! {cancel, self()},
    receive
        {your_coins, C} ->
            C
    after 1000 ->
        ok
    end.

%%---------Student-----------------------
student(M) ->
    timer:sleep(
        rand:uniform(1000)),
    R = insert_coins(M),
    if R == insufficient_founds ->
           io:format("Sad Msg: no money!");
       length(R) == 0 ->
           io:format("Sad Msg: no drinks!");
       true ->
           B = select_beverage(lists:nth(
                                   rand:uniform(length(R)), R)),
           io:format("Happy to buy ~p ~n ", [B])
    end.

%%--------------Queuing---------------------

queuing(L) ->
    Pid = self(),
    [spawn_link(fun() -> Pid ! student(E) end) || E <- L].

queuing([], []) ->
    ok;
queuing(L, N) ->
    spawn(hd(N), machine, student(hd(L))),
    queuing(tl(L), tl(N)).
