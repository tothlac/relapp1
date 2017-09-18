-module(relapp_m1).

-export([store_fun/0]).

store_fun() ->
    F = fun() ->
            io:format("hello friend, i'm a function\n")
        end,
    relapp_srv2:store_fun(F).
