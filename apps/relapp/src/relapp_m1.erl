-module(relapp_m1).

-export([test/1,
         store_fun/0]).

test(Arg) -> {ok, Arg}.

store_fun() ->
    F = fun() ->
            io:format("hello, i'm a function\n")
        end,
    relapp_srv2:store_fun(F).
