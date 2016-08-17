-module(relapp_m1).

-export([test/1,
         store_fun/0]).

test(undefined) -> {error, no_arg};
test(Arg) -> {ok, Arg}.

store_fun() ->
    F = fun() ->
            io:format("hello friend, i'm a function\n")
        end,
    relapp_srv2:store_fun(F).
