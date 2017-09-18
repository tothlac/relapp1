-module(relapp_m2).

-export([test/1]).

test(undefined) -> {error, no_arg};
test(m1) ->
    relapp_m1:test(m2);
test(Arg) -> {ok, Arg}.

