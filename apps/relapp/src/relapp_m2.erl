-module(relapp_m2).

-export([test/1]).

test(undefined) -> {error, no_arg};
test("error") -> {error, "error"};
test(Arg) -> {ok, Arg}.

