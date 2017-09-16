-module('RELAPP-CAPITAL-TEST_m2').

-export([test/1]).

test(undefined) -> {error, no_arg};
test(Arg) -> {ok, Arg}.

