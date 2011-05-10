-module(mymod).

-export([do_something/0]).

do_something() ->
    file:write("something.out", <<"something.out">>).
