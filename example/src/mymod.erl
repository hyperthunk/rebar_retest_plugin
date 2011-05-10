-module(mymod).

-export([do_something/0]).

do_something() ->
    {ok, Pwd} = file:get_cwd(),
    io:format("PWD: ~p~n", [Pwd]),
    file:write_file(filename:join(Pwd, "something.out"), <<"something.out">>).


