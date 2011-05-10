-module(testmod_rt).

-compile(export_all).

files() -> [].

run(_Dir) ->
    X = mymod:do_something(),
    retest_log:log(debug, "Result: ~p~n", [X]),
    ok.
