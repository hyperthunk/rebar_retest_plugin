-module(testmod_rt).

-compile(export_all).

files() -> [].

run(_Dir) ->
    mymod:do_something(),
    ok.
