-module(mymod_rt).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

files() ->
    [{copy, "rebar.config", "rebar.config"},
     {copy, "../deps", "deps"},
     {copy, "testmod.erl", "inttest/testmod_rt.erl"},
     {copy, "../src/mymod.erl", "src/mymod.erl"},
     {create, "ebin/myapp.app", app(myapp, [mymod])}].

run(_Dir) ->
    ?assertMatch({ok, _}, retest:sh("rebar get-deps compile")),
    ?assertMatch({ok, _}, retest:sh("rebar retest -v")),
    ?assert(filelib:is_regular("something.out")),
    ok.

%%
%% Generate the contents of a simple .app file
%%
app(Name, Modules) ->
    App = {application, Name,
           [{description, atom_to_list(Name)},
            {vsn, "1"},
            {modules, Modules},
            {registered, []},
            {applications, [kernel, stdlib]}]},
    io_lib:format("~p.\n", [App]).
