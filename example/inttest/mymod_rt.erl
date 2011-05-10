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
    {ok, _Out} = retest:sh("rebar compile retest -v skip_deps=true"),
    %% retest_log:log(info, "Output: ~p~n", [Out]),
    [Expected] = filelib:wildcard("*/current/*/something.out"), 
    ?assert(filelib:is_regular(Expected)),
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

