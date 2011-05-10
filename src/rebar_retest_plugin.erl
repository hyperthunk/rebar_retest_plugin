%% -----------------------------------------------------------------------------
%%
%% Rebar Retest Plugin
%%
%% Copyright (c) 2011 Tim Watson (watson.timothy@gmail.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -----------------------------------------------------------------------------
%% @author Tim Watson [http://hyperthunk.wordpress.com]
%% @copyright (c) Tim Watson, 2011
%% @since: April 2011
%%
%% @doc Rebar Retest Plugin.
%%
%% This plugin allows you to run tests built with `retest' using rebar.
%% The plugin exports a new `retest` command, which will run any tests found in
%% the directory specified by 'retest.testdir', or "inttest" by default.
%%
%% Plugin Options (added to `rebar.config`):
%%
%%   retest.verbose  : true | false
%%   retest.testdir  : Path (relative) to test directories/files
%%   retest.outdir   : Path (relative) to "working" directory
%%   retest.loglevel : error | warn | info | debug
%% -----------------------------------------------------------------------------
-module(rebar_retest_plugin).

-export([retest/2, clean/2]).

clean(Config, _) ->
    WorkDir = rebar_config:get_local(Config, retest_outdir, "rt.work"),
    rebar_log:log(info, "Cleaning ~p~n", [WorkDir]),
    rebar_file_utils:rm_rf(WorkDir),
    ok.

retest(Config, _AppFile) ->
    case code:which(retest_core) of
        non_existing ->
            rebar_log:log(error, "Retest not found on path!~n", []),
            {error, not_found};
        _ ->
            Path = update_code_path(Config),
            Res = run(Config),
            restore_code_path(Path),
            Res
    end.

run(Config) ->
    catch(retest_core:run(create_args(Config))).

update_code_path(Config) -> 
    case rebar_config:get_local(Config, lib_dirs, []) of 
        [] -> 
            no_change; 
        Paths ->
            OldPath = code:get_path(),
            Pwd = rebar_utils:get_cwd(),
            LibPaths = [ filename:join(Pwd, P) || P <- Paths ],
            %% expand_lib_dirs(Paths, rebar_utils:get_cwd(), []), 
            ok = code:add_pathsa(LibPaths), 
            {old, OldPath}
    end.

restore_code_path(no_change) ->
    ok;
restore_code_path({old, Path}) ->
    true = code:set_path([F || F <- Path, filelib:is_file(F)]),
    ok.

create_args(Config) ->
    lists:concat([get_opts(Config), list_dir(Config)]).

get_opts(Conf) ->
    OptStrings = [ get_opt(K, Conf) || K <- [retest_verbose, 
                                             retest_outdir, 
                                             retest_loglevel] ],
    rebar_log:log(debug, "ReTest Pre-Opts: ~p~n", [OptStrings]),
    Opts = [ X || X <- OptStrings, length(X) > 0 ],
    rebar_log:log(info, "ReTest Options: ~p~n", [Opts]),
    Opts.

list_dir(Config) ->
	  io:format("Config: ~p~n", [Config]),
    TestDir = rebar_config:get(Config, retest_testdir, "retest"),
    rebar_log:log(info, "ReTest TestDir: ~p~n", [TestDir]),
    case file:list_dir(TestDir) of
        {ok, Dirs} ->
            Targets = [ filename:join(TestDir, D) || D <- Dirs ],
            rebar_log:log(info, "ReTest Targets: ~p~n", [Targets]),
            Targets;
        _ ->
            []
    end.

get_opt(retest_verbose, Config) ->
    case rebar_config:get_local(Config, retest_verbose, undefined) of
        true ->
            "-v";
        _ ->
            case rebar_config:is_verbose() of
                true ->
                    "-v";
                _ -> 
                    ""
            end
    end;
get_opt(retest_loglevel, Config) ->
    case rebar_config:get_local(Config, retest_loglevel, undefined) of
        undefined ->
            "";
        Other when is_atom(Other) ->
            "-l " ++ atom_to_list(Other)
    end;
get_opt(retest_outdir, Config) ->
    case rebar_config:get_local(Config, retest_outdir, undefined) of
        undefined ->
            "";
        Other ->
            "-o" ++ Other
    end;
get_opt(Name, Config) ->
    rebar_config:get(Config, Name, "").

