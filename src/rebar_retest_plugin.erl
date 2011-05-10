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

-export([retest/2]).

retest(Config, _AppFile) ->
    case code:which(retest_core) of
        non_existing ->
            rebar_log:log(error, "Retest not found on path!~n", []),
            {error, not_found};
        _ ->
            catch(retest_core:run(create_args(Config)))
    end.

create_args(Config) ->
    lists:concat([get_opts(Config), list_dir(Config)]).

get_opts(Conf) ->
    OptStrings = [ get_opt(K, Conf) || K <- [retest.verbose, 
                                             retest.outdir, 
                                             retest.loglevel] ],
    Opts = [ X || X <- OptStrings, length(X) > 0 ],
    rebar_log:log(error, "ReTest Options: ~p~n", [Opts]),
    Opts.

list_dir(Config) ->
    TestDir = rebar_config:get_local(Config, retest.testdir, "retest"),
    rebar_log:log(info, "ReTest TestDir: ~p~n", [TestDir]),
    case file:list_dir(TestDir) of
        {ok, Dirs} ->
            Targets = [ filename:join(TestDir, D) || D <- Dirs ],
            rebar_log:log(error, "ReTest Targets: ~p~n", [Targets]),
            Targets;
        _ ->
            []
    end.

get_opt(retest.verbose, Config) ->
    case rebar_config:get_local(Config, retest.verbose, undefined) of
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
get_opt(retest.loglevel, Config) ->
    case rebar_config:get_local(Config, retest.loglevel, undefined) of
        undefined ->
            "";
        Other when is_atom(Other) ->
            atom_to_list(Other)
    end;
get_opt(Name, Config) ->
    rebar_config:get_local(Config, Name, "").
