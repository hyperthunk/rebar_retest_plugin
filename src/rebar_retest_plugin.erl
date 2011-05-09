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
    catch(retest_core:run(create_args(Config))).

create_args(Config) ->
    [get_opt(retest.verbose, Config),
     get_opt(retest.outdir, Config),
     get_opt(retest.loglevel, Config),
     list_dir(Config)].

list_dir(_Config) ->
    ok.

get_opt(Name, Config) ->
    rebar_config:get_local(Config, Name, "").
