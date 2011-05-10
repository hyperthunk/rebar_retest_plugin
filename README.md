# Rebar ReTest Plugin

This is a rebar plugin that allows users to run ReTest by giving rebar
a new `retest` command. More information about ReTest is available from
https://github.com/dizzyd/retest.

## Installing

You can either (a) install into your `ERL_LIBS` path or (b) require the
plugin as a rebar dependency. For (a), you may wish to use a package 
manager:

    user@host$ alias PMAN='epm' # or agner, sutro, cean, etc
    user@host$ PMAN install hyperthunk/rebar-retest-plugin

Using rebar `deps` you may reference the plugin like so:

    {deps, [
      {'rebar-retest-plugin', ".*",
          {git, "../../rebar-retest-plugin", "master"}}
    ]}.

## Usage

Include the plugin (name) in your `rebar.config` in order to make its 
commands available to rebar. 

    {rebar_plugins, [rebar_dist_plugin]}.

You can now run `retest` from rebar:

    user@host$ rebar retest -v

## Config

The ReTest plugin supports the following configuration options:

1. retest_testdir:  the directory in which test files/dirs are located
2. retest_verbose:  instruct retest to run in `verbose` mode
3. retest_loglevel: specify the log level (passed to `retest_log`)
4. retest_outdir:   the *work* directory `retest` should use

NB: The directory specified by `retest_outdir` will be recursively 
deleted when the `rebar clean` command is given and the ReTest plugin
configured in your `rebar.config` file. This defaults to *rt.work*.

## License

This plugin is made available under a permissive BSD-style license. 
See the LICENSE file for details.

