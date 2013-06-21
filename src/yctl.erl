%% -----------------------------------------------------------------------------
%%
%%             ____    ____  __    __  .__   __.  __    ______
%%             \   \  /   / |  |  |  | |  \ |  | |  |  /  __  \  
%%              \   \/   /  |  |  |  | |   \|  | |  | |  |  |  | 
%%               \_    _/   |  |  |  | |  . `  | |  | |  |  |  | 
%%                 |  |     |  `--'  | |  |\   | |  | |  `--'  | 
%%                 |__|      \______/  |__| \__| |__|  \______/  
%%                                                   
%%                       Copyright (c) 2011 - 2013 Yunio.
%%
%% -----------------------------------------------------------------------------
-module(yctl).

-export([main/1,
		run/2]).

-include("yctl.hrl").
-include_lib("deps/ectl/include/ectl.hrl").

main(Args) ->
    application:set_env(ectl, cmd_mod_prefix, "y_ctl"),
	application:set_env(ectl, app_name, yctl),
	application:load(ectl),
	application:load(yctl),
	ectl:run(?MODULE, Args, spec(), cmdline()).

run([], _) ->
	ectl:usage(spec(), cmdline());
run(Targets, Args) ->
	ectl:subcmd(Targets, Args, spec(), cmdline()).

spec() ->
	[
		{help, $h, "help", string, "show command help"},
		{config, $c, "config", string, "config to use"}
	].

cmdline() ->
	{?SCRIPT, "[command ...]",[
		{"service", ""},
		{"etop", ""}
	]}.
