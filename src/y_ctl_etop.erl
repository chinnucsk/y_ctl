-module(y_ctl_etop).

-export([run/2]).

-include("yctl.hrl").
-include_lib("deps/ectl/include/ectl.hrl").

run([], _) ->
	ectl:usage([], cmdline());
run(["start","help"|_], _) ->
	ectl:usage(spec(start), cmdline());
run(["start" | _], Args) ->
	?with(spec(start), cmdline(start), [], Args, fun etop_start/1).


cmdline() ->
	{?SCRIPT("service"), "[command ...]",[
		{"start", "etop start"},
		{"start help", "help of start"}
	]}.

spec(start) ->
	[
		{node, $n, "node", string, "service node"},
		{sort, $s, "sort", atom, "sort method"},
		{tracing, $t, "tracing", atom, "tracing"},
		{cookie, $c, "cookie", atom, "erlang cookie"}
	].

cmdline(start) ->
	{?SCRIPT("etop start"), "<etop>", []}.

etop_start(Opts) ->
	Opts1 = case ectl:opt(node, Opts) of
		undefined ->
			yctl_util:get_opt(Opts);
		_ ->
			Opts
			end,
	Node1 = ectl:opt(node, Opts1),
	Node = yctl_util:connect_target(Opts1, Node1),
	Sort = ectl:opt(sort, Opts1, memory),
	Trace = ectl:opt(tracing, Opts1, off),
	EtopOpts = [{output,text},{tracing,Trace},{node,Node},{sort,Sort}],
	yctl_util:wait_for(spawn_link(fun() -> etop:start(EtopOpts) end)). 

