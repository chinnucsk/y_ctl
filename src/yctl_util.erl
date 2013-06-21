-module(yctl_util).

-include("yctl.hrl").
-include_lib("deps/ectl/include/ectl.hrl").

-export([
        try_to_int/1,
        connect_target/2,
        wait_for/1,
		get_opt/1,
		get_dir/1
    ]).

try_to_int(Value) ->
    case erl_scan:string(Value) of
        {ok, [{integer,1,ValueInt}], 1} ->
            ValueInt;
        _ ->
            false
    end.

connect_target(Opts, NodeStr) ->
    Node = init_net_kernel(Opts, NodeStr),
 	case {net_kernel:hidden_connect_node(Node), net_adm:ping(Node)} of
        {true, pong} ->
            Node;
        {_, pang} ->
            ?CONSOLE("Node ~p not responding to pings.\n", [Node]),
            halt(1)
    end.

init_net_kernel(Options, TargetNode) ->
    %% Generate a name for this node, based on target node
    ThisNode = append_node_suffix(TargetNode, "_ctl_"),
    case ectl:opt(shortname, Options, false) of
        true ->
            {ok, _} = net_kernel:start([ThisNode, shortnames]);
        false ->
            {ok, _} = net_kernel:start([ThisNode, longnames])
    end,
    %% Set the cookie, if necessary
    Cookie = ectl:opt(cookie, Options),
    erlang:set_cookie(node(), Cookie),
    %% Return the node name as an atom (with host appended to end, if necessary)
    nodename(TargetNode).

append_node_suffix(Name, Suffix) ->
    case string:tokens(Name, "@") of
        [Node, Host] ->
            y:to_a(lists:concat([Node, Suffix, os:getpid(), "@", Host]));
        [Node] ->
            y:to_a(lists:concat([Node, Suffix, os:getpid()]))
    end.

nodename(Name) ->
    case string:tokens(Name, "@") of
        [_Node, _Host] ->
            y:to_a(Name);
        [Node] ->
            [_, Host] = string:tokens(y:to_l(node()), "@"),
            y:to_a(lists:concat([Node, "@", Host]))
    end.

wait_for(Pid) ->
    Mref = erlang:monitor(process, Pid),
    receive
        {'DOWN', Mref, _, _, _} ->
            ok
    end.

get_opt(Opts) ->
    Con = ectl:opt(config, Opts, ?CONF_FILE),
    ectl:consult(config,Opts,Con).

get_dir(Opts) ->
	Opt = get_opt(Opts),
    ectl:opt(dir, Opt).
