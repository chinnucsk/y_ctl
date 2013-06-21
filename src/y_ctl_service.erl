-module(y_ctl_service).

-export([run/2]).

-include("yctl.hrl").
-include_lib("deps/ectl/include/ectl.hrl").

run([], _) ->
	ectl:usage([], cmdline());
run(["list","help"|_], _) ->
	ectl:usage(spec(list), cmdline(list));
run(["list" | _], Args) ->
	?with(spec(list), cmdline(list), [], Args, fun list_service/1);
run(["ping", Service | _], Args) ->
	?with(spec(ping), cmdline(ping), [Service], Args, fun ping_service/2);
run(["ping"], _) ->
	ectl:usage(spec(ping), cmdline(ping));
run(["attach", Service | _], Args) ->
	?with(spec(attach), cmdline(attach), [Service], Args, fun attach_service/2);
run(["attach"], _) ->
	ectl:usage(spec(attach), cmdline(attach)).


cmdline() ->
	{?SCRIPT("service"), "[command ...]", [
		{"list", "list service"},
		{"list help", "list help service"},
		{"ping", "ping service"},
		{"attach", "attach service"}
		]}.

spec(list) ->
	[
		{dir, $d, "dir", string, "directory"}
	];
spec(ping) ->
	?CONFIG([]);
spec(attach) ->
	?CONFIG([]).

cmdline(list) ->
	{?SCRIPT("service list"), "<service>", []};
cmdline(ping) ->
	{?SCRIPT("service ping"), "<service>", []};
cmdline(attach) ->
	{?SCRIPT("service attach"), "<service>", []}.

list_service(Opts) ->
	Con = get_content(Opts),
	Dir1 = get_y(Con, []),
	Dir = ensure_dir(Dir1, []),
	display(Dir).

display([]) ->
	ok;
display([Dir|Rest]) ->
	io:format("~p~n",[Dir]),
	display(Rest).

get_content(Opts) ->
	Path = case ectl:opt(dir, Opts) of
		undefined ->
			yctl_util:get_dir(Opts);
		Dir ->
			Dir
			end,
	Res = os:cmd("ls " ++ Path),
	string:tokens(Res, "\n").

get_y([], Dir) ->
	lists:reverse(Dir);
get_y([Obj|Rest], Dir) ->
	case Obj of
		"y" ++ _ ->
			get_y(Rest, [Obj|Dir]);
		_ ->
			get_y(Rest, Dir)
	end.

ensure_dir([], Dir) ->
	lists:reverse(Dir);
ensure_dir([Obj | Rest], Dir) ->
	case filelib:is_regular(Obj) of
		true ->
			ensure_dir(Rest, Dir);
		false ->
			ensure_dir(Rest, [Obj|Dir])
	end.

ping_service(Service, Opts) ->
	Dir = yctl_util:get_dir(Opts),
	Res1 = os:cmd(Dir ++ "/" ++ Service ++ "/bin/" ++ Service ++ " ping"),
	[Res] = string:tokens(Res1, "\n"),
	io:format("~p~n",[Res]).

attach_service(Service, Opts) ->
	Dir = yctl_util:get_dir(Opts),
	os:cmd(Dir ++ "/" ++ Service ++ "/bin/" ++ Service ++ " attach").
