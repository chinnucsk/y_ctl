-define(SCRIPT, "yctl").
-define(SCRIPT(SubCmd), ?SCRIPT ++ " " ++ SubCmd).
-define(CONF_FILE, "yctl.config").

-define(CONFIG(Config), [
        {config, $c, "config", string, "config file to use"} | Config
    ]).

-define(CONFIG_T(Config), [
        {token, $t, "token", string, "authorized token (required)"} | ?CONFIG(Config)
    ]).

-define(WITH_TOKEN(Opts, Fun, Type), 
    y_cli:with_token(Opts, Fun, spec(Type), cmdline(Type))).

-define(IS_PATH(Path), hd(Path) == $/).

