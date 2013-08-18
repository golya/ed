-module(ed_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1, poc/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:start(ed).

start(_StartType, _StartArgs) ->
    io:format("ED stared.~n", []),
    application:start(ex_fcgi),
    application:start(inets),
    application:start(crypto),
    application:start(ssl),
    application:start(cowboy),
    ed_sup:start_link().

stop(_State) ->
    application:stop(cowboy),
    application:stop(ex_fcgi),
    application:stop(inets),
    application:start(crypto),
    application:start(ssl),
    ok.

poc() ->
    ed_cron:add_job([1000, {pfr, [{script_filename, <<"/index.php">>}]}]),
    ed_cron:add_job([5000, {pfr, [{script_filename, <<"/slowdown.php">>}]}]),
    ok.