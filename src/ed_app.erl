%% @author Adam Golya <adam.stork@gmail.com>

%% @doc ED is a tentative project for examine concept about
%%  erlang based php process handling.
%%
%% <ul>
%%   <li><a href="overview-summary.html">ED User Manual</a></li>
%% </ul>
%%
%% @type proplist() = [term()]
%%
-module(ed_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

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