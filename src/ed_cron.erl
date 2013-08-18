%% @author Adam Golya <adam.stork@gmail.com>

%% @doc Responsible for scheduling jobs.
%%
%% <ul>
%%   <li><a href="overview-summary.html">ED User Manual</a></li>
%% </ul>
%%
%% @type proplist() = [term()]
%%
-module(ed_cron).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% Callbacks
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3]).

%% API
-export([add_job/1, execute_call/2]).

-record(state, {supervisor}).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_job([MilliSec, {pfr, Params}]) ->
    gen_server:cast(ed_cron, {add_job, pfr, Params, MilliSec}).

%% ===================================================================
%% Callbacks
%% ===================================================================

init(SupervisorPid) ->
    io:format("start scheduler: ~p~n", [SupervisorPid]),
    {ok, #state{supervisor=SupervisorPid}}.

terminate(_Reason, _State) ->
    ok.

%% ====================================================================
%% gen_server (call events)
%% ====================================================================

handle_call({add_job, pfr, Params, MilliSec}, _From, State) ->
    io:format("add new pfr job: ~p~n", [Params]),
    ok = register_call({MilliSec, Params}),
    {reply, ok, State}.

%% ====================================================================
%% gen_server (cast events)
%% ====================================================================

handle_cast({add_job, pfr, Params, MilliSec}, State) ->
    io:format("add new pfr job: ~p~n", [Params]),
    ok = register_call({MilliSec, Params}),
    {noreply,State}.

%% ====================================================================
%% gen_server (other events)
%% ====================================================================
handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

register_call({MilliSec, Params}) ->
    timer:apply_after(MilliSec, ed_cron, execute_call, [MilliSec, Params]),
    ok.

execute_call(MilliSec, Params) ->
    poolboy:transaction(ed_caller, fun(Worker) ->
        gen_server:cast(Worker, {execute_call, Params})
    end),
    register_call({MilliSec, Params}),
    ok.