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
-export([add_job/1, add_requests/1, execute_call/2]).

-record(state, {supervisor, doc_root}).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_job([MilliSec, {pfr, Params}]) ->
    gen_server:cast(ed_cron, {add_job, pfr, Params, MilliSec}).

add_requests(Requests) ->
    gen_server:cast(ed_cron, {execute_requests, Requests}).

%% ===================================================================
%% Callbacks
%% ===================================================================

init(SupervisorPid) ->
    io:format("start scheduler: ~p~n", [SupervisorPid]),
    {Config} = get_config(),
    add_jobs_by_config(Config),
    execute_requests_by_config(Config),

    {ok, #state{
            supervisor=SupervisorPid,
            doc_root=proplists:get_value(<<"doc_root">>, Config)
        }
    }.

terminate(_Reason, _State) ->
    ok.

%% ====================================================================
%% gen_server (call events)
%% ====================================================================

handle_call(_, _From, State) ->
    {noreply, State}.

%% ====================================================================
%% gen_server (cast events)
%% ====================================================================

handle_cast({add_job, pfr, Params, MilliSec}, State) ->
    io:format("add new pfr job: ~p~n", [Params]),
    ok = register_call({MilliSec, Params}),
    {noreply,State};
handle_cast({execute_requests, Requests}, State) ->
    lists:foreach(
        fun ({Params}) ->
            begin_pf_transaction([{<<"doc_root">>, State#state.doc_root} | Params])
        end,
        Requests
    ),

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

execute_call(0, Params) ->
    begin_pf_transaction(Params),
    ok;
execute_call(MilliSec, Params) ->
    begin_pf_transaction(Params),
    register_call({MilliSec, Params}),
    ok.

begin_pf_transaction(Params) ->
    poolboy:transaction(ed_caller, fun(Worker) ->
        gen_server:cast(Worker, {execute_call, Params})
    end),
    ok.

get_config() ->
    {ok, ConfigPath} = file:get_cwd(),
    {ok, RawConfig} = file:read_file(ConfigPath ++ "/jobs.json"),
    jiffy:decode(RawConfig).

add_jobs_by_config(Config) ->
    DocRoot = proplists:get_value(<<"doc_root">>, Config),
    Fun = fun ({Job}) ->
        Time = proplists:get_value(<<"time">>, Job),
        {Params} = proplists:get_value(<<"pfr">>, Job),
        add_job([Time, {pfr, [{<<"doc_root">>, DocRoot} | Params]}]),
        ok
    end,
    register_pfrs(Config, <<"jobs">>, Fun),
    ok.

execute_requests_by_config(Config) ->
    DocRoot = proplists:get_value(<<"doc_root">>, Config),
    Fun = fun ({Request}) ->
        {Params} = proplists:get_value(<<"pfr">>, Request),
        begin_pf_transaction([{<<"doc_root">>, DocRoot} | Params]),
        ok
    end,
    register_pfrs(Config, <<"requests">>, Fun),
    ok.

register_pfrs( Config, Type, Fun ) ->

    Pfrs = proplists:get_value(Type, Config),
    lists:foreach(Fun, Pfrs),
    ok.

