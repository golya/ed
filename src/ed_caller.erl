%% @author Adam Golya <adam.stork@gmail.com>

%% @doc Responsible for exexute jobs.
%%
%% <ul>
%%   <li><a href="overview-summary.html">ED User Manual</a></li>
%% </ul>
%%
%% @type proplist() = [term()]
%%
-module(ed_caller).

-behaviour(gen_server).
-behaviour(poolboy_worker).

%% API
-export([start_link/1]).

%% Callbacks
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3]).

-record(state, {pid}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%% ===================================================================
%% Callbacks
%% ===================================================================

init(_Args) ->
    {ok, Pid} = ex_fcgi:start(list_to_atom(pid_to_list(self())), "127.0.0.1", 9000),
    {ok, #state{pid=Pid}}.

terminate(_Reason, _State) ->
    ok.

%% ====================================================================
%% gen_server (call events)
%% ====================================================================

handle_call({execute_call, Params}, _From, State) ->
    ok = fpm_call(Params, State#state.pid),
    {reply, ok, State}.

%% ====================================================================
%% gen_server (cast events)
%% ====================================================================

handle_cast({execute_call, Params}, State) ->
    ok = fpm_call(Params, State#state.pid),
    {noreply,State}.

%% ====================================================================
%% gen_server (other events)
%% ====================================================================
handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

fpm_call(Params, Pid) ->
    ScriptFilename = proplists:get_value(script_filename, Params),
    {ok, Path} = file:get_cwd(),
    CGIParams = [
        {<<"GATEWAY_INTERFACE">>, <<"FastCGI/1.0">>},
        {<<"REQUEST_METHOD">>, <<"GET">>},
        {<<"SCRIPT_FILENAME">>, Path++ScriptFilename},
        {<<"SCRIPT_NAME">>, ScriptFilename},
        {<<"QUERY_STRING">>, <<"">>},
        {<<"REQUEST_URI">>, ScriptFilename},
        {<<"SERVER_PROTOCOL">>, <<"HTTP/1.1">>},
        {<<"SERVER_SOFTWARE">>, <<"php/fcgiclient">>}
    ],
    {ok, Ref} = ex_fcgi:begin_request(Pid, responder, CGIParams, 3000),
    get_messages(Ref),
    ok.

get_messages(Ref) ->
    receive
        {ex_fcgi, Ref, Messages} ->
            case lists:last(Messages) of
                {end_request,request_complete,0} ->
                    format_msg(Messages),
                    ok;
                _ ->
                    get_messages(Ref)
            end;
        {ex_fcgi_timeout, Ref} ->
            io:format("got timeout~n", [])
    after 20000 ->
        io:format("got nothing~n", [])
    end,
    ok.

format_msg(Messages) ->
    Out = proplists:get_value(stdout, Messages),
    [_|Body] = binary:split(Out, <<"\r\n\r\n">>),
    io:format("[~p] Body: ~p~n", [calendar:local_time(), Body]),
    ok.
