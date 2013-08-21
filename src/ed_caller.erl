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

-include("ed.hrl").

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

handle_call(_, _From, State) ->
    {noreply, State}.

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
    ScriptFilename = proplists:get_value(<<"script_filename">>, Params),
    Query = proplists:get_value(<<"query_string">>, Params, <<"">>),
    {ok, Path} = file:get_cwd(),
    CGIParams = [
        {<<"GATEWAY_INTERFACE">>, <<"FastCGI/1.0">>},
        {<<"REQUEST_METHOD">>, <<"GET">>},
        {<<"SCRIPT_FILENAME">>, Path++ScriptFilename},
        {<<"SCRIPT_NAME">>, ScriptFilename},
        {<<"QUERY_STRING">>, Query},
        {<<"REQUEST_URI">>, ScriptFilename},
        {<<"SERVER_PROTOCOL">>, <<"HTTP/1.1">>},
        {<<"SERVER_SOFTWARE">>, <<"php/fcgiclient">>}
    ],
    {ok, Ref} = ex_fcgi:begin_request(Pid, responder, CGIParams, 1000000),
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
            ?DEBUG("got timeout", [])
    after 1000000 ->
        ?DEBUG("got nothing", [])
    end,
    ok.

format_msg(Messages) ->
    has_error(proplists:get_value(stderr, Messages, false)),
    Out = proplists:get_value(stdout, Messages),
    [_|Body] = binary:split(Out, <<"\r\n\r\n">>),
    ?DEBUG("[~p] Body: ~p", [calendar:local_time(), Body]),
    interpret_body(Body),
    ok.

interpret_body(Body) ->
    try
        ParsedBody = jiffy:decode(Body),
        serach_for_action(ParsedBody)
    catch
        Error:Reason ->  {Error, Reason}
    end,
    ok.


serach_for_action({[{<<"requests">>, Requests}]}) ->
    ed_cron:add_requests(Requests),
    ok;
serach_for_action(_) ->
    not_found.

has_error(false) ->
    ok;
has_error(Error) ->
    ?DEBUG("Error found: ~p", [Error]).