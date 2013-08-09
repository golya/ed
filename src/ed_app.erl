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
    poc(),
    ed_sup:start_link().

stop(_State) ->
    ok.

poc() ->
    {ok, _Pid} = ex_fcgi:start(fcgi, "127.0.0.1", 9000),
    {ok, Path} = file:get_cwd(),
    Params = [
        {<<"GATEWAY_INTERFACE">>, <<"FastCGI/1.0">>},
        {<<"REQUEST_METHOD">>, <<"GET">>},
        {<<"SCRIPT_FILENAME">>, Path++<<"/index.php">>},
        {<<"SCRIPT_NAME">>, <<"/index.php">>},
        {<<"QUERY_STRING">>, <<"">>},
        {<<"REQUEST_URI">>, <<"/index.php">>},
        {<<"SERVER_PROTOCOL">>, <<"HTTP/1.1">>},
        {<<"SERVER_SOFTWARE">>, <<"php/fcgiclient">>}
    ],
    {ok, Ref} = ex_fcgi:begin_request(fcgi, responder, Params, 3000),
    get_messages(Ref),
    ok.

get_messages(Ref) ->
    receive        
        {ex_fcgi, Ref, Messages} ->            
            io:format("messages: ~p~n", [Messages]),
            case lists:last(Messages) of
                {end_request,request_complete,0} ->            
                    ok;
                _ ->
                    get_messages(Ref)
            end;
        {ex_fcgi_timeout, Ref} ->
            io:format("got timeout~n", [])
    after 2000 ->
        io:format("got nothing~n", [])
    end,
    ok.
