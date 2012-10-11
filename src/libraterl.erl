-module(libraterl).
-author('jonafree@gmail.com').
-behaviour(gen_server).

-export([start_link/3, start_link/4, gauge_bump/2, gauge_bump/4]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).

-include("libraterl.hrl").

-record(state, {user, api, url, success = 0, prefix, error = 0}).

start_link(User, API, Prefix) when is_list(User), is_list(API), is_list(Prefix) ->
    {ok, _PID} = gen_server:start_link(?MODULE, [User, API, Prefix], []).    
start_link(Name, User, API, Prefix) when is_list(User), is_list(API), is_list(Prefix) ->
    {ok, _PID} = gen_server:start_link(Name, ?MODULE, [User, API, Prefix], []).

gauge_bump(Name, Gauges) when is_list(Gauges) ->
    gen_server:cast(Name, {gauge_bump, Gauges}).
gauge_bump(Name, Key, Value, TS) when is_list(Key), is_integer(Value), is_integer(TS) ->
    gen_server:cast(Name, {gauge_bump, Key, Value, TS}).

init([User, API, Prefix]) ->
    {ok, #state{user = User, api = API, url = ?URL, prefix = Prefix}}.

handle_cast({gauge_bump, G}, State) ->
    {noreply, p_gauge_bump(G, State)};
handle_cast({gauge_bump, Key, Value, TS}, State) ->
    {noreply, p_gauge_bump([{Key, Value, TS}], State)}.

handle_call(undefined, _From, State) ->
    {noreply, State}.

handle_info(undefined, State) ->
    {noreply, State}.

code_change(_, Stage, _) ->
    {ok, Stage}.

terminate(_, _State) ->
    ok.

p_auth(H, #state{user = U, api = A}) ->
    [{"Authorization", "Basic " ++ base64:encode_to_string(U ++ ":" ++ A)}|H].

p_gauge_bump(GS, #state{url = URL, success = S, error = E, prefix = Prefix} = State) ->
    R = jiffy:encode({[{gauges, {lists:map(encode_gauge_fun(State), GS)}}]}),
    H = p_auth([], State),
    case httpc:request(post, {URL ++ "/metrics", H, "application/json", R}, [], []) of
        {ok, {{_, 200, _}, HR, B}} ->
	    State#state{success = S + 1};
	{error, socket_closed_remotely} ->
	    State#state{error = E + 1}
    end.

encode_gauge_fun(#state{prefix = Prefix}) ->
    fun({Key, Value, TS}) when is_list(Key), is_integer(Value), is_integer(TS) ->
	    {list_to_binary(Prefix ++ Key), {[
					      {value, Value},
					      {measure_time, TS}
					     ]}}
    end.
