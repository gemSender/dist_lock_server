%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. 六月 2016 13:43
%%%-------------------------------------------------------------------
-module(dls_ws_handler).
-author("Administrator").

-behaviour(cowboy_websocket_handler).
-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).
-include("constants.hrl").

-record(state, {map :: map()}).
%% callbacks
init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    {ok, Req, #state{map = #{}}}.

websocket_handle({text, Msg}, Req, State) ->
    Data = jiffy:decode(Msg, [return_maps]),
    #{<<"cmd">> := Command, <<"key">> := Key} = Data,
    NewMap =
        case Command of
            <<"lock">> ->
                common:deal_cmd(?cmd_lock, Key, State#state.map);
            <<"unlock">> ->
                common:deal_cmd(?cmd_unlock, Key, State#state.map)
        end,
    {ok, Req, State#state{map = NewMap}};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({get_lock, Key}, Req, State) ->
    ReplyMap = #{<<"msg">> => <<"get_lock">>, <<"key">> => Key},
    {reply, {text, jiffy:encode(ReplyMap)}, Req, State};
websocket_info({unlock, Key}, Req, State) ->
    ReplyMap = #{<<"msg">> => <<"unlock">>, <<"key">> => Key},
    {reply, {text, jiffy:encode(ReplyMap)}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(Reason, _Req, _State) ->
    io:format("~p terminated with reason ~p~n", [self(), Reason]),
    ok.
