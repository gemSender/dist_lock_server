%%%-------------------------------------------------------------------
%%% @author Color
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. 五月 2016 1:12
%%%-------------------------------------------------------------------
-module(conn_handler).
-behaviour(ranch_protocol).
-author("Color").
-define(reading_command, 0).
-define(reading_length, 1).
-define(reading_key, 2).
-include_lib("constants.hrl").

-record(state,{map :: undefined | map(), data_state :: integer(), data_pre :: {binary()|integer(), binary()|integer(), binary()}}).
%% callbacks
-export([start_link/4, init/4]).

start_link(Ref, Socket, Transport, Opts) ->
  Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
  {ok, Pid}.

init(Ref, Socket, Transport, _Opts = []) ->
  ok = ranch:accept_ack(Ref),
  loop(Socket, Transport, ?reading_command, #state{map = #{}, data_pre = {<<>>, <<>>, <<>>}}).

loop(Socket, Transport,StreamState, State)->
  Transport:setopts(Socket, [{active, once}]),
  {OK, Closed, Error} = Transport:messages(),
  receive
    {get_lock, Key} ->
      MsgType = int_to_bin(?msg_get_lock),
      Len = int_to_bin(byte_size(Key)),
      Transport:send(Socket, <<MsgType/binary, Len/binary, Key/binary>>),
      loop(Socket, Transport,StreamState, State);
    {unlock, Key} ->
      MsgType = int_to_bin(?msg_unlock),
      Len = int_to_bin(byte_size(Key)),
      Transport:send(Socket, <<MsgType/binary, Len/binary, Key/binary>>),
      loop(Socket, Transport,StreamState, State);
    {OK, Socket, Data} ->
      io:format("~p~n", [Data]),
      {NewStreamState, NewState} = on_message(StreamState, Data, State),
      loop(Socket, Transport, NewStreamState, NewState);
    {Closed, Socket} ->
      ok;
    {Error, Socket, _} ->
      ok = Transport:close(Socket)
  end.

on_message(StreamState, <<>>, State) ->
  {StreamState, State};
on_message(?reading_command, <<H, Rest/binary>>, State) ->
  {CmdBin, _Len, _DataBytes} =  State#state.data_pre,
  NewCmdBin = <<CmdBin/binary, H>>,
  case byte_size(NewCmdBin) of
    4 ->
      CmdInt = bin_to_int(NewCmdBin),
      io:format("Command ~p~n", [CmdInt]),
      on_message(?reading_length, Rest, State#state{data_state = ?reading_length, data_pre = {CmdInt, <<>>, <<>>}});
    N when N < 4 ->
      on_message(?reading_command, Rest, State#state{data_pre = {NewCmdBin, <<>>, <<>>}})
  end;
on_message(?reading_length, <<H, Rest/binary>>, State) ->
  {CmdInt, LenBin, _} =  State#state.data_pre,
  NewLenBin = <<LenBin/binary, H>>,
  case byte_size(NewLenBin) of
    4 ->
      case bin_to_int(NewLenBin) of
        0 ->
          State1 = State#state{map = common:deal_cmd(CmdInt, <<>>, State)},
          on_message(?reading_command, Rest, State1#state{data_pre = {<<>>, <<>>, <<>>}});
        LenInt ->
          io:format("Key Length ~p~n", [LenInt]),
          on_message(?reading_key, Rest, State#state{data_pre = {CmdInt, LenInt, <<>>}})
      end;
    N when N < 4 ->
      on_message(?reading_length, Rest, State#state{data_pre = {CmdInt, NewLenBin, <<>>}})
  end;
on_message(?reading_key, <<H, Rest/binary>>, State) ->
  {CmdInt, LenInt, KeyBin} = State#state.data_pre,
  NewKeyBin = <<KeyBin/binary, H>>,
  case byte_size(NewKeyBin) of
    LenInt ->
      State1 = State#state{map = common:deal_cmd(CmdInt, NewKeyBin, State)},
      on_message(?reading_command, Rest, State1#state{data_pre = {<<>>, <<>>, <<>>}});
    N when N < LenInt ->
      on_message(?reading_key, Rest, State#state{data_pre = {CmdInt, LenInt, NewKeyBin}})
  end.

bin_to_int(<<B1, B2, B3, B4>>) ->
  B1 bor (B2 bsl 8) bor (B3 bsl 16) bor (B4 bsl 24).

int_to_bin(N) ->
  <<(N band 16#FF)/integer, ((N bsr 8) band 16#FF)/integer, ((N bsr 16) band 16#FF)/integer, ((N bsr 24) band 16#FF)/integer>>.