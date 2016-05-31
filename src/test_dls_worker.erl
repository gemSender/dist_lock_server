%%%-------------------------------------------------------------------
%%% @author Color
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 五月 2016 23:18
%%%-------------------------------------------------------------------
-module(test_dls_worker).
-author("Color").

%% API
-export([test_nolock/0, test_lock/1]).

test_nolock() ->
  io:format("print without lock...~n"),
  M = self(),
  W =
    fun() ->
      lists:foreach(
        fun(X) ->
          io:format("~p~n", [X])
        end,
        lists:seq(1, 10)
      ),
      M ! self()
    end,
  PList = lists:map(fun(_) -> spawn(W) end, lists:seq(1, 3)),
  wait(PList),
  io:format("print without lock done!~n").

test_lock(Key) ->
  io:format("print with lock...~n"),
  M = self(),
  W =
    fun() ->
      dls_worker:lock(Key),
      lists:foreach(
        fun(X) ->
          io:format("~p~n", [X])
        end,
        lists:seq(1, 10)
      ),
      dls_worker:unlock(Key),
      M ! self()
    end,
  PList = lists:map(fun(_) -> spawn(W) end, lists:seq(1, 3)),
  wait(PList),
  io:format("print with lock done!~n").

wait([]) ->
  ok;
wait([H|T]) ->
  receive
    H ->
      wait(T)
  end.

