%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. 六月 2016 14:24
%%%-------------------------------------------------------------------
-module(common).
-author("Administrator").

%% API
-export([deal_cmd/3]).
-include("constants.hrl").

deal_cmd(?cmd_lock, Key, Map) ->
    CountPre = maps:get(Key, Map, 0),
    case dls_worker:try_lock(Key) of
        get_lock ->
            self() ! {get_lock, Key};
        wait ->
            ok
    end,
    Map#{Key => CountPre + 1};
deal_cmd(?cmd_unlock, Key, Map) ->
    case maps:get(Key, Map, 0) of
        0 ->
            Map;
        CountPre ->
            case dls_worker:unlock(Key) of
                ok ->
                    self() !{unlock, Key},
                    Map#{Key := CountPre + 1};
                error ->
                    Map
            end
    end.