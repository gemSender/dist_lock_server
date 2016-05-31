%%%-------------------------------------------------------------------
%%% @author Color
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 五月 2016 22:19
%%%-------------------------------------------------------------------
-module(dls_worker).
-author("Color").

-behaviour(gen_server).

%% API
-export([start/1, get_pid/1, lock/1, unlock/1, try_lock/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {queue :: undefined | queue:queue(), key :: undefined | binary()}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start(Key::binary()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start(Key) ->
  gen_server:start_link({global, Key}, ?MODULE, [Key], []).

-spec(get_pid(Key::binary()) ->
  Pid :: pid()).
get_pid(Key) ->
  case start(Key) of
    {ok, Pid} ->
      Pid;
    {error, {already_started, Pid}} ->
      Pid
  end.

try_lock(Key) ->
  io:format("try get lock of~p~n", [Key]),
  Pid = get_pid(Key),
  gen_server:call(Pid, {lock, self()}).

lock(Key) ->
  case try_lock(Key) of
    get_lock ->
      ok;
    wait ->
      receive
        get_lock ->
          ok
      end
  end.

unlock(Key) ->
  io:format("unlock ~p~n", [Key]),
  Pid = get_pid(Key),
  gen_server:call(Pid, {unlock, self()}).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([Key]) ->
  {ok, #state{queue = queue:new(), key = Key}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_call({lock, Pid}, _From, State) ->
  Queue = State#state.queue,
  Reply = case queue:is_empty(Queue) of
            true ->
              get_lock;
            false ->
              wait
          end,
  NewQue = queue:in(Pid, Queue),
  {reply, Reply, State#state{queue = NewQue}};
handle_call({unlock, Pid}, _From, State) ->
  Queue = State#state.queue,
  {NewQueue, Reply} =
    case queue:peek(Queue) of
      empty ->
        {Queue, error};
      {value, Peek} ->
        if
          Peek =:= Pid ->
            {{value, _}, NewQueue_} = queue:out(Queue),
            case queue:peek(NewQueue_) of
              empty ->
                ok;
              {value, NewPeek} ->
                NewPeek ! {get_lock, State#state.key}
            end,
            {NewQueue_, ok};
          true ->
            {Queue, error}
        end
    end,
  {reply, Reply, State#state{queue = NewQueue}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
