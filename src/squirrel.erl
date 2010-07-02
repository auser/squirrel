%%% squirrel_srv.erl
%% @author Ari Lerner <arilerner@mac.com>
%% @copyright 06/24/10 Ari Lerner <arilerner@mac.com>
%% @doc Basic squirrel
-module (squirrel).

-behaviour(gen_server).

% We use an ETS table to maintain the state of the queue (so we don't lose any messages)
% even if the server needs to be restarted.
-define (QUEUE_TABLE, 'squirrel_table').

%% API
-export([
  start_link/0,   % Start
  new_queue/1,    % Add a new queue
  subscribe/2,    % Add a subscriber
  publish/2       % Publish a message
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% gen_cluster callback
% -export([handle_join/3, handle_leave/4]).

-record(state, {
  queues
}).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], [{timeout, infinity}]).

new_queue(Name) ->
  gen_server:call(?SERVER, {new_queue, Name}, infinity).

subscribe(Name, Fun) ->
  gen_server:call(?SERVER, {subscribe, Name, Fun}).

publish(Name, Msg) ->
  gen_server:cast(?SERVER, {publish, Name, Msg}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
  {ok, #state{
    queues = dict:new()
  }}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({new_queue, Name}, _From, State) ->
  Reply = handle_start_queue(Name),
  {reply, Reply, State};
handle_call({subscribe, Name, Fun}, _From, State) ->
  QueuePid = get_queue_worker(Name),
  Reply = squirrel_worker:subscribe(QueuePid, Fun),
  {reply, Reply, State};
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({publish, Name, Msg}, State) ->
  QueuePid = get_queue_worker(Name),
  squirrel_worker:publish(QueuePid, Msg),
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
get_queue_worker(Name) ->
  case squirrel_sup:is_queue_running(Name) of
    false ->
      handle_start_queue(Name);
    true ->
      proplists:get_value(Name, squirrel_sup:queue_pids())
  end.

handle_start_queue(Name) ->
  case squirrel_sup:is_queue_running(Name) of
    false -> 
      {ok, Pid} = squirrel_sup:start_named_queue(Name),
      Pid;
    true -> ok
  end.