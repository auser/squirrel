-module (squirrel_sup).

-behaviour(supervisor).

%% API
-export([
  start_link/0,
  stop/0,
  start_named_queue/1,
  running_queues/0,
  is_queue_running/1,
  queue_pids/0
]).

%% Supervisor callbacks
-export([init/1]).

% timeout time
-define(SHUTDOWN_TIME, 16#ffffffff).
-define(MAX_RESTART,  10).
-define(MAX_TIME,     60).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, ?SHUTDOWN_TIME, Type, [I]}).
-define(SUP_CHILD (I, Args), {I, {supervisor, start_link, Args}, permanent, ?SHUTDOWN_TIME, supervisor, []}).
-define(NAMED_CHILD(Id, M, Args, Type), {Id, {M, start_link, Args}, temporary, ?SHUTDOWN_TIME, Type, [M]}).
-define(TCHILD(I, Type), {I, {I, start_link, []}, transient, ?SHUTDOWN_TIME, Type, [I]}).
-define(COUNT_CHILD(I, Type, Count), fun() -> [{N, {I, start_link, [N]}, transient, ?SHUTDOWN_TIME, Type, [I]} || N <- lists:seq(1, Count)] end()).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_named_queue(Name) ->
  supervisor:start_child(squirrel_workers_sup, ?NAMED_CHILD(Name, squirrel_worker, [Name], worker)).

running_queues() ->
  lists:map(fun({Name, _Pid}) -> Name end, queue_pids()).

queue_pids() ->
  lists:map(fun({Id, Pid, _, _}) -> {Id, Pid} end, supervisor:which_children(squirrel_workers_sup)).

is_queue_running(Name) ->
  lists:member(Name, running_queues()).

stop() ->
  lists:foreach(fun({WorkerId, _Pid, _, _}) ->
    supervisor:terminate_child(?MODULE, WorkerId)
  end, supervisor:which_children(squirrel_workers_sup)).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->
  Server    =  ?CHILD(squirrel, worker),
  WorkerSup  = ?SUP_CHILD(squirrel_workers_sup, [{local, squirrel_workers_sup}, ?MODULE, ['_squirrel_worker']]), 
    
  {ok, {{one_for_one, ?MAX_RESTART, ?MAX_TIME}, [Server, WorkerSup]}};

init([Name]) ->
  Child = ?NAMED_CHILD(Name, squirrel_worker, [Name], worker),
  {ok, {{one_for_one, ?MAX_RESTART, ?MAX_TIME}, [Child]}}.
