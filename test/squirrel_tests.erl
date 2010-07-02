-module (squirrel_tests).
-include_lib("eunit/include/eunit.hrl").

setup() ->
  squirrel_sup:start_link(),
  _Pid = test_server:start(),
  ok.
  
teardown(_X) ->
  squirrel_sup:stop(),
  ok.

starting_test_() ->
  {spawn,
    {setup,
      fun setup/0,
      fun teardown/1,
      [
        fun squirrel_simple_tests/0,
        fun squirrel_subscribe_tests/0
      ]
    }
  }.

squirrel_simple_tests() ->
  ?assert(['_squirrel_worker'] =:= squirrel_sup:running_queues()),
  squirrel:new_queue(dogs),
  ?assert([dogs, '_squirrel_worker'] =:= squirrel_sup:running_queues()),
  squirrel:new_queue(dogs),
  ?assert([dogs, '_squirrel_worker'] =:= squirrel_sup:running_queues()),
  passed.

squirrel_subscribe_tests() ->
  ?assert([dogs, '_squirrel_worker'] =:= squirrel_sup:running_queues()),
  squirrel:subscribe(dogs, fun(Num) ->
    test_server:add(Num)
  end),
  squirrel:publish(dogs, 2),
  timer:sleep(100),
  ?assert(2 =:= test_server:get_value(self())),
  squirrel:publish(dogs, 2),
  timer:sleep(100),
  ?assert(4 =:= test_server:get_value(self())),
  passed.
