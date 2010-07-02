% squirrel
-record (queue, {
  name,         % name of the queue
  subscribers,  % subscribers of the queue
  durable       % should we store this to disk
}).

-record (subscriber, {
  pid,          % pid of subscriber
  fun           % function to call when a message is received
}).