{application, squirrel,
 [
  {description, ""},
  {vsn, "0.1"},
  {modules, [
             squirrel_app, squirrel_sup,
             squirrel, squirrel_worker
            ]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {mod, { squirrel_app, []}},
  {env, []}
 ]}.
