-module(brokeback_bridge).

-export([init/3, handle/2, terminate/2]).

init(_Trans, Req, Loop) ->
  {ok, Req, Loop}.

handle(Req, Loop) ->
  BReq = {brokeback_req, self()},
  process_flag(trap_exit, true),
  spawn_link(fun() -> Loop(BReq) end),
  loop(Req, Loop).

loop(Req, Loop) ->
  receive
    {uri, Pid} ->
      {Uri, _} = cowboy_req:method(Req),
      Pid ! {uri, Uri},
      loop(Req, Loop);
    {response, Code, Headers, Body} ->
      {ok, Req2} = cowboy_req:reply(Code, Headers, Body, Req),
      loop(Req2, Loop);
    {'EXIT', _, normal} ->
      {ok, Req, Loop};
    {'EXIT', _, Reason} ->
      {error, Reason, Loop}
  end.

terminate(_, _) ->
  ok.
