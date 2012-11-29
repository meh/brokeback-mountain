-module(brokeback_bridge).

-export([init/3, handle/2, terminate/2]).

init(_Trans, Req, Loop) ->
  {ok, Req, Loop}.

handle(Req, Loop) ->
  BReq = {brokeback_req, self()},
  process_flag(trap_exit, true),
  spawn_link(fun() -> Loop(BReq) end),
  loop(Req, Loop, []).

loop(Req, Loop, PropList) ->
  receive
    {get_request, Pid} ->
      Pid ! {request, Req},
      loop(Req, Loop, PropList);
    {set_request, Req2} ->
      loop(Req2, Loop, PropList);
    {get_proplist, Pid} ->
      Pid ! {proplist, PropList},
      loop(Req, Loop, PropList);
    {set_proplist, PropList2} ->
      loop(Req, Loop, PropList2);
    {'EXIT', _, normal} ->
      {ok, Req, Loop};
    {'EXIT', _, Reason} ->
      {error, Reason, Loop}
  end.

terminate(_, _) ->
  ok.
