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
      Req2 = skip_body(Req, PropList),
      {ok, Req2, Loop};

    {'EXIT', _, Reason} ->
      skip_body(Req, PropList),
      {error, Reason, Loop}
  end.

skip_body(Req, PropList) ->
  case cowboy_req:has_body(Req) of
    {true, _} ->
      case lists:keyfind(body, 1, PropList) of
        false -> case cowboy_req:skip_body(Req) of
            {ok, Req2} -> Req2;
            _          -> Req
          end;

        _ -> Req
      end;

    _ -> Req
  end.

terminate(_, _) ->
  ok.
