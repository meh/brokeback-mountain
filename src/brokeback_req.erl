-module(brokeback_req).

-export([respond/2, respond/3, respond/4, respond/5, ok/2, ok/3, ok/4, redirect/2, redirect/3]).
-export([resource/2]).
-export([get/2, get_variable/3, parse_qs/1]).
-export([get_request/1]).

get(transport, {brokeback_req, Pid}) ->
  cowboy_req:get(transport, get_request(Pid));

get(socket, {brokeback_req, Pid}) ->
  cowboy_req:get(socket, get_request(Pid));

get(socket_mode, Req) ->
  case get(transport, Req) of
    ssl -> https;
    _   -> http
  end;

get(peer_port, Req) ->
  case (get(transport, Req)):peername(get(socket, Req)) of
    {ok, {_, Port}} -> Port;
    _               -> undefined
  end;

get(peer_cert, Req) ->
  case get(transport, Req) of
    ssl ->
      case ssl:peercert(get(socket, Req)) of
        {ok, Cert} -> Cert;
        _          -> undefined
      end;

    _ -> undefined
  end;

get(connection, Req) ->
  case lists:keyfind(connection, 1, get(headers, Req)) of
    false         -> keep_alive;
    {_Key, Value} -> connection(get(vsn, Req), Value)
  end;

get(content_length, {brokeback_req, Pid}) ->
  {Length, _} = cowboy_req:body_length(get_request(Pid)),
  Length;

get(vsn, {brokeback_req, Pid}) ->
  cowboy_req:get(version, get_request(Pid));

get(method, {brokeback_req, Pid}) ->
  PropList = get_proplist(Pid),

  case lists:keyfind(method, 1, PropList) of
    false ->
      Method = list_to_atom(binary_to_list(cowboy_req:get(method, get_request(Pid)))),
      set_proplist(Pid, [{method, Method} | PropList]),
      Method;

    {_Key, Value} -> Value
  end;

get(uri, Req = {brokeback_req, Pid}) ->
  PropList = get_proplist(Pid),

  case lists:keyfind(uri, 1, PropList) of
    false ->
      Uri = case resource([], Req) of
        invalid -> invalid;
        Res     -> "/" ++ string:join(Res, "/")
      end,

      set_proplist(Pid, [{uri, Uri} | PropList]),
      Uri;

    {_Key, Value} -> Value
  end;

get(args, {brokeback_req, Pid}) ->
  {Args, _} = cowboy_req:qs(get_request(Pid)),
  binary_to_list(Args);

get(headers, {brokeback_req, Pid}) ->
  PropList = get_proplist(Pid),

  case lists:keyfind(headers, 1, PropList) of
    false ->
      {Headers0, _} = cowboy_req:headers(get_request(Pid)),
      Headers       = lists:map(fun
            ({Key, Value}) -> {list_to_atom(binary_to_list(Key)), binary_to_list(Value)};
            (Unknown)      -> Unknown
        end, Headers0),

      set_proplist(Pid, [{headers, Headers} | PropList]),
      Headers;

    {_Key, Value} -> Value
  end;

get(body, {brokeback_req, Pid}) ->
  CowboyReq = get_request(Pid),

  case cowboy_req:has_body(CowboyReq) of
    {true, _} ->
      PropList = get_proplist(Pid),

      case lists:keyfind(body, 1, PropList) of
        false ->
          {ok, Body, CowboyReq0} = cowboy_req:body(CowboyReq),

          set_request(Pid, CowboyReq0),
          set_proplist(Pid, [{body, Body}|PropList]),

          Body;

        {_Key, Value} -> Value
      end;

    _ -> <<>>
  end;

get(peer_addr, {brokeback_req, Pid}) ->
  {PA, _} = cowboy_req:peer_addr(get_request(Pid)),
  PA;

get(uri_unquoted, {brokeback_req, Pid} = Req) ->
  PropList = get_proplist(Pid),

  case lists:keyfind(uri_unquoted, 1, PropList) of
    false ->
      UriUnquoted = case resource([urldecode], Req) of
        invalid -> invalid;
        Res     -> "/" ++ string:join(Res, "/")
      end,

      set_proplist(Pid, [{uri_unquoted, UriUnquoted}|PropList]),

      UriUnquoted;

    {_Key, Value} -> Value
  end.

connection({1, 1}, V) ->
  case string:to_upper(V) of
    "CLOSE" -> close;
    _       -> keep_alive
  end;

connection({1, 0}, V) ->
  case string:to_upper(V) of
    "KEEP-ALIVE" -> keep_alive;
    _            -> close
  end;

connection(_Vsn, _V) -> close.

get_variable(VarName, Variables, _ReqT) ->
  case lists:keyfind(VarName, 1, Variables) of
    false       -> undefined;
    {_K, Value} -> Value
  end.

parse_qs({brokeback_req, Pid}) ->
  PropList = get_proplist(Pid),

  case lists:keyfind(qs, 1, PropList) of
    false ->
      {Args, CBReq0} = cowboy_req:qs_vals(get_request(Pid)),

      set_request(Pid, CBReq0),

      Qs = lists:map(fun
          ({Key, Value}) -> {binary_to_list(Key), binary_to_list(Value)};
          (Unknown)      -> Unknown
        end, Args),

      set_proplist(Pid, [{qs, Qs} | PropList]),

      Qs;

    {_Key, Value} -> Value
  end.

respond(HttpCode, Req) ->
  respond(HttpCode, [], [], Req).

respond(HttpCode, Body, Req) ->
  respond(HttpCode, [], Body, Req).

respond(HttpCode, Headers, Body, {brokeback_req, Pid}) ->
  {ok, Req2} = cowboy_req:reply(HttpCode, convert_headers(Headers),
    to_bin(Body), get_request(Pid)),

  set_request(Pid, Req2).

respond(HttpCode, Headers, Template, Vars, Req) ->
  respond(HttpCode, Headers, io_lib:format(Template, Vars), Req).

ok(Body, Req) ->
  ok([], Body, Req).

ok(Headers, Body, Req) ->
  respond(200, Headers, Body, Req).

ok(Headers, Template, Vars, Req) ->
  respond(200, Headers, Template, Vars, Req).

redirect(Url, Req) ->
  respond(302, [{"Location", Url}], "", Req).

redirect(permanent, Url, Req) ->
  respond(301, [{"Location", Url}], "", Req).

resource(Options, {brokeback_req, Pid}) when is_list(Options) ->
  PropList = get_proplist(Pid),
  Path = case lists:keyfind(resource, 1, PropList) of
    false ->
      {Path0, _} = cowboy_req:path(get_request(Pid)),
      Path1      = case sanitize_path_tokens(split_path(Path0)) of
        Path2 when is_list(Path2) -> Path2;
        _                         -> invalid
      end,

      set_proplist(Pid, [{resource, Path1}|PropList]),

      Path1;

    {_Key, Value} -> Value
  end,

  case Path of
    Path when is_list(Path) -> [lists:foldl(fun clean_uri/2, P, Options) || P <- Path];
    _                       -> invalid
  end.

get_request({brokeback_req, Pid}) ->
  get_request(Pid);

get_request(Pid) ->
  Pid ! {get_request, self()},

  receive
    {request, Req} -> Req
  end.

set_request({brokeback_req, Pid}, Req) ->
  set_request(Pid, Req);

set_request(Pid, Req) ->
  Pid ! {set_request, Req},

  ok.

get_proplist({brokeback_req, Pid}) ->
  get_proplist(Pid);

get_proplist(Pid) ->
  Pid ! {get_proplist, self()},

  receive
    {proplist, Req} -> Req
  end.

set_proplist({brokeback_req, Pid}, PropList) ->
  set_proplist(Pid, PropList);

set_proplist(Pid, PropList) ->
  Pid ! {set_proplist, PropList},

  ok.

skip_slashes(<<$/, Rest/bits>>) ->
  skip_slashes(Rest);

skip_slashes(Rest) ->
  Rest.

split_path(Path) when is_binary(Path) ->
  split_path(skip_slashes(Path), []).

split_path(Path, Acc) ->
  case binary:match(Path, <<"/">>) of
    nomatch when Path =:= <<>> ->
      lists:reverse([binary_to_list(S) || S <- Acc]);

    nomatch ->
      lists:reverse([binary_to_list(S) || S <- [Path|Acc]]);

    {Pos, _} ->
      <<Segment:Pos/binary, _:8, Rest/bits>> = Path,
      split_path(skip_slashes(Rest), [Segment|Acc])
  end.

clean_uri(lowercase, Uri) ->
  string:to_lower(Uri);

clean_uri(urldecode, Uri) ->
  binary_to_list(cowboy_http:urldecode(list_to_binary(Uri)));

clean_uri(_Unavailable, Uri) ->
  Uri.

convert_headers(Headers) ->
  lists:map(fun
      ({Name, Value}) -> {to_bin(Name), to_bin(Value)};
      (Unknown)       -> Unknown
    end, Headers).

to_bin(Bin) when is_binary(Bin) ->
  Bin;

to_bin(Str) when is_list(Str) ->
  list_to_binary(Str);

to_bin(Atom) when is_atom(Atom) ->
  list_to_binary(atom_to_list(Atom));

to_bin(Unknown) ->
  list_to_binary(io_lib:format("~w", [Unknown])).

sanitize_path_tokens(Path) when is_list(Path) ->
  F = fun(B) ->
      case string:str(B, "\\") of
        0 -> false;
        _ -> true
      end
  end,

  case lists:any(F, Path) of
    true -> invalid;
    _    -> sanitize_path_tokens(lists:reverse(Path), 0, [])
  end.

sanitize_path_tokens([], RemCount, _Acc) when RemCount > 0 ->
  invalid;

sanitize_path_tokens([], _RemCount, Acc) ->
  Acc;

sanitize_path_tokens([".."], _RemCount, _Acc) ->
  invalid;

sanitize_path_tokens([".."|T], RemCount, Acc) ->
  sanitize_path_tokens(T, RemCount + 1, Acc);

sanitize_path_tokens([_H|T], RemCount, Acc) when RemCount > 0 ->
  sanitize_path_tokens(T, RemCount - 1, Acc);

sanitize_path_tokens([H|T], RemCount, Acc) ->
  sanitize_path_tokens(T, RemCount, [H|Acc]).
