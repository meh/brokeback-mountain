-module(brokeback_req).

-export([respond/2, respond/3, respond/4, respond/5, ok/2, ok/3, ok/4, redirect/2, redirect/3]).
-export([resource/2]).

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
  respond(302, [{'Location', Url}], "", Req).
redirect(permanent, Url, Req) ->
  respond(301, [{'Location', Url}], "", Req).

resource(Options, {brokeback_req, Pid}) when is_list(Options) ->
  {Path, _} = cowboy_req:path(get_request(Pid)),
  [lists:foldl(fun clean_uri/2, P, Options) || P <- split_path(Path)].

get_request(Pid) ->
  Pid ! {get, self()},
  receive
    {request, Req} ->
      Req
  end.

set_request(Pid, Req) ->
  Pid ! {set, Req},
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
  convert_headers(Headers, []).
convert_headers([], Headers) ->
  Headers;
convert_headers([{Name, Value}|Rest], Headers) ->
  convert_headers(Rest, [{to_bin(Name), to_bin(Value)}|Headers]);
convert_headers([U|Rest], Headers) ->
  convert_headers(Rest, [U|Headers]).

to_bin(Bin) when is_binary(Bin) ->
  Bin;
to_bin(Str) when is_list(Str) ->
  list_to_binary(Str);
to_bin(Atom) when is_atom(Atom) ->
  list_to_binary(atom_to_list(Atom));
to_bin(Unknown) ->
  list_to_binary(io_lib:format("~w", [Unknown])).
