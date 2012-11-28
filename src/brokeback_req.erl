-module(brokeback_req).

-export([respond/2, respond/3, respond/4, respond/5, ok/2, ok/3, ok/4, redirect/2, redirect/3]).
-export([resource/2]).

respond(HttpCode, Req) ->
  respond(HttpCode, [], [], Req).
respond(HttpCode, Body, Req) ->
  respond(HttpCode, [], Body, Req).
respond(HttpCode, Headers, Body, {brokeback_req, Pid}) ->
  Pid ! {response, HttpCode, convert_headers(Headers), to_bin(Body)},
  ok.
respond(HttpCode, Headers, Template, Vars, {brokeback_req, Pid}) ->
  Pid ! {response, HttpCode, convert_headers(Headers), to_bin(io_lib:format(Template, Vars))},
  ok.

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
  Pid ! {uri, self()},
  receive
    {uri, Uri} ->
      [ [ clean_uri(S, P) || S <- Options] || P <- split_path(Uri)]
  end.

split_path(<<$/, Path/bits>>) ->
  split_path(Path, []);
split_path(<<Path/bits>>) ->
  split_path(Path, []).

split_path(Path, Acc) ->
  case binary:match(Path, <<"/">>) of
    nomatch when Path =:= <<>> ->
      lists:reverse([cowboy_http:urldecode(S) || S <- Acc]);
    nomatch ->
      lists:reverse([cowboy_http:urldecode(S) || S <- [Path|Acc]]);
    {Pos, _} ->
      <<Segment:Pos/binary, _:8, Rest/bits>> = Path,
      split_path(Rest, [Segment|Acc])
  end.

clean_uri(lowercase, Uri) ->
  string:to_lower(Uri);
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
