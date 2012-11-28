-module(brokeback).
-behaviour(supervisor).
-vsn("0.1").

-export([start_link/1, stop/0, stop/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Options) when is_list(Options) ->
  case supervisor:start_link(?MODULE, [Options]) of
    {ok, _} ->
      ok;
    Reason ->
      {error, Reason}
  end.

stop() ->
  stop(?SERVER).

stop(Ref) ->
  cowboy:stop_listener(Ref).

init([Options]) ->
  OptionProps = [
    % TransOpts
    {ip, {0, 0, 0, 0}, fun check_and_convert_string_to_ip/1, ivalid_ip},
    {port, 80, fun is_non_neg_integer/1, port_not_integer},
    {backlog, 128, fun is_non_neg_integer/1, backlog_not_integer},
    {max_connections, 4096, fun is_non_neg_integer/1, invalid_max_connections_option},
    % nodelay
    % ProtoOpts
    {ssl, false, fun check_ssl_options/1, invalid_ssl_options},

    %brokeback
    {loop, {error, undefined_loop}, fun is_function/1, loop_not_function},
    {name, ?SERVER, fun true/1, invalid_name}
  ],
  OptionsVerified = lists:foldl(fun(OptionProp, Acc) -> [get_option(OptionProp, Options)|Acc] end, [], OptionProps),
  case proplists:get_value(error, OptionsVerified) of
    undefined ->
      % TransOpts
      Ip = proplists:get_value(ip, OptionsVerified),
      Port = proplists:get_value(port, OptionsVerified),
      Backlog = proplists:get_value(backlog, OptionsVerified),
      MaxConnections = proplists:get_value(max_connections, OptionsVerified),
      TransOpts = [{ip, Ip}, {port, Port}, {backlog, Backlog}, {max_connections, MaxConnections}],
      % ProtoOpts
      SslOptions0 = proplists:get_value(ssl, OptionsVerified),
      % brokeback options
      Name = proplists:get_value(name, OptionsVerified),
      Loop = proplists:get_value(loop, OptionsVerified),

      case SslOptions0 of
        false ->
          Method = start_http,
          InetOpt = case Ip of
            {_, _, _, _} ->
              inet;
            {_, _, _, _, _, _, _, _} ->
              inet6
          end,
          AdditionalOptions = [InetOpt],
          true;
        _ ->
          Method = start_https,
          AdditionalOptions = [{ssl_imp, new}|SslOptions0],
          true
      end,

      Dispatch = [{'_', [{[], brokeback_bridge, Loop}]}],
      ProtoOpts = [{dispatch, Dispatch}|AdditionalOptions] ++
        lists:foldl(fun (Key, Opts) -> proplists:delete(Key, Opts) end, Options, [ip, port, backlog, max_connections, ssl, loop, name]),
      AppStartResults = lists:keyfind(error, 1, [start_application(crypto), start_application(ranch), start_application(cowboy)]),
      case AppStartResults of
        false ->
          case cowboy:Method(Name, MaxConnections, TransOpts, ProtoOpts) of
            {ok, _} ->
              {ok, {{one_for_one, 10, 10}, []}};
            Reason ->
              {error, Reason}
          end;
        _ ->
          {error, AppStartResults}
      end;
    Reason ->
      {error, Reason}
  end.

% non API functions
get_option({OptionName, DefaultValue, CheckAndConvertFun, FailTypeError}, Options) ->
  case proplists:get_value(OptionName, Options) of
    undefined ->
      case DefaultValue of
        {error, Reason} ->
          {error, Reason};
        Value ->
          {OptionName, Value}
      end;
    Value ->
      case CheckAndConvertFun(Value) of
        false ->
          {error, {FailTypeError, Value}};
        true ->
          {OptionName, Value};
        OutValue ->
          {OptionName, OutValue}
      end
  end.

start_application(Application) ->
  case lists:keyfind(Application, 1, application:which_applications()) of
    false ->
      case application:start(Application) of
        ok ->
          ok;
        {error, Reason} ->
          {error, Reason}
      end;
    _ ->
      ok
  end.

% misultin functions

% Checks and if necessary converts a string Ip to inet repr.
-spec check_and_convert_string_to_ip(Ip::string() | tuple()) -> inet:ip_address() | false.
check_and_convert_string_to_ip(Ip) when is_tuple(Ip) ->
  case size(Ip) of
    4 ->
      % check for valid ipv4
      LIp = [Num || Num <- tuple_to_list(Ip), Num >= 0, Num =< 255],
      length(LIp) =:= 4 andalso Ip;
    8 ->
      % check for valid ipv6
      LIp = [Num || Num <- tuple_to_list(Ip), Num >= 0, Num =< 16#FFFF],
      length(LIp) =:= 8 andalso Ip;
    _ ->
      false
  end;
check_and_convert_string_to_ip(Ip) ->
  case inet_parse:address(Ip) of
    {error, _Reason} ->
      false;
    {ok, IpTuple} ->
      IpTuple
  end.

% Checks if all necessary Ssl Options have been specified
%-spec check_ssl_options(SslOptions::gen_proplist()) -> boolean().
check_ssl_options(SslOptions) ->
  Opts = [verify, fail_if_no_peer_cert, verify_fun, depth, certfile, keyfile, password, cacertfile, ciphers, reuse_sessions, reuse_session],
  F = fun({Name, _Value}) ->
    case lists:member(Name, Opts) of
      false -> false;
      _ -> true
    end
  end,
  lists:all(F, SslOptions).

% check if recbuf has been set
-spec check_recbuf(RecBuf::default | non_neg_integer()) -> boolean().
check_recbuf(default) -> default;
check_recbuf(RecBuf) when is_integer(RecBuf), RecBuf > 0 -> true;
check_recbuf(_RecBuf) -> false.

% check if access log fun has been set
-spec check_access_log(AccessLogFun::false | function()) -> boolean().
check_access_log(undefined) -> true;
check_access_log(AccessLogFun) when is_function(AccessLogFun) -> true;
check_access_log(_AccessLogFun) -> false.

% check if ws specified versions are implemented. order does matter so we build a list in proper order
%-spec check_ws_version([websocket_version()]) -> false | [websocket_version()].
check_ws_version(WsVsn) ->
  ImplementedVsn = ['draft-hybi-17', 'draft-hybi-10', 'draft-hixie-76', 'draft-hixie-68'],
  %  build an ordered list of supported versions chosen by user.
  F = fun(SupportedVsn, Acc) ->
    case lists:member(SupportedVsn, WsVsn) of
      true -> [SupportedVsn|Acc];
      false -> Acc
    end
  end,
  OrderedVsn = lists:reverse(lists:foldl(F, [], ImplementedVsn)),
  % if length do not agree, then user has specified an unsupported version
  case length(OrderedVsn) =:= length(WsVsn) of
    true -> OrderedVsn;
    _ -> false
  end.

% check if a number is a non negative integer
-spec is_non_neg_integer(term()) -> boolean().
is_non_neg_integer(N) when is_integer(N), N >= 0 -> true;
is_non_neg_integer(_) -> false.

% check if the static option is valid
-spec check_static(list() | false) -> boolean().
check_static(false) -> true;
check_static(Path) when is_list(Path) -> true;
check_static(_) -> false.

true(_) -> true.
