-module(citp_msex).
% -export([]).
-compile([export_all]).

-define(CITP_PORT, 4809).
-define(CITP_MULTICAST_IP, {224, 0, 0, 180}).


listen() ->
  {ok, Socket} = gen_udp:open(0, [binary, {active, true}, {broadcast, true}, {reuseaddr, true},
      {recbuf, 128000}, {read_packets, 256}, {multicast_loop, true}]),
  {ok, IfList} = inet:getif(),
  IpList = [{Socket, Ip} || {Ip = {A,B,C,D}, _Broadcast, _Subnet} <- IfList, A=/= 127, B=/=0, C=/=0, D=/=1],
  lists:foreach(fun(C) -> multicast_subscribe(C) end, IpList),
  {ok, Socket}.

multicast_subscribe({Socket, IpAddress}) ->
  ok = inet:setopts(Socket, [{add_membership, {?CITP_MULTICAST_IP, IpAddress}}]),
  io:format("Subscribing ~w to multicast ~w~n", [IpAddress, ?CITP_MULTICAST_IP]).

parseHeader(<<$C, $I, $T, $P, VersionMajor, VersionMinor, RequestIndex:16/little,
  MessageSize:32/little, MessagePartCount:16/little, MessagePart:16/little,
  ContentType:32/little, Data/binary>>) ->
  %io:format("Got CITP packet: ~p~n", [binary_to_list(<<ContentType:32/little>>)]),
  parseBody(<<ContentType:32/little>>, Data);
parseHeader(_Data) ->
  not_citp.

parseBody(<<$P, $I, $N, $F>>, <<$P, $L, $o, $c, ListeningPort:16/little, Strings/binary>>) ->
  %io:format("Got PLoc packet: port ~w~n", [ListeningPort]),
  StringList = binary_to_list(Strings),
  Type = lists:takewhile(fun(X) -> X /= 0 end, StringList),
  [_ | StringRest] = lists:dropwhile(fun(X) -> X /=0 end, StringList),
  Name = lists:takewhile(fun(X) -> X /= 0 end, StringRest),
  [_ | State0] = lists:dropwhile(fun(X) -> X /=0 end, StringRest),
  State = lists:filter(fun(X) -> X /= 0 end, State0),
  %% io:format("Type = ~p, Name = ~p, State: ~p Port: ~p~n", [Type, Name, State, ListeningPort]),
  {ploc, ListeningPort, Type, Name, State};
parseBody(_Data, _Data2) ->
  unknown_citp_packet.

sendPLoc(Socket, ListeningTCPPort, Name, State) ->
  Port = <<ListeningTCPPort:16/little>>,
  TypeBin = list_to_binary("MediaServer" ++ [0]),
  NameBin = list_to_binary(Name ++ [0]),
  StateBin = list_to_binary(State ++ [0]),
  MessageSize = 24 + 2 + byte_size(TypeBin) + byte_size(NameBin) + byte_size(StateBin),
  Header = <<$C, $I, $T, $P, 1:8, 0:8, 0:16, MessageSize:32/little, 1:16/little, 0:16/little,
             $P, $I, $N, $F, $P, $L, $o, $c>>,
  gen_udp:send(Socket, ?CITP_MULTICAST_IP, ?CITP_PORT, [Header, Port, TypeBin, NameBin, StateBin]).
