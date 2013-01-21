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
  Header = <<"CITP", 1:8, 0:8, 0:16, MessageSize:32/little, 1:16/little, 0:16/little,
             "PINF", "PLoc">>,
  gen_udp:send(Socket, ?CITP_MULTICAST_IP, ?CITP_PORT, [Header, Port, TypeBin, NameBin, StateBin]).

build_SInf() ->
  ProductNameBin = <<"ErlMedia"/utf16-little, 0:16>>,
  VersionMajor = <<1:8>>,
  VersionMinor = <<2:8>>,
  LayerCount = <<1:8>>,
  %% DMXSourceBin = list_to_binary("BSRE1.31/0/1" ++ [0]),
  DMXSourceBin = list_to_binary("ArtNet/0/0/1" ++ [0]),
  MessageSize = 26 + byte_size(ProductNameBin) + 3 + byte_size(DMXSourceBin),
  Header = <<"CITP", 1:8, 0:8, 0:16, MessageSize:32/little, 1:16/little, 0:16/little,
             "MSEX", 1:8, 0:8, "SInf">>,
  {ok, [Header, ProductNameBin, VersionMajor, VersionMinor, LayerCount, DMXSourceBin]}.


parse_header(<<"CITP", _VersionMajor, _VersionMinor, RequestIndex:16/little,
  MessageSize:32/little, _MessagePartCount:16/little, _MessagePart:16/little,
  ContentType:32/little, _Data/binary>>) ->
  io:format("Got CITP packet: ~p~n", [binary_to_list(<<ContentType:32/little>>)]),
  {ok, {binary_to_list(<<ContentType:32/little>>), RequestIndex, MessageSize}};

parse_header(_Data) ->
  {error, {not_citp}}.



parse_body("PINF", <<"PLoc", ListeningPort:16/little, Strings/binary>>) ->
  StringList = binary_to_list(Strings),
  Type = lists:takewhile(fun(X) -> X /= 0 end, StringList),
  [_ | StringRest] = lists:dropwhile(fun(X) -> X /=0 end, StringList),
  Name = lists:takewhile(fun(X) -> X /= 0 end, StringRest),
  [_ | State0] = lists:dropwhile(fun(X) -> X /=0 end, StringRest),
  State = lists:filter(fun(X) -> X /= 0 end, State0),
  {ok, {ploc, ListeningPort, Type, Name, State}};
%
% Client Information
parse_body("MSEX", <<VersionMajor:8, VersionMinor:8, "CInf", Count:8, SupportedList/binary>>) ->
  io:format("got CInf~n"),
  {ok, {cinf, VersionMajor, VersionMinor, Count}};
%
% Get Element Library Information v1.0
parse_body("MSEX", <<1:8, 0:8, "GELI", LibraryType:8, LibraryCount:8, LibraryNumbers/binary>>) ->
  {ok, {geli_1_0, LibraryType, LibraryCount}};
%
% Get Element Library Information v1.1
parse_body("MSEX", <<1:8, 1:8, "GELI", LibraryType:8, Level:8, Level1:8, Level2:8, Level3:8, 
                         LibraryCount:8, LibraryNumbers/binary>>) ->
  {ok, {geli_1_1, LibraryType, Level, Level1, Level2, Level3, LibraryCount}};
%
% Unmatched content handler
parse_body(ContentType, Data) ->
  io:format("Unknown CITP packet: ~p: ~w~n", [ContentType, Data]),
  {error, {unknown_citp_packet, ContentType}}.

