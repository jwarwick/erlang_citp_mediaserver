-module(citp_msex).
-include("citp.hrl").
% -export([]).
-compile([export_all]).

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


sendPLoc(Socket, ListeningTCPPort, Name, State) ->
  Port = <<ListeningTCPPort:16/little>>,
  TypeBin = list_to_binary("MediaServer" ++ [0]),
  NameBin = list_to_binary(Name ++ [0]),
  StateBin = list_to_binary(State ++ [0]),
  MessageSize = 24 + 2 + byte_size(TypeBin) + byte_size(NameBin) + byte_size(StateBin),
  Header = <<"CITP", 1:8, 0:8, 0:16, MessageSize:32/little, 1:16/little, 0:16/little,
             "PINF", "PLoc">>,
  gen_udp:send(Socket, ?CITP_MULTICAST_IP, ?CITP_PORT, [Header, Port, TypeBin, NameBin, StateBin]).

%% Sending 1.0 in Header as MSEX version... 
build_SInf(ProductName, VersionMajor, VersionMinor) ->
  ProductNameBin = <<"ErlMediaServer"/utf16-little, 0:16>>,
  VersionMajorBin = <<VersionMajor:8>>,
  VersionMinorBin = <<VersionMinor:8>>,
  LayerCount = <<1:8>>,
  %% DMXSourceBin = list_to_binary("BSRE1.31/0/1" ++ [0]),
  DMXSourceBin = list_to_binary("ArtNet/0/0/1" ++ [0]),
  MessageSize = 26 + byte_size(ProductNameBin) + 3 + byte_size(DMXSourceBin),
  Header = <<"CITP", 1:8, 0:8, 0:16, MessageSize:32/little, 1:16/little, 0:16/little,
             "MSEX", 1:8, 0:8, "SInf">>,
  {ok, [Header, ProductNameBin, VersionMajorBin, VersionMinorBin, LayerCount, DMXSourceBin]}.

build_Nack(MessageType) ->
  MessageSize = ?CITP_HEADER_SIZE + 14,
  Header = <<"CITP", 1:8, 0:8, 0:16, MessageSize:32/little, 1:16/little, 0:16/little,
             "MSEX", 1:8, 0:8, "Nack">>,
  {ok, [Header, MessageType]}.


parse_header(<<"CITP", _VersionMajor, _VersionMinor, RequestIndex:16/little,
  MessageSize:32/little, _MessagePartCount:16/little, _MessagePart:16/little,
  ContentType:32/little, _Data/binary>>) ->
  %% io:format("Got CITP packet: ~p~n", [binary_to_list(<<ContentType:32/little>>)]),
  {ok, {binary_to_list(<<ContentType:32/little>>), RequestIndex, MessageSize}};

parse_header(_Data) ->
  {error, {not_citp}}.


%
% Peer Location
parse_body("PINF", <<"PLoc", ListeningPort:16/little, Strings/binary>>) ->
  StringList = binary_to_list(Strings),
  Type = lists:takewhile(fun(X) -> X /= 0 end, StringList),
  [_ | StringRest] = lists:dropwhile(fun(X) -> X /=0 end, StringList),
  Name = lists:takewhile(fun(X) -> X /= 0 end, StringRest),
  [_ | State0] = lists:dropwhile(fun(X) -> X /=0 end, StringRest),
  State = lists:filter(fun(X) -> X /= 0 end, State0),
  {ok, {ploc, ListeningPort, Type, Name, State}};
%
% Peer Name
parse_body("PINF", <<"PNam", Strings/binary>>) ->
  StringList = binary_to_list(Strings),
  Name = lists:takewhile(fun(X) -> X /= 0 end, StringList),
  {ok, {pnam, Name}};
%
% Client Information
parse_body("MSEX", <<VersionMajor:8, VersionMinor:8, "CInf", Count:8, SupportedList/binary>>) ->
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

