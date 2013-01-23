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
  ProductNameBin = ucs2(ProductName),
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
  MessageSize = ?CITP_HEADER_SIZE + 6 + 4,
  Header = <<"CITP", 1:8, 0:8, 0:16, MessageSize:32/little, 1:16/little, 0:16/little,
             "MSEX", 1:8, 0:8, "Nack">>,
  {ok, [Header, MessageType]}.

build_ELIn() ->
  LibraryCount = 3,
  ElementList = [build_ELIn_element(Idx) || Idx <- lists:seq(0, LibraryCount-1)],
  ElementListBin = list_to_binary(lists:flatten(ElementList)),
  MessageSize = ?CITP_HEADER_SIZE + 6 + 1 + 1 + byte_size(ElementListBin),
  Header = <<"CITP", 1:8, 0:8, 0:16, MessageSize:32/little, 1:16/little, 0:16/little,
             "MSEX", 1:8, 0:8, "ELIn">>,
  {ok, [Header, ?MEDIA_ELEMENT_TYPE, LibraryCount, ElementListBin]}.

build_ELIn_element(Idx) ->
  Number = Idx,
  DmxRangeMin = Idx,
  DmxRangeMax = Idx,
  NameList = io_lib:fwrite("Folder ~B", [Idx+1]),
  Name = ucs2(NameList),
  ElementCount = 2,
  [Number, DmxRangeMin, DmxRangeMax, Name, ElementCount].

build_MEIn(LibraryNumber) ->
  ElementCount = LibraryNumber + 1,
  MediaInfoList = [build_MEIn_element(Idx) || Idx <- lists:seq(0, ElementCount-1)],
  MediaListBin = list_to_binary(lists:flatten(MediaInfoList)),
  MessageSize = ?CITP_HEADER_SIZE + 6 + 1 + 1 + byte_size(MediaListBin),
  Header = <<"CITP", 1:8, 0:8, 0:16, MessageSize:32/little, 1:16/little, 0:16/little,
             "MSEX", 1:8, 0:8, "MEIn">>,
  {ok, [Header, LibraryNumber, ElementCount, MediaListBin]}.

build_MEIn_element(Idx) ->
  Number = Idx,
  DmxRangeMin = Idx,
  DmxRangeMax = Idx,
  NameList = io_lib:fwrite("Media ~B", [Idx+1]),
  Name = ucs2(NameList),
  TimestampBin = <<0:64>>,
  MediaWithBin = <<1024:16>>,
  MediaHeightBin = <<768:16>>,
  MediaLengthBin = <<66:32>>,
  MediaFPS = 30,
  [Number, DmxRangeMin, DmxRangeMax, Name, TimestampBin, 
   MediaWithBin, MediaHeightBin, MediaLengthBin, MediaFPS].
  

build_EThn(ThumbnailFormat, ThumbnailWidth, ThumbnailHeight, ThumbnailFlags, 
           LibraryType, LibraryNumber, Element) ->
  {ok, Buffer} = file:read_file("/Users/jwarwick/Desktop/td.jpg"),
  BufferSize = byte_size(Buffer),
  WidthBin = <<ThumbnailWidth:16/little>>,
  HeightBin = <<ThumbnailHeight:16/little>>,
  SizeBin = <<BufferSize:16/little>>,
  MessageSize = ?CITP_HEADER_SIZE + 6 + 13 + BufferSize,
  Header = <<"CITP", 1:8, 0:8, 0:16, MessageSize:32/little, 1:16/little, 0:16/little,
             "MSEX", 1:8, 0:8, "EThn">>,
  {ok, [Header, LibraryType, LibraryNumber, Element, 
        ThumbnailFormat, WidthBin, HeightBin, SizeBin, Buffer]}.

ucs2(String) ->
  List16 = [[V, 0] || V <- String],
  list_to_binary(List16 ++ [0, 0]).

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
% Universe Name
parse_body("SDMX", <<"UNam", UniverseIndex:8, UniverseNames/binary>>) ->
  NameList = binary_to_list(UniverseNames),
  Name = lists:takewhile(fun(X) -> X /= 0 end, NameList),
  {ok, {unam, UniverseIndex, Name}};
%
% Channel Block Message
parse_body("SDMX", <<"ChBk", Blind:8, UniverseIndex:8, 
                     FirstChannel:16/little, ChannelCount:16/little, Channels/binary>>) ->
  {ok, {chbk, Blind, UniverseIndex, FirstChannel, ChannelCount, Channels}};
%
% Client Information
parse_body("MSEX", <<VersionMajor:8, VersionMinor:8, "CInf", Count:8, SupportedList/binary>>) ->
  Supported = binary_to_list(SupportedList),
  {ok, {cinf, VersionMajor, VersionMinor, Count, Supported}};
%
% Get Element Library Information v1.0
parse_body("MSEX", <<1:8, 0:8, "GELI", LibraryType:8, LibraryCount:8, LibraryNumbers/binary>>) ->
  LibraryList = binary_to_list(LibraryNumbers),
  {ok, {geli_1_0, LibraryType, LibraryCount, LibraryList}};
%
% Get Element Library Information v1.1
parse_body("MSEX", <<1:8, 1:8, "GELI", LibraryType:8, Level:8, Level1:8, Level2:8, Level3:8, 
                         LibraryCount:8, LibraryNumbers/binary>>) ->
  {ok, {geli_1_1, LibraryType, Level, Level1, Level2, Level3, LibraryCount}};
%
% Get Element Information v1.0
parse_body("MSEX", <<1:8, 0:8, "GEIn", LibraryType:8, LibraryNumber:8, 
                     ElementCount:8, ElementNumbers/binary>>) ->
  ElementList = binary_to_list(ElementNumbers),
  {ok, {gein_1_0, LibraryType, LibraryNumber, ElementCount, ElementList}};
%
% Get Element Library Thumbnail v1.0
parse_body("MSEX", <<1:8, 0:8, "GELT", ThumbnailFormat:32/little, ThumbnailWidth:16/little, ThumbnailHeight:16/little,
                     ThumbnailFlags:8, LibraryType:8, LibraryCount:8, LibraryNumber/binary>>) ->
  FormatString = binary_to_list(<<ThumbnailFormat:32/little>>),
  LibraryList = binary_to_list(LibraryNumber),
  {ok, {gelt_1_0, FormatString, ThumbnailWidth, ThumbnailHeight, ThumbnailFlags, 
        LibraryType, LibraryCount, LibraryList}};
%
% Get Element Thumbnail v1.0
parse_body("MSEX", <<1:8, 0:8, "GETh", ThumbnailFormat:32/little, ThumbnailWidth:16/little, ThumbnailHeight:16/little,
                     ThumbnailFlags:8, LibraryType:8, LibraryNumber:8, ElementCount:8, ElementNumbers/binary>>) ->
  FormatString = binary_to_list(<<ThumbnailFormat:32/little>>),
  ElementList = binary_to_list(ElementNumbers),
  {ok, {geth_1_0, FormatString, ThumbnailWidth, ThumbnailHeight, ThumbnailFlags, 
        LibraryType, LibraryNumber, ElementCount, ElementList}};
%
% Unmatched content handler
parse_body(ContentType, Data) ->
  io:format("Unknown CITP packet: ~p: ~w~n", [ContentType, Data]),
  {error, {unknown_citp_packet, ContentType}}.

