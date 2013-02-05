-module(msex_protocol).
-behaviour(ranch_protocol).

-include("citp.hrl").

-export([start_link/4]).
-export([init/4]).


-define(MSEX_VERSION_MAJOR, 1).
-define(MSEX_VERSION_MINOR, 1).

start_link(ListenerPid, Socket, Transport, Opts) ->
  Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]),
  {ok, Pid}.

init(ListenerPid, Socket, Transport, _Opts = []) ->
  ok = ranch:accept_ack(ListenerPid),
  % send Peer Name on connect
  io:format("Sending PNam~n"),
  {ok, PNam} = citp_msex:build_PNam(content:server_name()),
  ok = Transport:send(Socket, PNam),
  % send Server Info on connect
  io:format("Sending SInf~n"),
  {ok, Packet} = citp_msex:build_SInf(content:server_name(), ?MSEX_VERSION_MAJOR, ?MSEX_VERSION_MINOR),
  Transport:send(Socket, Packet),
  wait_for_header(Socket, Transport).

wait_for_header(Socket, Transport) ->
  case Transport:recv(Socket, ?CITP_HEADER_SIZE, 250) of
    {ok, Data} ->
      case citp_msex:parse_header(Data) of
        {ok, Result = {ContentType, RequestIndex, MessageSize}} ->
          wait_for_body(Socket, Transport, Result);
        Result ->
          io:format("Unknown parse_header response: ~w~n", [Result])
      end,
      wait_for_header(Socket, Transport);
    {error, timeout} ->
      {ok, Packet} = citp_msex:build_LSta(),
      %% io:format("Sending LSta msg~n"),
      ok = Transport:send(Socket, Packet),
      wait_for_header(Socket, Transport);
    Other ->
      io:format("Got something other than data when reading header: ~w~n", [Other]),
      ok = Transport:close(Socket)
  end.

%% the LSC Clarity console sends messages with the message_size=citp_header_size,
%% so MessageSize = 0, which will read whatever data is available, which might not be the whole packet...
wait_for_body(Socket, Transport, {ContentType, RequestIndex, MessageSize}) ->
  %% io:format("wait for body: ~p, ~w, ~w~n", [ContentType, RequestIndex, MessageSize]),
  %% io:format("waiting for ~w bytes~n", [MessageSize - ?CITP_HEADER_SIZE]),
  case Transport:recv(Socket, MessageSize - ?CITP_HEADER_SIZE, infinity) of
    {ok, Data} ->
      case citp_msex:parse_body(ContentType, Data) of
        {ok, Result} ->
          handle_citp_packet(Socket, Transport, Result);
        {error, {unknown_citp_packet, ContentType}} ->
          io:format("Unknown CITP packet: ~p~n", [ContentType]);
        {error, {not_citp}} ->
          io:format("Not a CITP packet~n");
        Unexpected ->
          io:format("Got unexpected result: ~w~n", [Unexpected])
      end,
      wait_for_header(Socket, Transport);
    Other ->
      io:format("Got something other than data when reading packet body: ~w~n", [Other])
  end.

handle_citp_packet(_Socket, _Transport, {pnam, Name}) ->
  io:format("Got PNam: ~p~n", [Name]);
handle_citp_packet(_Socket, _Transport, {unam, UniverseIndex, UniverseName}) ->
  io:format("Got UNam packet: ~p, Index:~w~n", [UniverseName, UniverseIndex]);
handle_citp_packet(_Socket, _Transport, {chbk, Blind, UniverseIndex, FirstChannel, ChannelCount, _Channels}) ->
  io:format("Got ChBk: UniverseIndex: ~w, FirstChannel: ~w, (Blind: ~w) (ChannelCount: ~w)~n", [UniverseIndex, FirstChannel, Blind, ChannelCount]);
handle_citp_packet(Socket, Transport, {cinf, VersionMajor, VersionMinor, Count, SupportedList}) ->
  io:format("Got CInf packet: ~w.~w, Count:~w, Supported:~w~n", [VersionMajor, VersionMinor, Count, SupportedList]),
  {ok, SInfPacket} = citp_msex:build_SInf(content:server_name(), ?MSEX_VERSION_MAJOR, ?MSEX_VERSION_MINOR),
  io:format("Sending SInf packet~n"),
  ok = Transport:send(Socket, SInfPacket);
handle_citp_packet(Socket, Transport, {geli_1_0, LibraryType, 0, LibraryNumbers}) ->
  io:format("Got GELI v1.0 packet, Type: ~w, All libraries~n", [LibraryType]),
  send_all_library_elements(Socket, Transport);
handle_citp_packet(Socket, Transport, {geli_1_0, LibraryType, LibraryCount, LibraryNumbers}) ->
  io:format("Got GELI v1.0 packet, Type: ~w, Count: ~w, Numbers: ~w~n", [LibraryType, LibraryCount, LibraryNumbers]);
handle_citp_packet(Socket, Transport, 
                   {gein_1_0, LibraryType, LibraryNumber, 0, ElementNumbers}) ->
  io:format("Got GEIn v1.0 packet, LibType:~w, LibNum:~w, All libraries~n",
            [LibraryType, LibraryNumber]),
  send_all_media_elements(Socket, Transport, LibraryNumber);
handle_citp_packet(Socket, Transport, 
                   {gein_1_0, LibraryType, LibraryNumber, ElementCount, ElementNumbers}) ->
  io:format("Got GEIn v1.0 packet, LibType:~w, LibNum:~w, EltCount:~w, EltNumbers:~w~n",
            [LibraryType, LibraryNumber, ElementCount, ElementNumbers]);
handle_citp_packet(Socket, Transport,
                   {gelt_1_0, ThumbnailFormat, ThumbnailWidth, ThumbnailHeight, ThumbnailFlags, 
                    LibraryType, LibraryCount, LibraryNumber}) ->
  io:format("Got GELT v1.0 packet, ThumbFormat:~p, ThumbW:~w, ThumbH:~w, ThumbFlag:~w, LibType:~w, LibCount:~w, LibNum:~w~n",
            [ThumbnailFormat, ThumbnailWidth, ThumbnailHeight, ThumbnailFlags, LibraryType, LibraryCount, LibraryNumber]);
handle_citp_packet(Socket, Transport,
                   {geth_1_0, ThumbnailFormat, ThumbnailWidth, ThumbnailHeight, ThumbnailFlags, 
                    LibraryType, LibraryNumber, ElementCount, ElementNumbers}) ->
  io:format("Got GETh v1.0 packet, ThumbFormat:~p, ThumbW:~w, ThumbH:~w, ThumbFlag:~w, LibType:~w, LibNum:~w, EltCnt:~w, EltNums:~w~n",
            [ThumbnailFormat, ThumbnailWidth, ThumbnailHeight, ThumbnailFlags, LibraryType, LibraryNumber, ElementCount, ElementNumbers]),
  send_thumbnails(Socket, Transport, ThumbnailFormat, ThumbnailWidth, ThumbnailHeight, ThumbnailFlags, 
                  LibraryType, LibraryNumber, ElementCount, ElementNumbers);
handle_citp_packet(Socket, Transport, {gvsr}) ->
  io:format("Got GVSr packet~n"),
  io:format("Sending Nack~n"),
  {ok, Nack} = citp_msex:build_Nack("GVSr"),
  ok = Transport:send(Socket, Nack);
handle_citp_packet(_Socket, _Transport, Result) ->
  io:format("Not doing anything with CITP packet: ~w~n", [Result]).

send_all_library_elements(Socket, Transport) ->
  io:format("Sending ELIn packet~n"),
  {ok, Libs} = content:get_all_element_libraries(),
  {ok, ELInPacket} = citp_msex:build_ELIn(Libs),
  ok = Transport:send(Socket, ELInPacket).
  
send_all_media_elements(Socket, Transport, LibraryNumber) ->
  io:format("Sending MEIn packet~n"),
  {ok, Shows} = content:get_all_elements(LibraryNumber),
  {ok, MEInPacket} = citp_msex:build_MEIn(LibraryNumber, Shows),
  ok = Transport:send(Socket, MEInPacket).

send_thumbnails(Socket, Transport, 
                ThumbnailFormat, ThumbnailWidth, ThumbnailHeight, ThumbnailFlags, 
                LibraryType, LibraryNumber, _ElementCount, ElementNumbers) ->
  [send_thumbnail(Socket, Transport, 
                  ThumbnailFormat, ThumbnailWidth, ThumbnailHeight, ThumbnailFlags, 
                  LibraryType, LibraryNumber, Element) ||  Element <- ElementNumbers].

send_thumbnail(Socket, Transport, 
               ThumbnailFormat, ThumbnailWidth, ThumbnailHeight, ThumbnailFlags, 
               LibraryType, LibraryNumber, Element) ->
  io:format("Sending EThn packet~n"),
  {ok, ThumbData} = content:get_thumbnail(ThumbnailFormat, ThumbnailWidth, ThumbnailHeight, ThumbnailFlags, LibraryNumber, Element),
  {ok, EThnPacket} = citp_msex:build_EThn(ThumbData, ThumbnailFormat, ThumbnailWidth, ThumbnailHeight, ThumbnailFlags, 
                                          LibraryType, LibraryNumber, Element),
  ok = Transport:send(Socket, EThnPacket).

