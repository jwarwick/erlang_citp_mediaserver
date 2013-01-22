-module(msex_protocol).
-behaviour(ranch_protocol).

-include("citp.hrl").

-export([start_link/4]).
-export([init/4]).


-define(MSEX_VERSION_MAJOR, 1).
-define(MSEX_VERSION_MINOR, 1).
-define(SERVER_NAME, "ErlMediaServer").

start_link(ListenerPid, Socket, Transport, Opts) ->
  Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]),
  {ok, Pid}.

init(ListenerPid, Socket, Transport, _Opts = []) ->
  ok = ranch:accept_ack(ListenerPid),
  {ok, Packet} = citp_msex:build_SInf(?SERVER_NAME, ?MSEX_VERSION_MAJOR, ?MSEX_VERSION_MINOR),
  %% io:format("packet: ~w~n", [Packet]),
  %% io:format("packet size: ~w~n", [iolist_size(Packet)]),
  Transport:send(Socket, Packet),
  wait_for_header(Socket, Transport).

wait_for_header(Socket, Transport) ->
  case Transport:recv(Socket, ?CITP_HEADER_SIZE, infinity) of
    {ok, Data} ->
      case citp_msex:parse_header(Data) of
        {ok, Result = {ContentType, RequestIndex, MessageSize}} ->
          wait_for_body(Socket, Transport, Result);
        Result ->
          io:format("Unknown parse_header response: ~w~n", [Result])
      end,
      wait_for_header(Socket, Transport);
    Other ->
      io:format("Got something other than data when reading header: ~w~n", [Other]),
      ok = Transport:close(Socket)
  end.

%% the LSC Clarity console sends a GELI v1.0 message with the message size=citp_header_size,
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

handle_citp_packet(Socket, Transport, {cinf, VersionMajor, VersionMinor, Count}) ->
  io:format("Got CInf packet: ~w.~w, Count:~w~n", [VersionMajor, VersionMinor, Count]),
  {ok, SInfPacket} = citp_msex:build_SInf(?SERVER_NAME, ?MSEX_VERSION_MAJOR, ?MSEX_VERSION_MINOR),
  io:format("Sending SInf packet: ~w~n", [SInfPacket]),
  ok = Transport:send(Socket, SInfPacket);
handle_citp_packet(Socket, Transport, {geli_1_0, LibraryType, 0}) ->
  io:format("Got GELI v1.0 packet, Type: ~w, All libraries~n", [LibraryType]),
  send_all_library_elements(Socket, Transport);
handle_citp_packet(Socket, Transport, {geli_1_0, LibraryType, LibraryCount}) ->
  io:format("Got GELI v1.0 packet, Type: ~w, Count: ~w~n", [LibraryType, LibraryCount]);
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
  io:format("Got GELT v1.0 packet, ThumbFormat:~w, ThumbW:~w, ThumbH:~w, ThumbFlag:~w, LibType:~w, LibCount:~w, LibNum:~w~n",
            [ThumbnailFormat, ThumbnailWidth, ThumbnailHeight, ThumbnailFlags, LibraryType, LibraryCount, LibraryNumber]);
handle_citp_packet(Socket, Transport,
                   {geth_1_0, ThumbnailFormat, ThumbnailWidth, ThumbnailHeight, ThumbnailFlags, 
                    LibraryType, LibraryNumber, ElementCount, ElementNumbers}) ->
  io:format("Got GETh v1.0 packet, ThumbFormat:~w, ThumbW:~w, ThumbH:~w, ThumbFlag:~w, LibType:~w, LibNum:~w, EltCnt:~w, EltNums:~w~n",
            [ThumbnailFormat, ThumbnailWidth, ThumbnailHeight, ThumbnailFlags, LibraryType, LibraryNumber, ElementCount, ElementNumbers]);

handle_citp_packet(_Socket, Transport, Result) ->
  io:format("Not doing anything with CITP packet: ~w~n", [Result]).

send_all_library_elements(Socket, Transport) ->
  {ok, ELInPacket} = citp_msex:build_ELIn(),
  ok = Transport:send(Socket, ELInPacket).
  
send_all_media_elements(Socket, Transport, LibraryNumber) ->
  {ok, MEInPacket} = citp_msex:build_MEIn(LibraryNumber),
  ok = Transport:send(Socket, MEInPacket).
