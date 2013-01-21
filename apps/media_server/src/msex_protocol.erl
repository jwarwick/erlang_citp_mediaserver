-module(msex_protocol).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/4]).

-define(CITP_HEADER_SIZE, 20).

start_link(ListenerPid, Socket, Transport, Opts) ->
  Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]),
  {ok, Pid}.

init(ListenerPid, Socket, Transport, _Opts = []) ->
  ok = ranch:accept_ack(ListenerPid),
  {ok, Packet} = citp_msex:build_SInf(),
  %% io:format("packet: ~w~n", [Packet]),
  %% io:format("packet size: ~w~n", [iolist_size(Packet)]),
  Transport:send(Socket, Packet),
  wait_for_header(Socket, Transport).

wait_for_header(Socket, Transport) ->
  io:format("wait for header~n"),
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

wait_for_body(Socket, Transport, {_ContentType, _RequestIndex, ?CITP_HEADER_SIZE}) ->
  io:format("CITP Message Size == CITP Header Size~n"),
  wait_for_header(Socket, Transport);
wait_for_body(Socket, Transport, {ContentType, RequestIndex, MessageSize}) ->
  io:format("wait for body: ~p, ~w, ~w~n", [ContentType, RequestIndex, MessageSize]),
  io:format("waiting for ~w bytes~n", [MessageSize - ?CITP_HEADER_SIZE]),
  case Transport:recv(Socket, MessageSize - ?CITP_HEADER_SIZE, infinity) of
    {ok, Data} ->
      io:format("got body data~n"),
      case citp_msex:parse_body(ContentType, Data) of
        {ok, Result} ->
          io:format("Got body packet: ~w~n", [Result]);
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

