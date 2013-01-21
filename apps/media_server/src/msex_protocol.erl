-module(msex_protocol).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/4]).

start_link(ListenerPid, Socket, Transport, Opts) ->
  io:format("in ranch start link...~n"),
  Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]),
  {ok, Pid}.

init(ListenerPid, Socket, Transport, _Opts = []) ->
  ok = ranch:accept_ack(ListenerPid),
  io:format("protocol init~n"),
  {ok, Packet} = citp_msex:build_SInf(),
  %% io:format("packet: ~w~n", [Packet]),
  %% io:format("packet size: ~w~n", [iolist_size(Packet)]),
  Transport:send(Socket, Packet),
  loop(Socket, Transport).

loop(Socket, Transport) ->
  case Transport:recv(Socket, 0, infinity) of
    {ok, Data} ->
      case citp_msex:parse_packet(Data) of
        {ok, Result} ->
          io:format("Got packet: ~w~n", [Result]);
        {error, unknown_citp_packet, ContentType} ->
          io:format("Unknown CITP packet: ~w~n", [ContentType]);
        {error, not_citp} ->
          io:format("Not a CITP packet~n");
        Data ->
          io:format("Unknown response: ~w~n", [Data])
      end,
      loop(Socket, Transport);
    Other ->
      io:format("Got something other than data: ~w~n", [Other]),
      ok = Transport:close(Socket)
  end.

