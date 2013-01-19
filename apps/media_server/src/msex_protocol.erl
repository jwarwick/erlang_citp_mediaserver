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
  io:format("protocol init"),
  loop(Socket, Transport).

loop(Socket, Transport) ->
  io:format("protocol loop"),
  case Transport:recv(Socket, 2, infinity) of
    {ok, Data} ->
      %% Transport:send(Socket, Data),
      io:format("Got data: ~w~n", [Data]),
      loop(Socket, Transport);
    _ ->
      io:format("Got something other than data...."),
      ok = Transport:close(Socket)
  end.
