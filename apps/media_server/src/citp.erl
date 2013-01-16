-module(citp).
% -export([]).
-compile([export_all]).

-define(CITP_PORT, 4809).
-define(CITP_MULTICAST_IP, {224, 0, 0, 180}).


start(SupplyTable) ->
  spawn(?MODULE, init, [SupplyTable]).
  
init(SupplyTable) ->
  {ok, Socket} = listen(),
  recv_loop(Socket, SupplyTable, 0, 0).

listen() ->
  {ok, Socket} = gen_udp:open(?CITP_PORT, [binary, {active, once}, {broadcast, true}, {reuseaddr, true},
      {recbuf, 128000}, {read_packets, 256}, {multicast_loop, false}]),
  {ok, IfList} = inet:getif(),
  IpList = [{Socket, Ip} || {Ip = {A,B,C,D}, _Broadcast, _Subnet} <- IfList, A=/= 127, B=/=0, C=/=0, D=/=1],
  lists:foreach(fun(C) -> multicast_subscribe(C) end, IpList),
  {ok, Socket}.

multicast_subscribe({Socket, IpAddress}) ->
  ok = inet:setopts(Socket, [{add_membership, {?CITP_MULTICAST_IP, IpAddress}}]),
  io:format("Subscribing ~w to multicast ~w~n", [IpAddress, ?CITP_MULTICAST_IP]).


recv_loop(Socket, SupplyTable, ChildPid, PacketCount) ->
  receive
    {udp, Socket, IP, _InPortNo, Packet} ->
      Result = parseHeader(Packet),
      case Result of
        {ploc, ListeningPort, Type, Name, State} ->
          io:format("PINF: ~w:~w, ~p, ~p, ~p~n", [IP, ListeningPort, Type, Name, State]),
          close(Socket),
          Child2 = spawnConnection(IP, ListeningPort, Type, Name, State, SupplyTable),
          recv_loop(Socket, SupplyTable, Child2, PacketCount+1);
        _ -> true
      end,
      recv_loop(Socket, SupplyTable, ChildPid, PacketCount+1);

    {portout, Universe, Data} -> 
      if
        is_pid(ChildPid) ->
          ChildPid ! {portout, Universe, Data};
        true ->
          true
      end,
      recv_loop(Socket, SupplyTable, ChildPid, PacketCount);

    stop -> io:format("CITP exiting~n"),
      if
        is_pid(ChildPid) ->
          ChildPid ! stop;
        true ->
          true
      end,
      close(Socket);

    stats -> io:format("CITP: saw ~w packets~n", [PacketCount]),
      io:format("CITP stats: ~w~n", [inet:getstat(Socket)]),
      recv_loop(Socket, SupplyTable, 0, PacketCount);

    {'EXIT', From, stopped} ->
      io:format("Child died: stopped~n");
    
    {'EXIT', From, Reason} ->
      io:format("Child died: ~w~n", [Reason]),
      {ok, S2} = listen(),
      recv_loop(S2, SupplyTable, ChildPid, 0);


    Other -> io:format("CITP got unknown message: ~w~n", [Other]),
      recv_loop(Socket, SupplyTable, ChildPid, PacketCount)
  end.


close(Socket) ->
  gen_udp:close(Socket).
  
parseHeader(<<$C, $I, $T, $P, VersionMajor, VersionMinor, RequestIndex:16/little,
  MessageSize:32/little, MessagePartCount:16/little, MessagePart:16/little,
  ContentType:32/little, Data/binary>>) ->
  %io:format("Got CITP packet: ~p~n", [binary_to_list(<<ContentType:32/little>>)]),
  parseBody(<<ContentType:32/little>>, Data).

parseBody(<<$P, $I, $N, $F>>, <<$P, $L, $o, $c, ListeningPort:16/little, Strings/binary>>) ->
  %io:format("Got PLoc packet: port ~w~n", [ListeningPort]),
  StringList = binary_to_list(Strings),
  Type = lists:takewhile(fun(X) -> X /= 0 end, StringList),
  [_ | StringRest] = lists:dropwhile(fun(X) -> X /=0 end, StringList),
  Name = lists:takewhile(fun(X) -> X /= 0 end, StringRest),
  [_ | State0] = lists:dropwhile(fun(X) -> X /=0 end, StringRest),
  State = lists:filter(fun(X) -> X /= 0 end, State0),
  %io:format("Type = ~p, Name = ~p, State: ~p~n", [Type, Name, State]),
  {ploc, ListeningPort, Type, Name, State}.

spawnConnection(IP, ListeningPort, _Type, _Name, _State, SupplyTable) ->
  process_flag(trap_exit, true),
  spawn_link(?MODULE, connectToCapture, [IP, ListeningPort, SupplyTable]).

connectToCapture(IP, ListeningPort, SupplyTable) ->
  {ok, Socket} = gen_tcp:connect(IP, ListeningPort, [binary, {packet, 0}, {active, true}, {sndbuf, 128000}]),
  sendName(Socket),
  sendUniverses(Socket, SupplyTable),
  dataLoop(Socket, SupplyTable).

sendName(Socket) ->
  NameStr = "sACN Bridge",
  Name = list_to_binary(NameStr ++ [0]),
  io:format("Sending Controller Name: ~p~n", [NameStr]),
  MessageSize = byte_size(Name) + 24,
  Header = <<$C, $I, $T, $P, 1:8, 0:8, 0:16, MessageSize:32/little, 1:16/little, 0:16/little,
             $P, $I, $N, $F, $P, $N, $a, $m>>,
  gen_tcp:send(Socket, [Header, Name]).

sendUniverses(Socket, SupplyTable) ->
  SupplyList = ets:match(SupplyTable, '$1'),
  UniverseList = [{Socket, Name, Universe} || [{Name, Universe, _IP, _Port, _KinetVersion}] <- SupplyList],
  lists:foreach(fun(X) -> sendUniverse(X) end, UniverseList).

sendUniverse({Socket, Name, Universe}) ->
  NameBin = list_to_binary(Name ++ [0]),
  UniverseIndex = <<Universe>>,
  io:format("Sending Universe Name: ~p~n", [Name]),
  MessageSize = byte_size(NameBin) + 1 + 24,
  Header = <<$C, $I, $T, $P, 1:8, 0:8, 0:16, MessageSize:32/little, 1:16/little, 0:16/little,
             $S, $D, $M, $X, $U, $N, $a, $m>>,
  gen_tcp:send(Socket, [Header, UniverseIndex, NameBin]).

sendChannelData(Socket, Universe, Data) ->
  %io:format("Sending Universe ~w : ~w~n", [Universe, Data]),
  DataSize = byte_size(Data),
  ChBk = <<0:8, Universe:8, 0:16/little, DataSize:16/little>>,
  MessageSize = byte_size(ChBk) + byte_size(Data) + 24,
  Header = <<$C, $I, $T, $P, 1:8, 0:8, 0:16, MessageSize:32/little, 1:16/little, 0:16/little,
             $S, $D, $M, $X, $C, $h, $B, $k>>,
  gen_tcp:send(Socket, [Header, ChBk, Data]).

dataLoop(Socket, SupplyTable) ->
  receive
    {tcp_closed, _Port} ->
      gen_tcp:close(Socket),
      io:format("TCP remote connection closed~n"),
      exit(remote_closed);

    {portout, Universe, Data} ->
      sendChannelData(Socket, Universe, Data);

    stop ->
      io:format("TCP process stopping~n"),
      gen_tcp:close(Socket),
      exit(stopped);

    Data ->
      io:format("tcp socket got data: ~w~n", [Data])
  end,
  dataLoop(Socket, SupplyTable).



