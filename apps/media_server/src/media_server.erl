-module(media_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-define(ANNOUNCE_INTERVAL, 5000). 

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
  {ok, _} = ranch:start_listener(tcp_msex, 5,
                                 ranch_tcp, [{port, 0}],
                                 msex_protocol, []
                                ),
  Port = ranch:get_port(tcp_msex),
  io:format("MSEX TCP Port: ~p~n", [Port]),
  {ok, Socket} = citp_msex:listen(),
  erlang:send_after(0, ?MODULE, announce),
  {ok, {Socket, Port}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.



handle_cast(_Msg, State) ->
  {noreply, State}.



handle_info(announce, State = {Socket, Port}) ->
  ok = citp_msex:sendPLoc(Socket, Port, "Erlang MediaServer", "Running"),
  erlang:send_after(?ANNOUNCE_INTERVAL, ?MODULE, announce),
  {noreply, State};

handle_info({udp, _Socket, IP, _InPortNo, Packet}, State) ->
  Result = citp_msex:parseHeader(Packet),
  case Result of
    {ploc, ListeningPort, Type, Name, CitpState} ->
      io:format("PINF/PLoc: ~w:~w, ~p, ~p, ~p~n", [IP, ListeningPort, Type, Name, CitpState]);
    Result ->
      io:format("Don't know that packet: ~p, ~p~n", [Result, Packet])
  end,
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.



terminate(_Reason, _State) ->
  ok.



code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

