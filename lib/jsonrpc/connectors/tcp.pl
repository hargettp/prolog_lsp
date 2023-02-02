:- module(jsonrpc_connectors_tcp,[
  create_tcp_connector/3,
  start_jsonrpc_server/2,
  run_jsonrpc_server/3,
  stop_jsonrpc_server/2
  ]).

:- use_module(library(socket)).
:- use_module(library(log4p)).
:- use_module('../jsonrpc_server').

% 
% Client methods
% 

% connect_to_server(Server, Connection)
connect_to_server(ServerAddress, Connection) :-
  tcp_connect(ServerAddress,StreamPair,[]),
  Connection = connection(ServerAddress,StreamPair).

% close_connection(Connnection)
close_connection(connection(_,StreamPair)) :-
  ignore(close(StreamPair)).

% 
% Server methods
% 

create_tcp_connector(ServerName, Port, tcp_server(ServerName, Port)).

serve_messages(tcp_server(ServerName, Port)) :-
  safe_run_jsonrpc_server(ServerName, Port).

% jsonrpc_server(Server, Port, ServerThreadId)
:- dynamic jsonrpc_server/3.

% jsonrpc_connection(Server, Port, Peer, ServerThreadId)
:- dynamic jsonrpc_connection/4.

start_jsonrpc_server(ServerName, _) :-
  jsonrpc_server(ServerName, _, _),
  throw(error(server_already_started)).

start_jsonrpc_server(_, Port) :-
  jsonrpc_server(_, Port, _),
  throw(error(server_already_on_port)).

start_jsonrpc_server(ServerName,Port) :-
  create_tcp_connector(ServerName, Port, Server),
  thread_create(serve_messages(Server), ServerThreadId,[]),
  assertz(jsonrpc_server(ServerName,Port,ServerThreadId)).

stop_jsonrpc_server(ServerName,Port) :-
  jsonrpc_server(ServerName,Port,ServerThreadId),
  thread_signal(ServerThreadId,throw(exit)),
  thread_join(ServerThreadId,Status),
  info('JSON RPC server %t exited with %t',[ServerName,Status]),
  retractall(jsonrpc_server(ServerName, Port, ServerThreadId)),
  !.

stop_jsonrpc_server(_, _) :-
  throw(error(server_not_started)).

safe_run_jsonrpc_server(ServerName,Port) :-
  info('Started JSON RPC server %w on %w',[ServerName,Port]),
  catch(
    setup_call_cleanup(
      setup_server(Port,Socket),
      run_jsonrpc_server(ServerName,Port,Socket),
      cleanup_server(ServerName,Port,Socket)
      ),
    Exception,
    warn('Exited JSON RPC server %w: %w',[ServerName,Exception])).

setup_server(Port,Socket) :-
  tcp_socket(Socket),
  tcp_setopt(Socket,reuseaddr),
  tcp_bind(Socket, Port).

run_jsonrpc_server(ServerName,Port,Socket) :-
  tcp_listen(Socket, 5),
  tcp_open_socket(Socket, AcceptFd, _),
  dispatch_connections(ServerName,Port,AcceptFd).

cleanup_server(ServerName,Port,Socket) :-
  catch(
    tcp_close_socket(Socket),
    _,
    info('Stopped JSON RPC server %w on %w',[ServerName,Port])),
  findall(ConnectionThreadId,jsonrpc_connection(ServerName,Port,_,ConnectionThreadId),ConnectionThreadIds),
  forall(member(ThreadId,ConnectionThreadIds),thread_signal(ThreadId,throw(exit))).

dispatch_connections(ServerName,Port,ServerFd) :-
  tcp_accept(ServerFd, Client, Peer),
  thread_create(safe_handle_connection(ServerName,Port,Client, Peer), _,
                [ detached(true),
                  debug(false)
                ]),
  dispatch_connections(ServerName,Port,ServerFd).

safe_handle_connection(ServerName, Port, Socket, Peer) :-
  setup_call_cleanup(
    setup_connection(ServerName, Port, Socket, Peer, StreamPair),
    catch(
      handle_connection(ServerName, Peer, StreamPair),
      eof,
      info('Exiting: connection closed from %w to %w on %w',[Peer,ServerName,Port])),
    cleanup_connection(ServerName, Port, Peer, StreamPair)).

setup_connection(ServerName, Port, Socket, Peer, StreamPair) :-
  thread_self(ThreadId),
  \+ jsonrpc_connection(ServerName, Port, Peer, ThreadId),
  % Note there is still a chance of races, but this hopefully helps with cleanup 
  % of connections; deliberately using asserta here
  asserta(jsonrpc_connection(ServerName, Port, Peer,ThreadId)),
  tcp_open_socket(Socket, StreamPair).

cleanup_connection(ServerName, Port, Peer, StreamPair) :-
  close(StreamPair),
  thread_self(ThreadId),
  retractall(jsonrpc_connection(ServerName, Port, Peer, ThreadId)),
  info('Closed connection to %w',[Peer]).
