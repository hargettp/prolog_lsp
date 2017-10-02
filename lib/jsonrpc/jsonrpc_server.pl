:- module(jsonrpc_server,[
  start_jsonrpc_server/2,
  stop_jsonrpc_server/2

]).

:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(socket)).

:- use_module(library(log4p)).
:- use_module(library(jsonrpc/jsonrpc_server)).

:- dynamic jsonrpc_connection/4.

start_jsonrpc_server(Tag,Port) :-
  \+recorded(jsonrpc_server(Tag,Port),_,_),
  thread_create(safe_run_server(Tag,Port),ServerThreadId,[]),
  recorda(jsonrpc_server(Tag,Port),ServerThreadId).

stop_jsonrpc_server(Tag,Port) :-
  recorded(jsonrpc_server(Tag,Port),ServerThreadId,Reference),
  thread_signal(ServerThreadId,throw(exit)),
  thread_join(ServerThreadId,Status),
  info('JSON RPC server %s exited with %t',[Tag,Status]),
  erase(Reference).

safe_run_server(Tag,Port) :-
  info('Started JSON RPC server %w on %w',[Tag,Port]),
  catch(
    setup_call_cleanup(
      setup_server(Port,Socket),
      run_server(Tag,Port,Socket),
      cleanup_server(Tag,Port,Socket)
      ),
    Exception,
    warn('Exited JSON RPC server %w: %w',[Tag,Exception])).

setup_server(Port,Socket) :-
  tcp_socket(Socket),
  tcp_bind(Socket, Port).

run_server(Tag,Port,Socket) :-
  tcp_listen(Socket, 5),
  tcp_open_socket(Socket, AcceptFd, _),
  dispatch_connections(Tag,Port,AcceptFd).

% TODO if there are active connections from clients, they remain open
% even though the server is shutting down
cleanup_server(Tag,Port,Socket) :-
  catch(
    tcp_close_socket(Socket),
    _,
    info('Stopped JSON RPC server %w on %w',[Tag,Port])),
  findall(ConnectionThreadId,jsonrpc_connection(Tag,Port,_,ConnectionThreadId),ConnectionThreadIds),
  forall(member(ThreadId,ConnectionThreadIds),thread_signal(ThreadId,throw(exit))).

dispatch_connections(Tag,Port,Server) :-
  tcp_accept(Server, Client, Peer),
  thread_create(safe_handle_connection(Tag,Port,Client, Peer), _,
                [ detached(true)
                ]),
  dispatch_connections(Tag,Port,Server).

safe_handle_connection(Tag, Port, Socket, Peer) :-
  setup_call_cleanup(
    setup_connection(Tag, Port, Socket, Peer, StreamPair),
    catch(
      handle_connection(Peer, StreamPair),
      exit,
      info('Exiting connection from %w to %w on %w',[Peer,Tag,Port])),
    cleanup_connection(Tag, Port, Peer, StreamPair)).

setup_connection(Tag, Port, Socket, Peer, StreamPair) :-
  % Note there is still a chance of races, but this hopefully helps with cleanup of connections
  thread_self(ThreadId),
  \+ jsonrpc_connection(Tag, Port, Peer,ThreadId),
  asserta(jsonrpc_connection(Tag, Port, Peer,ThreadId)),
  tcp_open_socket(Socket, StreamPair).

handle_connection(Peer,StreamPair) :-
  stream_pair(StreamPair,In,Out),
  read_message(In,Message),
  handle_message(Peer,Out,Message),
  handle_connection(Peer,StreamPair).

handle_message(Peer,Out,Message) :-
  info('Received message from %w: %w',[Peer,Message]),
  is_list(Message)
    -> handle_batch(Peer, Out, Message)
    ; handle_notification_or_request(Peer, Out, Message).

handle_batch(Peer,Out,[Message|Rest]) :-
  handle_notification_or_request(Peer, Out, Message),
  handle_batch(Peer, Out, Rest).

handle_batch(_, _ , []).

handle_notification_or_request(Peer, Out, Message) :-
  Message.get(id) = _Id
    -> handle_request(Peer,Out,Message)
    ; handle_notification(Peer,Out,Message).

handle_notification(_Peer, _Out, _Request).

handle_request(_Peer, Out, _Request) :-
  Response = ok,
  write_response(Out,Response).

cleanup_connection(Tag, Port, Peer, StreamPair) :-
  close(StreamPair),
  thread_self(ThreadId),
  retractall(jsonrpc_connection(Tag, Port, Peer, ThreadId)),
  info('Closed connection to %w',[Peer]).

read_message(In,Request) :-
  json_read_dict(In,Request).

write_response(Out,Response) :-
  json_write(Out,Response),
  write(Out,'\n'),
  flush_output(Out).
