:- module(jsonrpc_server,[
  start_jsonrpc_server/2,
  stop_jsonrpc_server/1

]).

:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(socket)).

:- use_module(library(log4p)).
:- use_module(library(jsonrpc/jsonrpc_server)).

start_jsonrpc_server(Tag,Port) :-
  thread_create(safe_run_server(Tag,Port),ServerThreadId,[]),
  recorda(Tag,ServerThreadId).

stop_jsonrpc_server(Tag) :-
  recorded(Tag,ServerThreadId,Reference),
  thread_signal(ServerThreadId,throw(exit)),
  thread_join(ServerThreadId,Status),
  info('JSON RPC server %s exited with %t',[Tag,Status]),
  erase(Reference).

safe_run_server(Tag,Port) :-
  info('Started JSON RPC server %w on %w',[Tag,Port]),
  catch(
    setup_call_cleanup(
      setup_server(Port,Socket),
      run_server(Port,Socket),
      cleanup_server(Tag,Port,Socket)
      ),
    Exception,
    error('Exited JSON RPC server %s: %w',[Tag,Exception])).

setup_server(Port,Socket) :-
  tcp_socket(Socket),
  tcp_bind(Socket, Port).

run_server(Port,Socket) :-
  tcp_listen(Socket, 5),
  tcp_open_socket(Socket, AcceptFd, _),
  dispatch_connections(Port,AcceptFd).

% TODO if there are active connections from clients, they remain open
% even though the server is shutting down
cleanup_server(Tag,Port,Socket) :-
  catch(
    tcp_close_socket(Socket),
    _,
    info('Stopped JSON RPC server %w on %w',[Tag,Port])).

dispatch_connections(Port,Server) :-
  tcp_accept(Server, Client, Peer),
  thread_create(safe_handle_connection(Port,Client, Peer), _,
                [ detached(true)
                ]),
  dispatch_connections(Port,Server).

safe_handle_connection(_Port,Socket, Peer) :-
  setup_call_cleanup(
    setup_connection(Socket, StreamPair),
    catch(
      handle_connection(Peer, StreamPair),
      eof,
      info('EOF: connection closed by peer: %w',[Peer])),
    cleanup_connection(Peer, StreamPair)).

setup_connection(Socket, StreamPair) :-
  tcp_open_socket(Socket, StreamPair).

handle_connection(Peer,StreamPair) :-
  stream_pair(StreamPair,In,Out),
  read_message(In,Message),
  handle_message(Peer,Out,Message,_Response),
  handle_connection(Peer,StreamPair).

% handle_message(_,_,@end_of_file,_) :-
%   thread_exit(eof).

handle_message(Peer,Out,Request,Response) :-
  info('Received message from %w: %w',[Peer,Request]),
  Response = ok,
  write_response(Out,Response).

cleanup_connection(Peer, StreamPair) :-
  close(StreamPair),
  info('Closed connection to %w',[Peer]).

read_message(In,Request) :-
  json_read_dict(In,Request).

write_response(Out,Response) :-
  json_write(Out,Response),
  write(Out,'\n'),
  flush_output(Out).
