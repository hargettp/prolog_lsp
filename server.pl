:- module(server,[
  start_language_server/2,
  stop_language_server/1

]).

:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(socket)).

:- use_module(library(log4p)).

start_language_server(tcp,Port) :-
  thread_create(safe_run_server(Port),ServerThreadId,[]),
  recorda(prolog_language_server,ServerThreadId).

stop_language_server(tcp) :-
  recorded(prolog_language_server,ServerThreadId,Reference),
  thread_signal(ServerThreadId,throw(exit)),
  thread_join(ServerThreadId,Status),
  info('Prolog language server exited with %t',[Status]),
  erase(Reference).

safe_run_server(Port) :-
  info('Started Prolog language server on %t',[Port]),
  call_cleanup(run_server(Port),
    info('Stopped Prolog language server on %t',[Port])
    ).

run_server(Port) :-
  tcp_socket(Socket),
  tcp_bind(Socket, Port),
  tcp_listen(Socket, 5),
  tcp_open_socket(Socket, AcceptFd, _),
  dispatch_connection(AcceptFd).

dispatch_connection(Server) :-
  tcp_accept(Server, Client, Peer),
  thread_create(safe_handle_connection(Client, Peer), _,
                [ detached(true)
                ]),
  dispatch_connection(Server).

safe_handle_connection(Socket, Peer) :-
  setup_call_cleanup(
    tcp_open_socket(Socket, StreamPair),
    handle_connection(Peer, StreamPair),
    close(StreamPair)).

handle_connection(Peer,StreamPair) :-
  stream_pair(StreamPair,In,Out),
  read_request(In,Request),
  handle_request(Peer,Request,Response),
  write_response(Out,Response),
  handle_connection(Peer,StreamPair).

read_request(In,Request) :-
  json_read(In,Request).

handle_request(_,@end_of_file,_) :-
  throw(done).

handle_request(Peer,Request,Response) :-
  info('Received request from %t: %t',[Peer,Request]),
  Response = ok.

write_response(Out,Response) :-
  json_write(Out,Response),
  write(Out,'\n'),
  flush_output(Out).
