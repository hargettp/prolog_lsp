:- module(jsonrpc_server,[
  handle_connection/3,

  echo/3,
  crash/3

]).

:- use_module(library(socket)).

:- use_module(library(log4p)).
:- use_module('./protocol').
:- reexport('./methods').
:- reexport('./errors').

% jsonrpc_server(Server, Port, ServerThreadId)
:- dynamic jsonrpc_server/3.

% jsonrpc_connection(Server, Port, Peer, ServerThreadId)
:- dynamic jsonrpc_connection/4.

handle_connection(Server, Peer, StreamPair) :-
  debug('handling connection for %w at %w',[Peer, Server]),
  stream_pair(StreamPair,In,Out),
  ( read_message(In, Message) ->
    ( 
      debug('Received request: ~%w', [Message]),
      handle_message(Server, Peer,Out,Message)
     ) ;
    parse_error(Out) ),
  handle_connection(Server, Peer,StreamPair).

handle_message(_, _Peer, Out, Message) :-
  info('handlng message %w',[Message]),
  Message.get(jsonrpc) = "2.0" ->
    fail ;
    invalid_request(Out).

handle_message(Server, Peer,Out,Message) :-
  is_list(Message)
    -> handle_batch(Server, Peer, Out, Message)
    ; handle_notification_or_request(Server, Peer, Out, Message).

handle_batch(Server, Peer,Out,[Message|Rest]) :-
  handle_notification_or_request(Server, Peer, Out, Message),
  handle_batch(Server, Peer, Out, Rest).

handle_batch(_, _, _ , []).

handle_notification_or_request(Server, Peer, Out, Message) :-
  Message.get(id) = _Id
    -> handle_request(Server, Peer,Out,Message)
    ; handle_notification(Server, Peer,Out,Message).

handle_notification(Server, _Peer, _Out, Request) :-
  dispatch_method(Server, Request.method, Request.params, _Result).

handle_request(Server, _Peer, Out, Request) :-
  catch(
      dispatch_method(Server, Request.id, Request.method, Request.params, Response),
      Exception,
      dispatch_exception(Server,Request,Exception,Response)),
  write_message(Out,Response),
  debug('Sent response: %w', [Response]),
  !.

handle_request(_Server, _Peer, Out, _Request) :-
  invalid_request(Out).

dispatch_method(Server, Id, MethodName, Params, Response) :-
  find_handler(Server, MethodName, Module:Handler),
  debug('found handler %w:%w for %w',[Module, Handler, Server]),
  apply(Module:Handler,[Server, Result, Params]),
  Response = _{id: Id, result: Result }.

dispatch_method(Server, MethodName, Params, Response) :-
  find_handler(Server, MethodName, Module:Handler),
  apply(Module:Handler,[Server, Result,Params]),
  Response = _{result: Result }.

% simple method to echo parameters; good for testing
echo(_Server, Params, Params) :-
  info('Echoing %w', [Params]).

crash(_Server, Params,Params) :-
  warn("Intentionally crashing"),
  throw(crash).
