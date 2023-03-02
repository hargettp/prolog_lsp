:- module(jsonrpc_server,[
  handle_connection/3,

  echo/3,
  crash/3,

  request_exit_server/1

]).

:- use_module(library(socket)).

:- use_module(library(log4p)).
:- use_module('./protocol', except([message_json/2])).
:- reexport(protocol, [message_json/2]).
:- reexport('./methods').
:- reexport('./errors').

% jsonrpc_server(Server, Port, ServerThreadId)
:- dynamic jsonrpc_server/3.

% jsonrpc_connection(Server, Port, Peer, ServerThreadId)
:- dynamic jsonrpc_connection/4.

handle_connection(ServerName, Peer, StreamPair) :-
  mark_server_as_running(ServerName),
  handle_messages(ServerName, Peer, StreamPair).

handle_messages(ServerName, _Peer, _StreamPair) :-
  \+ is_server_running(ServerName), 
  info("Exiting server %s", [ServerName]),
  !.
  
handle_messages(ServerName, Peer, StreamPair) :-
  debug('handling connection for %w at %w',[Peer, ServerName]),
  stream_pair(StreamPair,In,Out),
  ( read_message(In, Message) ->
    ( 
      handle_message(ServerName, Peer,Out,Message)
     ) ;
    parse_error(Out) ),
    handle_messages(ServerName, Peer,StreamPair).

handle_message(_, _Peer, Out, Message) :-  
  message_json(Message, Json),
  debug('handlng message %w',[Json]),
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
    ; handle_notification(Server, Peer, Message).

handle_request(Server, _Peer, Out, Request) :-
  catch(
      dispatch_method(Server, Request.id, Request.method, Request.params, Response),
      Exception,
      dispatch_exception(Server,Request,Exception,Response)),
  write_message(Out,Response),
  message_json(Response, Json),
  debug('Sent response: %w', [Json]),
  !.

handle_request(_Server, _Peer, Out, _Request) :-
  invalid_request(Out).

handle_notification(Server, _Peer, Notification) :-
  catch(
      dispatch_notification(Server, Notification.method, Notification.params),
      Exception,
      warn("Notification %w failed: %w", [Notification.method, Exception])
    ),
  debug("Handled notification %w", [Notification.method]).

dispatch_method(Server, Id, MethodName, Params, Response) :-
  find_handler(Server, MethodName, Module:Handler),
  debug('found handler %w:%w for %w',[Module, Handler, Server]),
  apply(Module:Handler,[Server, Result, Params]),
  Response = _{id: Id, result: Result },
  info("Method %w called", [MethodName]).

dispatch_method(Server, Id, MethodName, _Params, _Response) :-
  warn("%w: Failed dispatchingr request %w method %w",[Server, Id, MethodName]).

dispatch_notification(Server, MethodName, Params) :-
  find_handler(Server, MethodName, Module:Handler),
  apply(Module:Handler,[Server, Params]),
  info("Notification %w called", [MethodName]).

dispatch_notification(Server, MethodName, _Params) :-
  warn("%w: Failed dispatchingr notification %w",[Server, MethodName]).

% simple method to echo parameters; good for testing
echo(_Server, Params, Params) :-
  info('Echoing %w', [Params]).

crash(_Server, Params,Params) :-
  warn("Intentionally crashing"),
  throw(crash).

is_server_running(ServerName) :-
  server_running_flag(ServerName, RunningFlag),
  get_flag(RunningFlag, true).

mark_server_as_running(ServerName) :-
  server_running_flag(ServerName, RunningFlag),
  set_flag(RunningFlag, true).

% Request that the indicated server stop running;
% note that this is an asynchronous request and provides
% no feedback.
request_exit_server(ServerName) :-
  server_running_flag(ServerName, RunningFlag),
  set_flag(RunningFlag, false).

server_running_flag(ServerName, RunningFlag) :-
  atom_concat(ServerName, '_running', RunningFlag).

