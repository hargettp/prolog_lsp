:- module(jsonrpc_server,[
  handle_connection/3,

  server_method/3,
  server_error/3,
  echo/2,
  crash/2

]).

:- use_module(library(socket)).

:- use_module(library(log4p)).
:- use_module('./jsonrpc_protocol').

:- meta_predicate
  server_method(:,:,:),
  declared_server_method(:,:,:),
  server_error(:,:,:),
  declared_server_error(:,:,:).

% jsonrpc_server(Server, Port, ServerThreadId)
:- dynamic jsonrpc_server/3.

% jsonrpc_connection(Server, Port, Peer, ServerThreadId)
:- dynamic jsonrpc_connection/4.

handle_connection(Server, Peer, StreamPair) :-
  stream_pair(StreamPair,In,Out),
  ( read_message(In, Message) ->
    handle_message(Server, Peer,Out,Message) ;
    parse_error(Out) ),
  handle_connection(Server, Peer,StreamPair).

handle_message(_, _Peer, Out, Message) :-
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
  write_message(Out,Response).

:- dynamic declared_server_method/3.

server_method(Server, Method, Module:Handler) :-
  Clause = declared_server_method(Server, Method, Module:Handler),
  ( Clause ; assertz(Clause) ).

dispatch_method(Server, Id, MethodName, Params, Response) :-
  find_handler(Server, MethodName, Module:Handler),
  apply(Module:Handler,[Result,Params]),
  Response = _{id: Id, result: Result }.

dispatch_method(Server, MethodName, Params, Response) :-
  find_handler(Server, MethodName, Module:Handler),
  apply(Module:Handler,[Result,Params]),
  Response = _{result: Result }.

find_handler(Server,MethodName, Module:Handler) :-
  atom_string(Method,MethodName),
  (declared_server_method(_MServer:Server, _MMethod:Method, Module:Handler) ;
    throw(unknown_method(Method))).

:- dynamic declared_server_error/3.

dispatch_exception(Server, Message, Exception, Response) :-
  declared_server_error(Server, Exception, Module:Handler),
  apply(Module:Handler, [Server, Exception, Error]),
  BaseResponse = _{error: Error},
  (Id = Message.get(id) ->
    Response = BaseResponse.put(id,Id) ;
    Response = BaseResponse).

% simple method to echo parameters; good for testing
echo(Params,Params) :-
  info('Echoing %w', [Params]).

crash(Params,Params) :-
  warn("Intentionally crashing"),
  throw(crash).

unknown_method(Server, unknown_method(Method),Error) :-
  warn("Method not found for %w: %w",[Server, Method]),
  Error = _{code: -32601, message: "Method not found", data: Method },!.

server_error(Server, Error, Module:Handler) :-
  Clause = declared_server_error(Server, Error, Module:Handler),
  ( Clause ; assertz(Clause) ).

unknown_error(_Server, Exception, Error) :-
  error("An unknown error occurred: %t", [Exception]),
  Error = _{code: -32000, message: "An unknown error occurred" }, !.

parse_error(Out) :-
  Error = _{code: -32700, message: "Parse error" },
  Response = _{error: Error},
  write_message(Out, Response).

invalid_request(Out) :-
  Error = _{code: -32600, message: "Invalid Request" },
  Response = _{error: Error},
  write_message(Out, Response).

:- server_error(_,unknown_method(_),jsonrpc_server:unknown_method).

:- server_error(_,_,jsonrpc_server:unknown_error).
