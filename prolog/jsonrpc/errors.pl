:- module(jsonrpc_errors, [
  dispatch_exception/4,
  server_error/3,

  unknown_method/1,
  unknown_method/3,
  unknown_error/3,
  
  parse_error/1,
  invalid_request/1

  ]).

:- use_module(library(log4p)).
:- use_module('./protocol').

:- meta_predicate
  server_error(*,:,:),
  declared_server_error(*,:,:),
  find_handler(*, :, :).

:- dynamic declared_server_error/3.

dispatch_exception(Server, Message, Exception, Response) :-
  % declared_server_error(Server, Exception, Module:Handler),
  find_handler(Server, Exception, Module:Handler),
  debug('found error handler %w',[Module:Handler]),
  (apply(Module:Handler, [Server, Exception, Error])
    -> true
    ; unknown_error(Server, Exception, Error)
    ),
  BaseResponse = _{error: Error},
  (Id = Message.get(id) ->
    Response = BaseResponse.put(id,Id) ;
    Response = BaseResponse).
  
% Typical case
find_handler(Server, Exception, Module:Handler) :-
  declared_server_error(Server, Exception, Module:Handler), !.

% Unknown method
% :- server_error(_,unknown_method(_),jsonrpc_server:unknown_method).
find_handler(_Server, jsonrpc_errors:unknown_method(_), jsonrpc_errors:unknown_method) :- !.

% Module agnostic exception check
find_handler(Server, _:Exception, Module:Handler) :-
  declared_server_error(Server, _:Exception, Module:Handler), !.

% Unknown error -- if fall through to it, not recognized
% :- server_error(_,_,jsonrpc_server:unknown_error).
find_handler(_Server, Exception, jsonrpc_errors:unknown_error) :- 
  warn("Using unknown error handler for %w",[Exception]),
  !.

server_error(Server, Error, Module:Handler) :-
  Clause = declared_server_error(Server, Error, Module:Handler),
  ( Clause ; assertz(Clause) ).

:- dynamic unknown_method/1.

unknown_method(Server, unknown_method(Method),Error) :-
  warn("Method not found for %w: %w",[Server, Method]),
  Error = _{code: -32601, message: "Method not found", data: Method },!.

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
