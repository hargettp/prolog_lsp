:- module(jsonrpc_methods, [
  server_method/3,
  find_handler/3
  ]).

:- use_module('./errors').

:- meta_predicate
  server_method(:,:,:),
  declared_server_method(:,:,:).

:- dynamic declared_server_method/3.

server_method(Server, Method, Module:Handler) :-
  Clause = declared_server_method(Server, Method, Module:Handler),
  ( Clause ; assertz(Clause) ).

find_handler(Server,MethodName, Module:Handler) :-
  atom_string(Method,MethodName),
  (declared_server_method(_MServer:Server, _MMethod:Method, Module:Handler) ;
    throw(unknown_method(Method))).