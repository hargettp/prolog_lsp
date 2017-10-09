:- module(methods,[

  ]).

:- use_module(library(jsonrpc/jsonrpc_server)).

:- server_method(prolog_language_server, echo, jsonrpc_server:echo).
:- server_method(prolog_language_server, crash, jsonrpc_server:crash).

:- server_method(prolog_language_server, initialize, pls_initialize).
:- server_method(prolog_language_server, initialized, pls_initialized).

pls_initialize(Result,Params) :-
  info("Method initialize called"),
  info("Client capabilities: %w",[Params]),
  server_capabilities(Result).

pls_initialized(_Result,_Params) :-
  info("Notification initialized called").

pls_shutdown(Result) :-
  info("Method shutdown called"),
  % we don't actually shut anything down right now
  Result = null.

pls_exit(Result) :-
  info("Method exit called"),
  % we don't actually exit anything down right now
  Result = null.


% common data structures
server_capabilities(Capabilities) :-
  Capabilities = _{
    }.
