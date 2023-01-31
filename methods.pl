:- module(methods,[

  ]).

:- use_module(library(log4p)).
:- use_module(library(jsonrpc/jsonrpc_server)).
:- use_module(code).

:- server_method(prolog_language_server, echo, jsonrpc_server:echo).
:- server_method(prolog_language_server, crash, jsonrpc_server:crash).

:- server_method(prolog_language_server, initialize, pls_initialize).
:- server_method(prolog_language_server, initialized, pls_initialized).

:- server_method(prolog_language_server, shutdown, pls_shutdown).
:- server_method(prolog_language_server, exit, pls_exit).

:- server_method(prolog_language_server, 'workspace/symbol', pls_workspace_symbols).

pls_initialize(Result,Params) :-
  info("Method initialize called"),
  info("Client capabilities: %w",[Params]),
  server_capabilities(Capabilities),
  Result = _{
    capabilities: Capabilities
    }.

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

pls_workspace_symbols(Symbols, Query) :-
  workspace_symbols(Query, Symbols).

% common data structures
server_capabilities(Capabilities) :-
  Capabilities = _{
    }.
