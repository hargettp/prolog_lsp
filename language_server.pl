:- module(language_server,[

  run_language_server/0

]).

:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(socket)).

:- use_module(library(log4p)).
:- use_module(lib(jsonrpc/connectors)).

run_language_server :-
  create_stdio_connector(prolog, Server),
  serve_messages(Server).

% stop_language_server(Port) :-
%   stop_jsonrpc_server(prolog_language_server,Port).
