:- module(language_server,[

  run_language_server/0

]).

:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(socket)).

:- use_module(library(log4p)).
:- use_module(lib(jsonrpc/connectors)).

% Run a language server synchronously in this process using stdio for streams; 
% does not return until server exits
run_language_server :-
  create_stdio_server(prolog_language_server, Server),
  serve_messages(Server).

% Run a language server asynchronously in a separate thread;
% returns immediately, but server can be stopped with 
% stop_language_server and the same arguments.
start_language_server(Port) :-
  start_jsonrpc_server(prolog_language_server, Port).

stop_language_server(Port) :-
  stop_jsonrpc_server(prolog_language_server, Port).
