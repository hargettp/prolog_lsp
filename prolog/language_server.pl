:- module(language_server,[

  run_stdio_language_server/0,
  run_tcp_language_server/1,

  start_language_server/1,
  stop_language_server/1

]).

:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(socket)).

:- use_module(library(log4p)).
:- use_module(jsonrpc/connectors).

% Run a language server synchronously in this process using stdio for streams; 
% does not return until server exits
run_stdio_language_server :-
  create_stdio_server(prolog_language_server, Server),
  serve_messages(Server).

run_tcp_language_server(Port) :-
  create_tcp_server(prolog_language_server, Port,Server),
  serve_messages(Server).

% Run a language server asynchronously in a separate thread;
% returns immediately, but server can be stopped with 
% stop_language_server and the same arguments.
start_language_server(Port) :-
  start_jsonrpc_server(prolog_language_server, Port).

stop_language_server(Port) :-
  stop_jsonrpc_server(prolog_language_server, Port).
