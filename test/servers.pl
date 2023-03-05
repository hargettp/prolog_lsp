:- module(test_servers, [
  run_test_stdio_server/0
  ]).

:- use_module(library(log4p)).
:- use_module('../prolog/jsonrpc/connectors').
:- use_module('../prolog/jsonrpc/hooks').
:- use_module('./methods').

run_test_stdio_server :-
  log4p:set_global_log_level(debug),
  enable_jsonrpc_tracing,
  create_stdio_server(jsonrpc_client_test, Server),
  serve_messages(Server).

