:- module(test_methods, [
  ]).

:- use_module('../prolog/prolog_lsp').

:- server_method(jsonrpc_client_test, echo, jsonrpc_server:echo).
:- server_method(jsonrpc_client_test, crash, jsonrpc_server:crash).

