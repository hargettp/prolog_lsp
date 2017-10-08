:- module(methods,[

  ]).

:- use_module(library(jsonrpc/jsonrpc_server)).

:- server_method(prolog_language_server, echo, jsonrpc_server:echo).
:- server_method(prolog_language_server, crash, jsonrpc_server:crash).
