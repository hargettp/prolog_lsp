:- module(methods,[

  ]).

:- use_module(library(jsonrpc/jsonrpc_server)).

:- method(prolog_language_server, echo, jsonrpc_server:echo).
:- method(prolog_language_server, crash, jsonrpc_server:crash).
