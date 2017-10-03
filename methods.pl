:- module(methods,[

  ]).

:- use_module(library(jsonrpc/jsonrpc_server)).

:- method(prolog_language_server, echo, jsonrpc_server:echo).
