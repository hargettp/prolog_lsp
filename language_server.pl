:- module(language_server,[
  start_language_server/1,
  stop_language_server/1

]).

:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(socket)).

:- use_module(library(log4p)).
:- use_module(library(jsonrpc/jsonrpc_server)).

start_language_server(Port) :-
  start_jsonrpc_server(prolog_language_server,Port).

stop_language_server(Port) :-
  stop_jsonrpc_server(prolog_language_server,Port).
