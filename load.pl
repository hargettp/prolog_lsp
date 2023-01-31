% user:file_search_path(library, './lib')

:- use_module(library(log4p)).
:- use_module(library(jsonrpc/jsonrpc_protocol)).
:- use_module(library(jsonrpc/jsonrpc_server)).
:- use_module(library(jsonrpc/jsonrpc_client)).
:- use_module(language_server).
:- use_module(language_client).
:- use_module(methods).
:- use_module(files).
:- use_module(workspace).
:- use_module(code).

:- use_module(library(plunit)).

:- set_test_options([
  run(make(all))
  ]).

:- load_test_files(_X).

:- run_tests.
