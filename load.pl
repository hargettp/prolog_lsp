:- use_module(library(log4p)).
:- use_module(library(jsonrpc/jsonrpc_server)).
:- use_module(server).
:- use_module(methods).

:- use_module(library(plunit)).

:- set_test_options([
  run(make(all))
  ]).

:- load_test_files(_X).

:- run_tests.
