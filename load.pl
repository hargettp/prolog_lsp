user:file_search_path(lib, './lib')

:- use_module(library(log4p)).

% Let's reconfigure logging to use stderr
:- initialization(use_stderr_log_handler).

:- use_module(lib(jsonrpc/jsonrpc_protocol)).
:- use_module(lib(jsonrpc/connectors)).
:- use_module(lib(jsonrpc/jsonrpc_server)).
:- use_module(lib(jsonrpc/jsonrpc_client)).
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
