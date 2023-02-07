:- module(prolog_lsp, [
  ]).

:- use_module(library(log4p)).

% Let's reconfigure logging to use stderr
:- initialization(use_stderr_log_handler).

:- use_module(jsonrpc/jsonrpc_protocol).
:- use_module(jsonrpc/connectors).
:- use_module(jsonrpc/jsonrpc_server).
:- use_module(jsonrpc/jsonrpc_client).
:- use_module(files).
:- use_module(workspace).
:- use_module(code).
:- use_module(errors).
:- use_module(methods).
:- reexport(language_server).
:- reexport(language_client).
