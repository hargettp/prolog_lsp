:- module(prolog_lsp, [
  ]).

:- use_module(library(log4p)).

% Let's reconfigure logging to use stderr
:- initialization(use_stderr_log_handler).

:- use_module(jsonrpc/protocol).
:- use_module(jsonrpc/connectors).
:- reexport('./jsonrpc/server').
:- reexport('./jsonrpc/client').
:- reexport('./jsonrpc/hooks').
:- use_module(files).
:- use_module(symbols).
:- use_module(code).
:- use_module(errors).
:- use_module(methods).
:- reexport(language_server).
:- reexport(language_client).
