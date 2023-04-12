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
:- use_module(symbols).
:- use_module(errors).
:- use_module(methods).
:- reexport(language_server).
:- reexport(language_client).
:- reexport('./pls_index/profiles').

% We load the base profile by default
:- use_module(base_language_profile).
:- assertz(pls_index_profiles:profile_loaded(base)).
