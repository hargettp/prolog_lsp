:- [prolog/prolog_lsp].

:- use_module(library(plunit)).

:- set_test_options([
  run(make(all))
  ]).

:- load_test_files(_X).

:- run_tests.
