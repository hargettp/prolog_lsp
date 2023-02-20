:- begin_tests(pls_index_indexing).

:- use_module(library(prolog_stack)).

:- use_module(documents).
:- use_module(indexing).

expected_vs_actual([], []).

expected_vs_actual([Expected | ExpectedMore], [Actual | ActualMore]) :-
  Expected = Actual
    -> expected_vs_actual(ExpectedMore, ActualMore)
    ; (
      writef("Expected: %w but found: %w\n", [Expected, Actual]),
      fail
      ).

expected_vs_actual(_, []) :- fail.

expected_vs_actual([], _) :- fail.

test(reading) :-
  read_file_to_string('./test.pl', Content, []),
  URI = 'file://test.pl',
  clear_document_items(URI),
  index_text('file://test.pl', Content),
  get_document_items(URI, Actual),
  Expected = [
    loads(prolog/prolog_lsp, _),
    uses_module(library(plunit), []),
    calls(set_test_options/1,_),
    calls(run/1,_),
    calls(make/1,_),
    calls(load_test_files/1,_),
    calls(run_tests/0,_)
  ],
  once(expected_vs_actual(Expected, Actual)).

:- end_tests(pls_index_indexing).