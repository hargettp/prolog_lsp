:- begin_tests(pls_index_indexing).

:- use_module(library(prolog_stack)).
:- use_module(library(uri)).

:- use_module(documents).
:- use_module(indexing).

expected_vs_actual([], []).

expected_vs_actual([Expected | ExpectedMore], [Actual | ActualMore]) :-
  Expected = Actual
    -> expected_vs_actual(ExpectedMore, ActualMore)
    ; (
      with_output_to(
        user_error, 
        writef("Expected: %w but found: %w\n", [Expected, Actual])
        ),      
      fail
      ).

expected_vs_actual(_, []) :- fail.

expected_vs_actual([], _) :- fail.

test(reading) :-
  read_file_to_string('prolog/pls_index/references.pl', Content, []),
  uri_file_name(URI, 'prolog/pls_index/references.pl'),
  set_document_content(URI, Content),
  index_text(URI),
  findall(
    Callable,
    get_document_item(URI, _Ref, defines(Callable)),
    Definitions
    ),
  list_to_set(Definitions, Actual),
  Expected = [
    references_for_position/3, 
    definition_reference/3, 
    call_reference/3
    ],
  once(expected_vs_actual(Expected, Actual)).

test(index_roots) :-
  uri_file_name(URI, '.'),
  index_roots([URI]).

:- end_tests(pls_index_indexing).