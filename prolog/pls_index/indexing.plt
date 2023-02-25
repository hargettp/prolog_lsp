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
  read_file_to_string('./test.pl', Content, []),
  uri_file_name(URI, 'test.pl'),
  set_document_content(URI, Content),
  index_text(URI),
  findall(
    Name/Arity,
    (index_defined(URI, Definition, _How), functor(Definition, Name, Arity)),
    Definitions
    ),
  list_to_set(Definitions, Actual),
  Expected = [
    run_stdio_language_server/0,
    run_tcp_language_server/1,
    start_tcp_language_server/1,
    stop_tcp_language_server/1,
    stdio_language_connector/1,
    with_stdio_language/2,
    with_tcp_language/3,
    initialize/2,
    initialized/1,
    shutdown/2,
    exit/1,
    (:)/2,
    set_test_options/1,
    begin_tests/1,
    begin_tests/2,
    end_tests/1,
    run_tests/0,
    run_tests/1,
    load_test_files/1,
    running_tests/0,
    current_test/5,
    current_test_unit/2,
    test_report/1
    ],
  once(expected_vs_actual(Expected, Actual)).

test(index_roots) :-
  uri_file_name(URI, '.'),
  index_roots([URI]).

:- end_tests(pls_index_indexing).