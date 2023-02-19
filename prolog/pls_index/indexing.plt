:- begin_tests(pls_index_indexing).

:- use_module(library(prolog_stack)).

:- use_module(indexing).

test(reading) :-
  read_file_to_string('./test.pl', Content, []),
  catch_with_backtrace(
    index_text('file://test.pl', Content), 
    Error,
    print_message(error, Error)
    ).

:- end_tests(pls_index_indexing).