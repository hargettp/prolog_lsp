:- begin_tests(pls_index_indexing).

:- use_module(library(prolog_stack)).

:- use_module(documents).
:- use_module(indexing).

test(reading) :-
  read_file_to_string('./test.pl', Content, []),
  URI = 'file://test.pl',
  clear_document_items(URI),
  index_text('file://test.pl', Content).

:- end_tests(pls_index_indexing).