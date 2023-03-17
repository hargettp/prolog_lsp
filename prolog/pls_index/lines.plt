:- begin_tests(pls_index_lines).

:- use_module(lines).
:- use_module(documents).

test(index_lines) :-
  uri_file_name(URI, 'test.pl'),
  index_lines(URI),
  findall(Count, get_document_line_count(URI, Count), [11]),
  findall(Line, get_document_line_position(URI, Line, 50), [2]),
  findall(Line, get_document_line_position(URI, Line, 100), [6]),
  findall(Line, get_document_line_position(URI, Line, 150), [11]).

:- end_tests(pls_index_lines).