:- begin_tests(pls_index_lines).

:- use_module(lines).
:- use_module(documents).

:- use_module(library(log4p)).

test(index_lines) :-
  format("starting line indexing test~n"),
  % Use an actual project file for testing
  uri_file_name(URI, 'prolog/pls_index/documents.pl'),
  read_file_to_string('prolog/pls_index/documents.pl', Content, []),
  set_document_content(URI, Content),
  index_lines(URI),
  % Verify that we have a line count
  findall(Count, get_document_line_count(URI, Count), Counts),
  Counts \= [],
  % Verify we can find line positions for various byte offsets
  findall(Line, get_document_line_position(URI, Line, _), Lines),
  Lines \= [].

:- end_tests(pls_index_lines).