:- module(pls_index_lines, [
  index_lines/1
]).

:- use_module(documents).

index_lines(URI) :-
  clear_document_lines(URI),
  clear_document_line_count(URI),
  forall(
    document_line_position(URI, Line, Position), 
    add_document_line(URI, Line, Position)
    ),
  findall(Line, get_document_line_position(URI, Line, _), Lines),
  max_list(Lines, LineCount),
  set_document_line_count(URI, LineCount).

document_line_position(URI, Line, Position) :-
  with_content(URI, In, 
    stream_line_position(In, Line, Position)
    ).

stream_line_position(_In, 1, 0).
stream_line_position(In, Line, Position) :-
  repeat,
  read_line_to_string(In, Text),
  ( Text \== end_of_file
    -> (
        line_count(In, Count),
        Line is Count - 1,
        character_count(In, Position)
      )
    ; ( !, fail)
    ).
  