:- module(pls_index_docs, [
  index_docs/4,
  hover_for_position/3
]).

% :- use_module(library(pcre)).

:- use_module(documents).

index_docs(URI, Range, Head, CommentPos) :-
  join_comments(CommentPos, Docs),
  functor(Head,Name,Arity),
  Callable = Name/Arity,
  ( Docs = ""
    -> true
    ; add_document_item(URI, Range, docs(Callable, Docs))
    ).

join_comments(CommentPos, Docs) :-
  with_output_to(string(Docs),
    forall(
      member(_Pos-Comment, CommentPos), 
      ( writeln(Comment) )
      )
    ).

% strip_comment(Comment, Stripped) :-
%   string_lines(Comment, Lines),
%   maplist(strip_line, Lines, StrippedLines),
%   with_output_to(string(Stripped),
%     forall(member(StripedLine, StrippedLines), writeln(StripedLine))
%   ).

% strip_line(Comment, String) :-
%   re_replace("%*[[:blank:]]+","", Comment, String).

hover_for_position(URI, Position, Hover) :-
  get_document_item(URI, Position, references(Callable)),
  get_document_item(URI, Range, references(Callable)),
  get_document_item(_DocURI, _DocRange, docs(Callable, Docs)),
  Hover = _{
    range: Range,
    content: Docs
  }.
