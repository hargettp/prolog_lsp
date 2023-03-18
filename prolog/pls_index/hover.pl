:- module(pls_index_hover, [
  index_docs/3,
  hover_for_position/3
]).

:- use_module(library(pldoc)).
:- use_module(library(pldoc/doc_process)).
:- use_module(library(pldoc/doc_wiki)).

:- use_module(documents).

index_docs(URI, CommentPos, TermPos) :-
  uri_file_name(URI, FileName),
  process_comments(CommentPos, TermPos, FileName).

join_comments(CommentPos, Docs) :-
  with_output_to(string(Docs),
    forall(
      member(_Pos-Comment, CommentPos), 
      ( writeln(Comment) )
      )
    ).

hover_for_position(URI, Position, Hover) :-
  get_document_item(URI, Position, references(Caller, Callable)),
  get_document_item(URI, Range, references(Caller, Callable)),
  doc_comment(Callable, _FileName:_Line, _Summary, Comment),
  comment_markup(Comment, Docs),
  Hover = _{
    range: Range,
    contents: _{
      kind: 'markdown',
      value: Docs
      }
  },!.

hover_for_position(URI, Position, Hover) :-
  get_document_item(URI, Position, defines(Callable)),
  get_document_item(URI, Range, defines(Callable)),
  doc_comment(Callable, _FileName:_Line, _Summary, Comment),
  comment_markup(Comment, Docs),
  Hover = _{
    range: Range,
    content: Docs
  }.

comment_markup(Comment, Markup) :-
  is_structured_comment(Comment, Prefixes),
  string_codes(Comment, Codes),
  indented_lines(Codes, Prefixes, Lines),
  findall(
    String, 
    (
      member(_Indent-Indented, Lines),
      string_codes(Line, Indented),
      format_doc_line(Line,String)
      ),
    Strings
    ),
  with_output_to(string(Markup),
    forall(member(String, Strings),writeln(String))
    ).

format_doc_line(Line, String) :-
  % TODO: don't make assumptions about amount of whitespace
  string_concat("! ", Core, Line),
  swritef(String, "**%w**\n",[Core]),
  !.

format_doc_line(Line, String) :-
  string_concat("% ", Core, Line),
  swritef(String, "**%w**\n",[Core]),
  !.

format_doc_line(Line, Line).
