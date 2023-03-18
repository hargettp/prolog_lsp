:- module(pls_index_docs, [
  index_docs/3,
  get_docs/2

]).

:- use_module(library(pldoc)).
:- use_module(library(pldoc/doc_process)).
:- use_module(library(pldoc/doc_wiki)).

index_docs(URI, CommentPos, TermPos) :-
  uri_file_name(URI, FileName),
  process_comments(CommentPos, TermPos, FileName).

get_docs(Callable, Docs) :-
  doc_comment(Callable, _FileName:_Line, _Summary, Comment),
  comment_markup(Callable, Comment, Docs).

comment_markup(Callable, Comment, Markup) :-
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
    (
      writef("## %w\n",[Callable]),
      forall(member(String, Strings),writeln(String))
      )
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