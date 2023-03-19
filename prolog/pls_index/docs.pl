:- module(pls_index_docs, [
  index_docs/3,
  get_docs/2

]).

:- use_module(library(dcg/basics)).

:- use_module(library(pldoc)).
:- use_module(library(pldoc/doc_process)).
:- use_module(library(pldoc/doc_wiki)).

index_docs(URI, CommentPos, TermPos) :-
  uri_file_name(URI, FileName),
  process_comments(CommentPos, TermPos, FileName).

get_docs(Predicate, Docs) :-
  doc_comment(Predicate, _FileName:_Line, _Summary, Comment),
  comment_markup(Predicate, Comment, Docs).

comment_markup(Predicate, Comment, Markup) :-
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
      writef("**%w**\n\n",[Predicate]),
      forall(member(String, Strings),writeln(String))
      )
    ).

format_doc_line(Line, String) :-
  % TODO: don't make assumptions about amount of whitespace
  mode(Line, Mode),
  swritef(String, "*%w*\n",[Mode]),
  !.

format_doc_line(Line, String) :-
  comment(Line, Comment),
  swritef(String, "%w\n",[Comment]),
  !.

format_doc_line(Line, Line).

mode(Line, Mode) :-
  string_codes(Line, Codes),
  phrase(mode(Mode),Codes).

mode(Mode) -->
  "!",
  whites,
  string(Codes),
  {string_codes(Mode, Codes)}.

mode(Mode) -->
  "%%",
  whites,
  string(Codes),
  {string_codes(Mode, Codes)}.

comment(Line, Comment) :-
  string_codes(Line, Codes),
  phrase(comment(Comment), Codes).

comment(Comment) -->
  "%",
  whites,
  string(Codes),
  {string_codes(Comment, Codes)}.
