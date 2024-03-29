:- module(pls_index_docs, [
  index_docs/4,
  get_docs/2

]).

:- use_module(library(dcg/basics)).

:- use_module(library(pldoc)).
:- use_module(library(pldoc/doc_process)).
:- use_module(library(pldoc/doc_wiki)).

:- use_module(documents).

% Index raw comments for later retrieval as docs
% 
index_docs(URI, PredicateOrKey, Range, CommentPos) :-
  ( filter_for_docs(Range, CommentPos, DocLine, Docs)
    -> add_document_item(URI, Range, docs(PredicateOrKey, DocLine, Docs))
    ; true
    ),
  !.

%! filter_for_docs(+Range, +CommentPos, -DocLine, -Docs) is nondet.
%
% Return the line where docs start and the docs (e.g., multi-line string 
% comments) that precede the provided range. This separates comments
% for documentation from comments that are interior to the definition,
% and thus comments and not documentation.
filter_for_docs(Range, CommentPos, DocLine, Docs) :-
  findall(
    Line-Comment,
    (
      member(Pos-Comment, CommentPos),
      stream_position_data(line_count, Pos, LineNo),
      Line is LineNo - 1,
      string_lines(Comment, Lines),
      length(Lines, LineCount),
      Start = Range.start.line,
      Start is Line + LineCount
    ),
    Comments
    ),
  Comments = [DocLine-Docs].

get_docs(Predicate, Docs) :-
  get_document_item(_URI, _Range, docs(Predicate, _DocLine, Comment)),
  comment_markup(Predicate, Comment, Docs),
  !.

 get_docs(Predicate, Docs) :-
  swritef(Docs, "### %w\n*No documentation available.*",[Predicate]).

comment_markup(Predicate, Comment, Markup) :-
  ( is_structured_comment(Comment, Prefixes)
    -> true
    ; Prefixes = []
    ),
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
      writef("### %w\n",[Predicate]),
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
