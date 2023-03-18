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

hover_for_position(URI, Position, Hover) :-
  get_document_item(URI, Position, exports(Callable)),
  get_document_item(URI, Range, exports(Callable)),
  get_hover(Callable, Range, Hover),
  !.

hover_for_position(URI, Position, Hover) :-
  get_document_item(URI, Position, references(Caller, Callable)),
  get_document_item(URI, Range, references(Caller, Callable)),
  get_hover(Callable, Range, Hover),
  !.

hover_for_position(URI, Position, Hover) :-
  get_document_item(URI, Position, defines(Callable)),
  get_document_item(URI, Range, defines(Callable)),
  get_hover(Callable, Range, Hover),
  !.

%! get_hover(+Callablle, +Range, -Hover) is nondet.
%
% Return a hover for the indicated range; will always
% return minimal markup, even if no documentation available.
%
get_hover(Callable, Range, Hover) :-
  doc_comment(Callable, _FileName:_Line, _Summary, Comment),
  comment_markup(Callable, Comment, Docs),
  Hover = _{
    range: Range,
    contents: _{
      kind: 'markdown',
      value: Docs
      }
  }.

get_hover(Callable, Range, Hover) :-
  swritef(Docs, "## %w\n*No documentation available.*",[Callable]),
  Hover = _{
    range: Range,
    contents: _{
      kind: 'markdown',
      value: Docs
      }
  }.

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