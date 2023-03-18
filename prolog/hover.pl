:- module(pls_index_hover, [
  hover_for_position/3
]).

:- use_module(pls_index).

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
  get_docs(Callable, Docs),
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

