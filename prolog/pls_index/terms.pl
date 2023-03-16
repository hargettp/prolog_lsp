:- module(pls_index_terms, [
  index_terms/1
  ]).

:- use_module(library(http/json)).
:- use_module(library(log4p)).

:- use_module(documents).

% Index the terms in a file, including subterms.
% A file or document is a sequence of terms, and
% terms contains goals.
index_terms(URI) :-
  clear_document_items(URI),
  forall(index_term(URI), true).

index_term(URI) :-
  with_content(URI, In, (
    repeat,
    read_term(In, Term, [subterm_positions(Pos)]),
    ( Term \== end_of_file
      -> (
          index_term(URI, Pos, Term)
          % writef("--- %q\n",[Pos])
          )
      ; (!, fail)
      )
    )).
    
index_term(URI, Pos, (:- module(Module, Exports))) :-
  term_position_range(URI, Pos, Range),
  add_document_item(URI, Range, module(Module, Exports)),
  !.

index_term(URI, Pos, (:- use_module(Module))) :-
  term_position_range(URI, Pos, Range),
  add_document_item(URI, Range, uses(Module)),
  !.

index_term(URI, Pos, (:- reexport(Module))) :-
  term_position_range(URI, Pos, Range),
  add_document_item(URI, Range, reexports(Module)),
  !.

index_term(URI, Pos, (:- reexport(Module, Imports))) :-
  term_position_range(URI, Pos, Range),
  add_document_item(URI, Range, reexports(Module,Imports)),
  !.

index_term(URI, Pos, (:- [FileSpec])) :-
  term_position_range(URI, Pos, Range),
  add_document_item(URI, Range, loads(FileSpec)),
  !.

index_term(URI, Pos, (:- include(FileSpec) )) :-
  term_position_range(URI, Pos, Range),
  add_document_item(URI, Range, includes(FileSpec)),
  !.

index_term(URI, Pos, (Head :- Body)) :-
  functor(Head, Name, Arity),
  Caller = Name/Arity,
  term_position_subpos(Pos, [HeadPos, BodyPos]),
  term_position_range(URI, HeadPos, Range),
  add_document_item(URI, Range, defines(Caller)),
  index_goals(URI, Caller, BodyPos, Body),
  !.

index_goals(URI, Caller, GoalPos, Goal) :-
  forall(index_goal(URI, Caller, GoalPos, Goal), true).

index_goal(URI, Caller, parentheses_term_position(_From, _To, ContentPos), Goal) :-
  index_goal(URI, Caller, ContentPos, Goal).

index_goal(URI, Caller, term_position(_From, _To, FFrom, FTo, _Subpos), Goal) :-
  functor_range(URI, FFrom, FTo, Range),
  functor(Goal, Name, Arity),
  Callable = Name/Arity,
  Item = references(Caller, Callable),
  debug("Adding item %w",[Item]),
  add_document_item(URI, Range, Item).

index_goal(URI, Caller, term_position(_From, _To, _FFrom, _FTo, Subpos), Goal) :-
  functor(Goal, _Name, Arity),
  between(1, Arity, Index),
  arg(Index, Goal, Arg),
  nth1(Index, Subpos, Pos),
  index_goal(URI, Caller, Pos, Arg).

functor_range(URI, term_position(_From, _To, FFrom, FTo, _Subpos), Range) :-
  term_range(URI, FFrom, FTo, Range).

functor_range(URI, FFrom, FTo, Range) :-
  term_range(URI, FFrom, FTo, Range).

term_position_range(URI, term_position(From, To, _FFrom, _FTo, _Subpos), Range) :-
  term_range(URI, From, To, Range).

term_range(URI, From, To, Range) :-
  get_document_line_position(URI, FromLine, From),
  get_document_line_position(URI, FromLine, FromStart),
  get_document_line_position(URI, ToLine, To),
  get_document_line_position(URI, ToLine, ToStart),
  FromPosition is From - FromStart,
  ToPosition is To - ToStart,
  Range = range{
    start: position{
      line: FromLine,
      character: FromPosition
      }, 
    end: position{
      line: ToLine, 
      character: ToPosition
      }
    }.

term_position_subpos(term_position(_From, _To, _FFrom, _FTo, Subpos), Subpos).