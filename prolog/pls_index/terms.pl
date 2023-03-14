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
  index_term(URI, Pos, (:- reexport(Module, []))),
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
  uri_file_name(URI, FileName),
  xref_defined(FileName, Goal, How),
  functor_range(URI, FFrom, FTo, Range),
  functor(Goal, Name, Arity),
  Reference = Name/Arity,
  context_for_reference(How, Context),!,
  Item = references(Caller, Context, Reference),
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

context_for_reference(local(_), local) :- !.

context_for_reference(imported(FileName), imported(URI)) :-
  uri_file_name(URI, FileName),
  !.
  
context_for_reference(_URI, How, How).

% Given a document URI, successive calls will iterate
% over *all* the compound and atomic subterms inside
% the document; this is the basis for indexing
% calls and other cross-references
document_subterm(URI, Term, From, To, Subterm) :-
  get_document_content(URI, Content),
  !,
  setup_call_cleanup(
    open_string(Content, In),
    read_subterm(In, Term, From, To, Subterm),
    close(In)
    ).

document_subterm(URI, Term, From, To, Subterm) :-
  uri_file_name(URI, FileName),
  setup_call_cleanup(
    open(FileName, read, In),
    read_subterm(In, Term, From, To, Subterm),
    close(In)
    ).

read_subterm(In, Term, From, To, Subterm) :-
  repeat,
  read_term(In, Term, [subterm_positions(Pos)]),
  ( Term \== end_of_file
    -> (
        find_subterm(In, Pos, From, To, Subterm),
        writef("--- %q\n",[Pos])
        )
    ; (!, fail)
    ).

% primitives: atoms, numbers, booleans
find_subterm(In, From-To, From, To, Subterm) :-
  fetch_from_stream(In, From, To, Text),
  term_string(Subterm, Text),
  \+ number(Subterm),
  \+ var(Subterm).

% strings
find_subterm(In, string_position(From, To), From, To, Subterm) :-
  fetch_from_stream(In, From, To, Subterm).

% brace expressions
find_subterm(In, brace_term_position(_From, _To, Arg), From, To, Subterm) :-
  % fetch_from_stream(In, From, To, Subterm),
  find_subterm(In, Arg, From, To, Subterm).

% lists
find_subterm(In, list_position(_From, _To, Elements, _Tail), From, To, Subterm) :-
  member(Element, Elements),
  find_subterm(In, Element, From, To, Subterm).

% compound terms
find_subterm(In, term_position(From, To, _FFrom, _FTo, _Args), From, To, Subterm) :-
  fetch_from_stream(In, From, To, Text),
  term_string(Subterm, Text).

find_subterm(In, term_position(_From, _To, _FFrom, _FTo, Args), From, To, Subterm) :-
  member(Arg, Args),
  find_subterm(In, Arg, From, To, Subterm).

% dictionaries 
find_subterm(In, dict_position(_From, _To, _TagFrom, _TagTo, KeyValuePosList), From, To, Subterm) :-
  member(KeyValuePos, KeyValuePosList),
  find_subterm(In, KeyValuePos, From, To, Subterm).

% dictionary key/value pair
find_subterm(In, key_value_position(_From, _To, _SepFrom, _SepTo, _Key, KeyPos, ValuePos), From, To, Subterm) :-
  % fetch_from_stream(In, From, To, Subterm).
  ( find_subterm(In, KeyPos, From, To, Subterm) ; find_subterm(In, ValuePos, From, To, Subterm) ).

% terms between parentheses
find_subterm(In, parentheses_term_position(_From, _To, ContentPos), From, To, Subterm) :-
  % fetch_from_stream(In, From, To, Subterm).
  find_subterm(In, ContentPos, From, To, Subterm).

fetch_from_stream(In, From, To, Value) :-
  Length is To - From,
  setup_and_call_cleanup(
    stream_property(In, position(Current)),
    (seek(In, From, bof, _), read_string(In, Length, Value) ),
    set_stream_position(In, Current)
  ).
