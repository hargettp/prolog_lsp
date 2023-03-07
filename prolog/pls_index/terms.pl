:- module(pls_index_terms, [
  ]).

:- use_module(documents).

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
    -> find_subterm(In, Pos, From, To, Subterm)
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
