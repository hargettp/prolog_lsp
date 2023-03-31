:- module(pls_index_documents, [
  store_document/4,

  get_document_properties/2,
  add_document_property/2,
  set_document_property/2,
  get_document_property/2,
  clear_document_properties/1,

  get_document_uri/1,
  set_document_uri/1,
  clear_document_uri/1,

  get_document_content/2,
  set_document_content/2,
  clear_document_content/1,
  with_content/3,

  add_document_item/3,
  get_document_item/3,
  clear_document_item/2,
  clear_document_items/1,

  add_document_line/3,
  get_document_line_position/3,
  clear_document_line/2,
  clear_document_lines/1,
  set_document_line_count/2,
  get_document_line_count/2,
  clear_document_line_count/1  
  ]).

:- dynamic document_content/2.
:- dynamic document_item/3.
:- dynamic document_line_count/2.
:- dynamic document_line_position/3.
:- dynamic document_property/2.
:- dynamic document_uri/1.

store_document(URI, Language, Version, Content) :-
  clear_document_properties(URI),
  clear_document_items(URI),
  clear_document_lines(URI),
  clear_document_line_count(URI),
  set_document_property(URI, language(Language)),
  set_document_property(URI, version(Version)),
  set_document_content(URI, Content).

get_document_properties(URI, Properties) :-
  findall(Property, document_property(URI, Property), Properties).

add_document_property(URI, Property) :-
  assertz(document_property(URI, Property)).

set_document_property(URI, Property) :-
  functor(Property, Name, Arity),
  functor(Clear, Name, Arity),
  clear_document_property(URI, Clear),
  assertz(document_property(URI, Property)).

get_document_property(URI, Property) :-
  document_property(URI, Property).

clear_document_properties(URI) :-
  clear_document_property(URI, _).

clear_document_property(URI, Clear) :-
  retractall(document_property(URI, Clear)).

set_document_content(URI, Content) :-
  clear_document_content(URI),
  assertz(document_content(URI, Content)).

get_document_content(URI, Content) :-
  document_content(URI, Content).

clear_document_content(URI) :-
  retractall(document_content(URI, _)).

% 
% document accessors
% 

% -- URI --

set_document_uri(URI) :-
  get_document_uri(URI), !.

set_document_uri(URI) :-
  assertz(document_uri(URI)).

get_document_uri(URI) :-
  document_uri(URI).

clear_document_uri(URI) :-
  retractall(document_uri(URI)).

% -- content --

%! set_content(+URI, +Content) is nondet.
% 
% Record content for the specified URI; usually
% for temporary available of content for a document
% where editing is underway
%
set_content(URI, Content) :-
  clear_language(URI),
  assertz(document_content(URI, Content)).

get_content(URI, Content) :-
  once(document_content(URI, Content)).

clear_content(URI) :-
  clear_document_content(URI).

% --- items --
add_document_item(URI, Range, Value) :-
  assertz(document_item(URI, Range, Value)).

%! get_document_item(?URI, +Position, -Item) is nondet.
%! get_document_item(?URI, ?Range, ?Item) is nondet.
%
% When called in its first form, get a document item based on its position,
% otherwise return a document item by unifying with URI, Range, and Item .
%
get_document_item(URI, Position, Item) :-
  nonvar(Position),
  Line = Position.get(line),
  Character = Position.get(character),
  % Must have ground actual position
  ground(Line),ground(Character),
  get_document_item(
    URI,
    _{
      start: _{line: Line, character: FromCharacter},
      end:  _{line: _ToLine, character: ToCharacter}
      },
    Item
    ),
  Character >= FromCharacter,
  Character =< ToCharacter.

get_document_item(URI, Range, Value) :-
  document_item(URI, Range, Value).

clear_document_item(URI, Value) :-
  retractall(document_item(URI, _Range, Value)).

clear_document_items(URI) :-
  clear_document_item(URI, _).

:- meta_predicate with_content(?, ?, :).
with_content(URI, In, Module:Goal) :-
  get_document_content(URI, Content),
  !,
  setup_call_cleanup(
    open_string(Content, In),
    call(Module:Goal),
    close(In)
    ).

with_content(URI, In, Module:Goal) :-
  uri_file_name(URI, FileName),
  setup_call_cleanup(
    open(FileName, read, In),
    call(Module:Goal),
    close(In)
    ).

% --- lines --

add_document_line(URI, Line, Position) :-
  assertz(document_line_position(URI, Line, Position)).

get_document_line_position(URI, Line, Position) :-
  ground(Position),
  get_document_line_count(URI, Max),
  MidPoint is ceiling(Max / 2),
  Range is ceiling(MidPoint / 2),
  find_line_position(URI, Position, MidPoint, Range, Line),
  !.

get_document_line_position(URI, Line, Position) :-
  document_line_position(URI, Line, Position).

clear_document_line(URI, Line) :-
  retractall(document_line_position(URI, Line, _)).

clear_document_lines(URI) :-
  clear_document_line(URI, _).

set_document_line_count(URI, LineCount) :-
  clear_document_line_count(URI),
  assertz(document_line_count(URI, LineCount)).

get_document_line_count(URI, LineCount) :-
  document_line_count(URI, LineCount).

clear_document_line_count(URI) :-
  retractall(document_line_count(URI, _)).

find_line_position(URI, Position, Start, _Range, Line) :-
  position_falls_on_line(URI, Start, Position),
  Line = Start.

find_line_position(URI, Position, Start, Range, Line) :-
  get_document_line_position(URI, Start, LinePosition),
  NewRange is ceiling(Range / 2),
  UpperStart is Start + NewRange,
  LowerStart is Start - NewRange,
  (Position < LinePosition
    -> find_line_position(URI, Position, LowerStart, NewRange, Line)
    ; find_line_position(URI, Position, UpperStart, NewRange, Line)
    ).

position_falls_on_line(URI, Line, Position) :-
  get_document_line_position(URI, Line, LinePosition),
  NextLine is Line + 1,
  (get_document_line_position(URI, NextLine, NextPosition) 
    -> true
    ; NextPosition is inf 
    ),
  ( Position >= LinePosition , Position < NextPosition).

% -- language --

set_language(URI, Language) :-
  clear_language(URI),
  set_document_property(URI, language(Language)).

get_language(URI, Language) :-
  once(get_document_property(URI, language(Language))).

clear_language(URI) :-
  clear_document_property(URI, language(_)).

% -- version --

set_version(URI, Version) :-
  clear_version(URI),
  set_document_property(URI, version(Version)).

get_version(URI, Version) :-
  once(get_document_property(URI, version(Version))).

clear_version(URI) :-
  clear_document_property(URI, version(_)).
