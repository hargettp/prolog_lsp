:- module(pls_index_completions, [
  completions_for_position/3,
  suggestions_for_position/3,
  identifier_for_position/3
]).

:- use_module(docs).
:- use_module(documents).

:- use_module(library(log4p)).

completions_for_position(URI, Position, Completions) :-
  % we usually are at the end, so not pointing at a character
  % so go back 1
  prev_position(Position, Previous),
  ( suggestions_for_position(URI, Previous, Suggestions)
    -> (
        findall(
          _{
            label: Label,
            kind: 3,
            documentation: Docs,
            insertText: Signature,
            insertMode: 2
          },
          (
            member(Suggestion, Suggestions),
            Suggestion = Name/Arity,
            term_string(Suggestion, Label),
            get_document_item(_DefURI, _DefRange, signature(Name/Arity, Signature)),
            get_docs(Suggestion, Docs)
            ),
          Completions
        )
      )
    ; Completions = []
    ).

suggestions_for_position(URI, Position, Suggestions) :-
  identifier_for_position(URI, Position, Identifier),
  atom_length(Identifier, Length),
  (Length >= 3
    -> findall(
        Name/Arity,
        (
          get_document_item(_DefURI, _DefRange, defines(Name/Arity)),
          sub_atom(Name, _Before, Length, _After, Identifier)
          ),
        Suggestions
      )
    ; fail
    ).

identifier_for_position(URI, Position, Identifier) :-
  find_identifier_start(URI, Position, Start),
  find_identifier_end(URI, Position, End),
  Length is End.character - Start.character + 1,
  content_at_position(URI, Start, Length, Text),
  atom_string(Identifier, Text).

find_identifier_start(URI, Position, Start) :-
  Position.character = 0,
  character_at_position(URI, Position, Character),
  identifier_character(Character),
  Start = Position,
  !.

find_identifier_start(URI, Position, Start) :-
  character_at_position(URI, Position, Character),
  identifier_character(Character),
  prev_position(Position, Previous),
  character_at_position(URI, Previous, PreviousCharacter),
  \+identifier_character(PreviousCharacter),
  Start = Position,
  !.

find_identifier_start(URI, Position, Start) :-
  character_at_position(URI, Position, Character),
  identifier_character(Character),
  prev_position(Position, Previous),
  find_identifier_start(URI, Previous, Start),
  !.

find_identifier_end(URI, Position, End) :-
  character_at_position(URI, Position, Character),
  identifier_character(Character),
  next_position(Position, NextPosition),
  character_at_position(URI, NextPosition, NextCharacter),
  \+identifier_character(NextCharacter),
  End = Position,
  !.

find_identifier_end(URI, Position, End) :-
  character_at_position(URI, Position, Character),
  identifier_character(Character),
  next_position(Position, NextPosition),
  find_identifier_end(URI, NextPosition, End),
  !.

prev_position(Position, _Previous) :-
  Position.character = 0, fail.

prev_position(Position, Previous) :-
  PreviousCharacter is Position.character - 1,
  Previous = _{
    line: Position.line,
    character: PreviousCharacter
    }.

next_position(Position, Next) :-
  NextCharacter is Position.character + 1,
  Next = _{
    line: Position.line,
    character: NextCharacter
    }.

character_at_position(URI, Position, Character) :-
  content_at_position(URI, Position, 1, Character).

content_at_position(URI, Position, Length, Text) :-
  Line = Position.line,
  CharacterPos = Position.character,
  get_document_line_position(URI, Line, LineStart),
  Offset is LineStart + CharacterPos,
  with_content(URI, In,
    (
      seek(In, Offset, bof, _),
      read_string(In, Length, Text),
      (Text \= [])
      )
    ).

identifier_character(Character) :-
  letter(Character).

identifier_character(Character) :-
  digit(Character).

identifier_character("_").

digit(Character) :-
  Character >= "0", Character =< "9".

letter(Character) :-
  Character >= "a", Character =< "z".

letter(Character) :-
  Character >= "A", Character =< "Z".
