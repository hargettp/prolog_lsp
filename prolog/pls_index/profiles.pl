:- module(pls_index_profiles, [
  use_language_profile/1,
  ensure_profile_loaded/1,
  get_document_profile/2,
  set_document_profile/2,

  profile_index_term/4,
  profile_index_docs/5,
  profile_index_signature/5,
  profile_index_goal/6,

  index_goals/4,
  index_goal/4,

  functor_range/3,
  functor_range/4,
  term_position_range/3,
  term_range/4,
  argument_positions/2
]).

:- use_module(library(log4p)).
:- use_module(documents).

user:file_search_path(pls_language_profile,library(pls_language_profile)).

% 
% Interface to profiles
% 

% profile_index_term(Profile, URI, SubPos, Term)
:- multifile profile_index_term/4.

%! profile_index_docs(+Profile, +URI, +SubPos, Term, +CommentPos) is nondet.
%
% Index the documentation for the term at the indicated SubPos,
% using the CommentPos from an earlier `read_term/3` call.
%
:- multifile profile_index_docs/5.

% profile_index_signature(Profile, URI, SubPos, Term, Vars)
:- multifile profile_index_signature/5.

%! profile_index_goal(+Profile, ?URI, ?Caller, ?SubPos, ?Goal) is nondet.
% 
% Index a goal for cross-referncing
% 
:- multifile profile_index_goal/6.

%! use_language_profile(+Profile) is det.
%
% Directive to indicate which language profile to use when indexing
% source. The specified profile applies to the end of the file, or
% until the next such directive.
use_language_profile(_Profile).

%! profile_loaded(+Profile) is det.
%
% Dynamic predicate to indicate a profile has been loaded
:- dynamic profile_loaded/1.

profile_module_file(Profile, ProfileModuleFile) :-
  exists_source(pls_language_profile(Profile), ProfileModuleFile).

ensure_profile_loaded(Profile) :-
  profile_loaded(Profile), 
  !.

ensure_profile_loaded(Profile) :-
  once(profile_module_file(Profile, ProfileModuleFile)),
  ensure_loaded(ProfileModuleFile),
  assertz(profile_loaded(Profile)).

get_document_profile(URI, Profile) :-
  get_document_property(URI,profile(Profile)),
  !.

get_document_profile(_URI, base).

set_document_profile(URI, Profile) :-
  set_document_property(URI, profile(Profile)).

% 
% goals
% 
index_goals(URI, Caller, GoalPos, Goal) :-
  forall(index_goal(URI, Caller, GoalPos, Goal), true).

index_goal(URI, Caller, GoalPos, Goal) :-
  get_document_profile(URI, Profile),
  profile_index_goal(Profile, URI, Caller, GoalPos, Goal).

% 
%  -- position helpers --
% 

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

argument_positions(term_position(_From, _To, _FFrom, _FTo, Subpos), Subpos).