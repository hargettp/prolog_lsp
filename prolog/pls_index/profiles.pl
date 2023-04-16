:- module(pls_index_profiles, [
  use_language_profile/1,
  provide_language_profile/1,
  register_language_profile/2,
  profile_module/2,
  ensure_profile_loaded/1,
  get_document_profile/2,
  set_document_profile/2,

  profile_index_term/4,
  profile_index_docs/5,
  profile_index_signature/5,
  profile_index_goal/5,

  index_goals/4,
  index_goal/4,

  functor_range/3,
  functor_range/4,
  term_position_range/3,
  term_range/4,
  argument_positions/2,
  symbol_kind/2
]).

:- use_module(library(log4p)).
:- use_module(documents).

user:file_search_path(pls_language_profile,library(pls_language_profile)).
user:file_search_path(pls_language_profile,LocalPath) :-
  working_directory(Cwd, Cwd),
  absolute_file_name(pls_language_profile,LocalPath, [relative_to(Cwd)]).

user:file_search_path(pls_language_profile,LocalPath) :-
  working_directory(Cwd, Cwd),
  absolute_file_name(prolog/pls_language_profile,LocalPath, [relative_to(Cwd)]).

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
:- multifile profile_index_goal/5.

%! profile_end_of_file(Profile, URI) is nondet.
%
% Allow the profile to cleanup after indexing a file.
%
:- multifile profile_end_of_file/2.

%! profile_symbol(+Profile, +URI, +Query, ?Range, ?Name, ?Detail, ?Kind) is nondet.
%
% Return details about a symbol in the indicated document
%
:- multifile profile_symbol/7.

%! use_language_profile(+Profile) is det.
%
% Directive to indicate which language profile to use when indexing
% source. The specified profile applies to the end of the file, or
% until the next such directive.
use_language_profile(_Profile) :- true.

:- meta_predicate use_language_profile(:).

%! profile_loaded(+Profile) is det.
%
% Dynamic predicate to indicate a profile has been loaded
:- dynamic profile_loaded/1.

%! provide_language_profile(+Profile) is det.
%
% A no-op in regular code, but is a single to the Prolog
% Language server during indexing that the file being indexed
% implements the indicated language profile.  
%
provide_language_profile(_Profile).

%! register_language_profile(+Profile, +ProfileModuleFile) is det.
%
% Register the indicated module file as an implementation of the
% indicated profile.
%
register_language_profile(Profile, ProfileURI) :-
  registered_language_profile(Profile, ProfileURI),
  info("Language profile %w in %w already registered",[Profile, ProfileURI]).

register_language_profile(Profile, ProfileURI) :-
  info("Registering language profile %w in %w",[Profile, ProfileURI]),
  assertz(registered_language_profile(Profile, ProfileURI)).

%! registered_language_profile(+Profile, +ProfileModuleFile) is det.
%
% Used to record that the indicated module file implements the
% specified language profile. The Prolog Language Server uses this
% track registration through register_language_profile/1, and
% dyamically load the profile from index source.
%
:- dynamic registered_language_profile/2.

profile_module(Profile, Module) :-
  profile_module_file(Profile, ProfileURI),
  get_document_item(ProfileURI, _Range, module(Module, _Exports)),
  info("Profile %w is in module %w",[Profile, Module]),
  !.

profile_module(_Profile, Module) :-
  Module = pls_language_profile_base.

profile_module_file(Profile, ProfileURI) :-
  registered_language_profile(Profile, ProfileURI).

profile_module_file(Profile, ProfileURI) :-
  exists_source(pls_language_profile(Profile), ProfileURI).

ensure_profile_loaded(Profile) :-
  profile_loaded(Profile), 
  !.

ensure_profile_loaded(Profile) :-
  once(profile_module_file(Profile, ProfileURI)),
  uri_file_name(ProfileURI, ProfileModuleFile),
  ensure_loaded(ProfileModuleFile),
  assertz(profile_loaded(Profile)).

get_document_profile(URI, Profile) :-
  get_document_property(URI,profile(Profile)),
  !.

get_document_profile(_URI, base).

set_document_profile(URI, Profile) :-
  set_document_property(URI, profile(Profile)).

reindex_for_profile(base).

reindex_for_profile(Profile) :-
  forall(
    get_document_profile(URI, Profile),
    % This is a little sketch, as we
    % are explicitly calling to avoid circular references
    (
      info("Reindexing %w for profile %w",[URI, Profile]),
      pls_index_indexing:index_text(URI)
      )
    ).

% f
% goals
% 
index_goals(URI, Caller, GoalPos, Goal) :-
  forall(index_goal(URI, Caller, GoalPos, Goal), true).

index_goal(URI, Caller, GoalPos, Goal) :-
  get_document_profile(URI, Profile),
  try_profile_index_goal(Profile, URI, Caller, GoalPos, Goal).

try_profile_index_goal(Profile, URI, Caller, GoalPos, Goal) :-
  pls_index_profiles:profile_index_goal(Profile, URI, Caller, GoalPos, Goal),
  !.

try_profile_index_goal(_Profile, URI, Caller, GoalPos, Goal) :-
  pls_index_profiles:profile_index_goal(base, URI, Caller, GoalPos, Goal).

% 
%  -- position helpers --
% 

%! functor_range(+URI, +Pos, -Range) is det.
%
% Given the position of a term, return the range for the functor.
%
functor_range(URI, term_position(_From, _To, FFrom, FTo, _Subpos), Range) :-
  term_range(URI, FFrom, FTo, Range).

functor_range(URI, FFrom, FTo, Range) :-
  term_range(URI, FFrom, FTo, Range).

term_position_range(URI, term_position(From, To, _FFrom, _FTo, _Subpos), Range) :-
  term_range(URI, From, To, Range).

%! term_range(+URI, +From, +To, -Range) is det.
%
% Given a From position and To position in the text of the document
% specified by URI, return a Ranage, which expresses as a start / end 
% pair of locations. Each location has a line number and character offset.
% 
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

%! symbol_kind(+Kind, -Code) is det.
%
% Transalte a known symbol kind into a pre-defined code.
%
symbol_kind(file, 1).
symbol_kind(module, 2).
symbol_kind(namespace, 3).
symbol_kind(package, 4).
symbol_kind(class, 5).
symbol_kind(method, 6).
symbol_kind(property, 7).
symbol_kind(field, 8).
symbol_kind(constructor, 9).
symbol_kind(enum, 10).
symbol_kind(interface, 11).
symbol_kind(function, 12).
symbol_kind(variable, 13).
symbol_kind(constant, 14).
symbol_kind(string, 15).
symbol_kind(number, 16).
symbol_kind(boolean, 17).
symbol_kind(array, 18).
