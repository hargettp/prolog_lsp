:- module(pls_index_profiles, [
  use_language_profile/1,
  ensure_profile_loaded/1,
  get_document_profile/2
]).

:- use_module(documents).

user:file_search_path(pls_language_profile,library(pls_language_profile)).

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