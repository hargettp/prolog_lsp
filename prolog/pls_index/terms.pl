:- module(pls_index_terms, [
  index_terms/1
  ]).

:- use_module(library(http/json)).
:- use_module(library(log4p)).
:- use_module(library(prolog_stack)).

:- use_module(library(prolog_source)).

:- use_module(documents).
:- use_module(docs).
:- use_module(profiles).

% Index the terms in a file, including subterms.
% A file or document is a sequence of terms, and
% terms contains goals.
index_terms(URI) :-
  clear_document_items(URI),
  forall(index_term(URI), true).

%! index_term(+URI) is nondet.
%
% On succesive calls, read and index every time in the source at URI.
%
index_term(URI) :-
  with_content(URI, In, (
    repeat,
    get_document_profile(URI, Profile),
    profile_module(Profile, ProfileModule),
    ( ProfileModule = pls_language_profile_base
      -> true
      ; info("Using profile %w in %w for %w",[Profile, ProfileModule, URI])
      ),
    read_term(In, Term, [
      syntax_errors(dec10),
      subterm_positions(SubPos), 
      module(ProfileModule),
      comments(CommentPos),
      variable_names(Vars)
      ]),
    ( Term \== end_of_file
      -> process_term(URI, SubPos, Term, CommentPos, Vars)
      ; (try_process_end_of_file(URI), !, fail)
      )
    )).

process_term(URI, _SubPos, (:- use_language_profile(Profile)), _CommentPos, _Vars) :-
  profile_module(Profile, ProfileModule),
  info("Using profile %w for %w in %w",[Profile, URI, ProfileModule]),
  set_document_profile(URI, Profile),
  !.

process_term(URI, SubPos, Term, CommentPos, Vars) :-
  get_document_profile(URI, Profile),
  ensure_profile_loaded(Profile),
  try_profile_index_term(Profile, URI, SubPos, Term),
  try_profile_index_docs(Profile, URI, SubPos, Term, CommentPos),
  try_profile_index_signature(Profile, URI, SubPos, Term, Vars),
  !.

try_profile_index_term(Profile, URI, SubPos, Term) :-
  pls_index_profiles:profile_index_term(Profile, URI, SubPos, Term),
  !.

try_profile_index_term(_Profile, URI, SubPos, Term) :-
  pls_index_profiles:profile_index_term(base, URI, SubPos, Term).

try_profile_index_docs(Profile, URI, SubPos, Term, CommentPos) :-
  pls_index_profiles:profile_index_docs(Profile, URI, SubPos, Term, CommentPos),
  !.

try_profile_index_docs(_Profile, URI, SubPos, Term, CommentPos) :-
  pls_index_profiles:profile_index_docs(base, URI, SubPos, Term, CommentPos).

try_profile_index_signature(Profile, URI, SubPos, Term, Vars) :-
  pls_index_profiles:profile_index_signature(Profile, URI, SubPos, Term, Vars),
  !.

try_profile_index_signature(_Profile, URI, SubPos, Term, Vars) :-
  pls_index_profiles:profile_index_signature(base, URI, SubPos, Term, Vars).

try_process_end_of_file(URI) :-
  get_document_profile(URI, Profile),
  ensure_profile_loaded(Profile),
  pls_index_profiles:profile_end_of_file(Profile, URI),
  !.

try_process_end_of_file(URI) :-
  pls_index_profiles:profile_end_of_file(base, URI).