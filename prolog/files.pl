:- module(files, [
  directory_file/2
  ]).

directory_file(Directory,File) :-
  directory_files(Directory,Entries),
  member(Entry,Entries),
  visible_entry(Entry),
  normalize_directory(Directory,Normalized),
  atom_concat(Normalized,Entry,Path),
  (exists_file(Path) ->
    File = Path ;
    directory_file(Path,File)).

visible_entry(Name) :-
  \+atom_chars(Name,['.'|_]).

normalize_directory(Directory,Normalized) :-
  ends_with_char(Directory,'/'),
  Normalized = Directory, !.

normalize_directory(Directory,Normalized) :-
  atom_concat(Directory,'/',Normalized).

ends_with_char(String,Char) :-
  char_code(Char,Code),
  string_length(String,Length),
  string_code(Length,String,Code).
