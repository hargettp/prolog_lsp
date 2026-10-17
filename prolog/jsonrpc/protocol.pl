:- module(jsonrpc_protocol, [
  read_message/1,
  read_message/2,

  write_message/1,
  write_message/2,

  message_json/2
  ]).

:- use_module(library(json)).
:- use_module(library(json_convert)).
:- use_module(library(dcg/basics)).
:- use_module(library(prolog_stack)).
:- use_module(library(readutil)).

:- use_module(library(log4p)).

:- multifile on_message_read/1.
:- multifile on_message_write/1.

read_message(In, Message) :-
  setup_and_call_cleanup(
    (current_input(SaveIn), set_input(In)), 
    read_message(Message),
    set_input(SaveIn)
    ).
    
read_message(Message) :-
  % Note: not requiring that the jsonrpc key be present
  % with value 2.0
  catch(
    (
      read_header(Size),
      read_content(Size, Content),
      message_json(Message, Content)
      ),
    error(syntax_error(json(illegal_json)),_),
    fail
    ),
  ignore(call_read_message_hooks(Message)).

message_json(Message, Json) :-
  atom_json_dict(Json, Message, []).

write_message(Out,Message) :-
  with_output_to(Out,write_message(Message)).

write_message(Message0) :-
  ensure_jsonrpc_version(Message0, Message),
  message_json(Message, Content),
  string_length(Content, Size),
  write_content_length(Size),
  write_blank_line,
  write_content(Content),
  flush_output,
  ignore(call_write_message_hooks(Message)).
  
%  JSON-RPC 2.0 requires every message to carry "jsonrpc":"2.0"; strict
%  clients (e.g. Fresh) reject responses without it.
ensure_jsonrpc_version(Message0, Message) :-
  is_dict(Message0),
  \+ get_dict(jsonrpc, Message0, _),
  !,
  put_dict(jsonrpc, Message0, "2.0", Message).
ensure_jsonrpc_version(Message, Message).

read_blank_line(In) :-
  read_line_to_codes(In,Codes),
  Codes \== end_of_file,
  phrase(blank_line,Codes,[]).

%  Headers are read strictly forward, one line at a time: input may be
%  a pipe (stdio), which cannot seek, so we must never rewind.
%  Header lines are consumed through the terminating blank line (or EOF).
%  Content-Type (and any other header) is ignored; Content-Length is required.
read_header(Size) :-
  current_input(In),
  read_header(In, Size).

read_header(In, Size) :-
  read_header_lines(In, none, MaybeSize),
  MaybeSize = size(Size).

read_header_lines(In, Acc, Size) :-
  read_line_to_codes(In, Line),
  (  Line == end_of_file
  -> Size = Acc
  ;  phrase(blank_line, Line)
  -> Size = Acc
  ;  phrase(content_length(S), Line, _)
  -> read_header_lines(In, size(S), Size)
  ;  read_header_lines(In, Acc, Size)
  ).

read_content_type :-
  current_input(In),
  read_line_to_codes(In, Line),
  Line \== end_of_file,
  phrase(content_type, Line).

read_content_length(Size) :-
  current_input(In),
  read_line_to_codes(In, Line),
  Line \== end_of_file,
  phrase(content_length(Size), Line).

write_content_length(Size) :-
  format("Content-Length: ~w\r\n",[Size]).

read_blank_line :-
  current_input(In),
  read_blank_line(In).

write_blank_line :-
  format("\r\n").

read_content(Size, Content) :-
  current_input(In),
  read_string(In, Size, Content).

write_content(Content) :-
  format("~s",[Content]).

content_length(Size) -->
  "Content-Length",
  whites,
  ":",
  whites,
  digits(Digits),
  { string_codes(String, Digits), number_string(Size,String)}.

content_type -->
  "Content-Type",
  whites,
  ":",
  whites,
  remainder(_).

blank_line -->
  whites.

% --- hooks ---

call_read_message_hooks(Message) :-
  forall(
    catch_with_backtrace(
      ignore(jsonrpc_protocol:on_message_read(Message)),
      Error,
      print_message(error, Error)
      ),
    true
    ).

call_write_message_hooks(Message)  :-
  forall(
    catch_with_backtrace(
      ignore(jsonrpc_protocol:on_message_write(Message)),
      Error,
      print_message(error, Error)
      ),
    true
    ).
