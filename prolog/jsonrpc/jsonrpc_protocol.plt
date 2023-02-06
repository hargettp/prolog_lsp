:- begin_tests(jsonrpc_protocol).
:- use_module(jsonrpc_protocol).
:- use_module(library(http/json)).

with_input_from(String, Module:Goal) :-
  setup_and_call_cleanup(
    open_string(String, In),
    setup_and_call_cleanup(
      ( current_input(SaveIn), set_input(In) ),
      Module:Goal, 
      set_input(SaveIn)
      ),
    close(In)
    ).

  test(read_content_length) :-
    with_input_from(
      "Content-Length: 0\r\n",
      jsonrpc_protocol:read_content_length(0)
      ).

  test(read_any_content_type) :-
    with_input_from(
      "Content-Type: something/somehow\r\n",
      jsonrpc_protocol:read_content_length(0)
      ).

  test(read_header1) :-
    with_output_to(
      string(Header),
      (
        format("Content-Length: 0\r\n"),
        format("Content-Type: something/somehow\r\n")
        )
      ),
    with_input_from(
      Header,
      jsonrpc_protocol:read_header(0)
      ).

  test(read_header2) :-
    with_output_to(
      string(Header),
      (
        format("Content-Type: something/somehow\r\n"),
        format("Content-Length: 0\r\n")
        )
      ),
    with_input_from(
      Header,
      jsonrpc_protocol:read_header(0)
      ).

  test(read_header3) :-
    with_output_to(
      string(Header),
      (
        format("Content-Length: 0\r\n")
        )
      ),
    with_input_from(
      Header,
      jsonrpc_protocol:read_header(0)
      ).

  test(read_message) :-
    atom_json_dict(Content, _{ jsonrpc: "2.0", foo: 1 }, []),
    string_length(Content, Size),
    with_output_to(
      string(Payload),
      (
        format("Content-Length: ~w\r\n",[Size]),
        format("\r\n"),
        format("~s",Content)
        )
      ),
    with_input_from(
      Payload,
      jsonrpc_protocol:read_message(_Msg)
      ).

  test(write_read_message) :-
    Msg = _{jsonrpc: "2.0", foo: 1, bar: "baz"},
    with_output_to(
      string(Payload), 
      write_message(Msg)
      ),
    
    with_input_from(
      Payload,
      jsonrpc_protocol:read_message(Msg)
      ).

  test(parse_error) :-
    RawMessage = '{ "jsonrpc" : "2.0", "id" : 1, "method", "echo", "params": [] }',
    % read_message should fail
    \+with_input_from(
      RawMessage,
      jsonrpc_protocol:read_message(_Msg)
      ).

  % Not testing for this at this time
  % test(invalid_request) :-
  %   RawMessage = '{ "id" : 1, "method" : "echo", "params": [] }',
  %   catch(
  %     ( with_input_from(
  %       RawMessage,
  %       jsonrpc_protocol:read_message(Msg)
  %       ), 
  %       format("Message: ~w~n", [Msg]),
  %       fail
  %       ),
  %     error(syntax_error(json(illegal_json)),_),
  %     true
  %     ).

    
:- end_tests(jsonrpc_protocol).