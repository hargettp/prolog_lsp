:- begin_tests(pls_index_indexing).

:- use_module(library(prolog_stack)).
:- use_module(library(uri)).

:- use_module(documents).
:- use_module(indexing).

expected_vs_actual([], []).

expected_vs_actual([Expected | ExpectedMore], [Actual | ActualMore]) :-
  Expected = Actual
    -> expected_vs_actual(ExpectedMore, ActualMore)
    ; (
      with_output_to(
        user_error, 
        writef("Expected: %w but found: %w\n", [Expected, Actual])
        ),      
      fail
      ).

expected_vs_actual(_, []) :- fail.

expected_vs_actual([], _) :- fail.

test(reading) :-
  read_file_to_string('./test.pl', Content, []),
  uri_file_name(URI, 'test.pl'),
  set_document_content(URI, Content),
  index_text(URI),
  findall(
    Name/Arity,
    (index_defined(URI, Definition, _How), functor(Definition, Name, Arity)),
    Definitions
    ),
  list_to_set(Definitions, Actual),
  Expected = [
    handle_connection/3,
    echo/3,
    crash/3,
    request_exit_server/1,
    message_json/2,
    server_method/3,
    find_handler/3,
    dispatch_exception/4,
    server_error/3,
    unknown_method/1,
    unknown_method/3,
    unknown_error/3,
    parse_error/1,
    invalid_request/1,
    jsonrpc_connect/2,
    jsonrpc_disconnect/1,
    with_connection/3,
    call_method/4,
    notify_method/3,
    expect_error/2,
    set_jsonrpc_log_directory/1,
    get_jsonrpc_log_directory/1,
    enable_jsonrpc_logging/1,
    enable_jsonrpc_logging/0,
    disable_jsonrpc_logging/0,
    clear_jsonrpc_logs/0,
    enable_jsonrpc_tracing/0,
    disable_jsonrpc_tracing/0,
    get_jsonrpc_tracing/1,
    set_jsonrpc_tracing/1,
    run_stdio_language_server/0,
    run_tcp_language_server/1,
    start_tcp_language_server/1,
    stop_tcp_language_server/1,
    stdio_language_connector/1,
    with_stdio_language/2,
    with_tcp_language/3,
    initialize/2,
    initialized/1,
    shutdown/2,
    exit/1,
    (:)/2,
    set_test_options/1,
    begin_tests/1,
    begin_tests/2,
    end_tests/1,
    run_tests/0,
    run_tests/1,
    load_test_files/1,
    running_tests/0,
    current_test/5,
    current_test_unit/2,
    test_report/1
    ],
  once(expected_vs_actual(Expected, Actual)).

test(index_roots) :-
  uri_file_name(URI, '.'),
  index_roots([URI]).

:- end_tests(pls_index_indexing).