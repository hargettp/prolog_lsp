:- begin_tests(language_client).

:- use_module(language_client).
:- use_module(language_server).

setup(stdio).

setup(stdio, Connection) :-
  stdio_language_connector(Connector),
  jsonrpc_connect(Connector, Connection).

teardown(stdio).

teardown(Type, Connection) :-
  jsonrpc_disconnect(Connection),
  teardown(Type).

test(initialize,[
    forall(member(Type, [stdio])), 
    setup(setup(Type, Connection)), 
    cleanup(teardown(Type, Connection))
    ]) :-
  call_method(Connection,initialize,[],_R).

test(echo,[
    forall(member(Type, [stdio])), 
    setup(setup(Type, Connection)), 
    cleanup(teardown(Type, Connection))
    ]) :-
  call_method(Connection,initialize,[],_R),
  call_method(Connection,echo,[],[]).


test(methods,[
    forall(member(Type, [stdio])), 
    setup(setup(Type, Connection)), 
    cleanup(teardown(Type, Connection))
    ]) :-
  call_method(Connection,initialize,[],_R),
  call_method(Connection,methods,[],_M).

:- end_tests(language_client).
