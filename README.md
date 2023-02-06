# Prolog Language Server

An implementation of the [Language Server Protocol](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/) written in Prolog to support a language server for Prolog applications.

This is written for [SWI-Prolog](https://swi-prolog.org), and employs predicates specific to that implementation.

## Getting Started

There are 2 implementations within this package:

* `stdio` - it reads request from `stdin` and writes responses out to `stdout`, with logging and diagnostics on `stderr`. Eventually, this would be the backend for IDE or editors plugins (e.g., in VS Code).
* `tcp` - it creates a server on the specified port and listens for new connections, handling requests and responses on each connection in separate threads.

To load using the SWI-Prolog manager and download the latest tagged version, in a SWI-Prolog console type:

  `pack_install(prolog_lsp)`

To load from a locally cloned copy of this repository:

  `pack_install('.')`

### Run stdio server

Once installed, running the following lines will start a `stdio` language server in the same process as the SWI-Prolog console. Note that this call will not return until the server exits, a signal is fired, or Ctrl-C is hit.

```prolog
use_module(library(prolog_lsp)).
run_stdio_language_server.
```

### Run tcp server

Similar to the `stdio` server, these lines will run the TCP server locally on the specified port. Note that this call will return once the server starts in a separate threads, so it becomes possible to interact with the server in the same process. The port in this example is 3101, but the choice is arbitrary and can be available port number.

```prolog
use_module(library(prolog_lsp)).
start_tcp_language_server(3101)
```

## Developing

For development, the easiest path may be to clone the git repository, then run either the explicit local package installation above, or use the `test.pl` to both load the package and run its unit tests.

`[test]`

## Status

This project is very immature and not at all recommended for daily production use. Happy to discuss contributions!