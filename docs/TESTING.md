# Testing Guide for Prolog LSP Server

## Test Structure

The Prolog LSP server includes comprehensive unit tests organized into categories:

### Test File: `prolog/language_client.plt`

This is the main test suite for the LSP server, testing both the client-server communication and the LSP method implementations.

## Test Categories

### 1. Initialization Tests

- **`initialize`**: Tests the LSP initialization handshake
  - Verifies that the server accepts `initialize` request
  - Confirms capabilities are returned
  - Tests state transition to "initializing" → "initialized"

- **`methods`**: Verifies all LSP methods are properly registered
  - Lists all available methods on the server
  - Ensures all expected methods are implemented

### 2. Utility Tests

- **`echo`**: Tests basic request/response communication
  - Sends an echo message and verifies it's returned correctly
  - Validates JSON-RPC communication layer

- **`shutdown` / `exit`**: Tests server lifecycle
  - Verifies state transition to "shutting_down"
  - Confirms invalid requests fail after shutdown
  - Tests proper server termination

### 3. Document Synchronization Tests

These tests verify that the server correctly tracks open documents and their changes:

- **`textDocument/didOpen`**: Opens a document
  - Sends a document to the server with content
  - Triggers indexing of the document
  - Verifies document is stored in-memory

- **`textDocument/didChange`**: Modifies an open document
  - Updates document content after opening
  - Triggers re-indexing of changed content
  - Verifies document state is updated

- **`textDocument/didClose`**: Closes an open document
  - Removes document from server memory
  - Clears associated indexes
  - Verifies resources are freed

### 4. Language Feature Tests

These tests exercise LSP language features using actual project files:

- **`textDocument/documentSymbol`**: Retrieves symbols from a document
  - Queries symbols (functions, predicates, modules) in a file
  - Returns structured symbol information with locations
  - Accepts both null (no symbols) and list results

- **`textDocument/hover`**: Provides hover information
  - Gets information about a symbol at a given position
  - Returns documentation or type information
  - Handles positions where no symbol exists (null result)

- **`textDocument/definition`**: Navigate to definition
  - Finds where a symbol is defined
  - Returns location(s) of the definition
  - Returns null if definition not found

- **`textDocument/references`**: Find all references
  - Locates all uses of a symbol
  - Can include or exclude the declaration
  - Returns list of locations where symbol is used

- **`textDocument/completion`**: Code completion
  - Provides completion suggestions at cursor position
  - Returns list of completion items
  - Returns null if no completions available

- **`workspace/symbol`**: Global symbol search
  - Searches for symbols across entire workspace
  - Uses query string to filter results
  - Supports workspace-wide refactoring and navigation

## Running Tests

### Run all tests
```bash
cd /Users/phil/src/prolog_lsp
swipl -s load.pl -g "load_files('prolog/language_client.plt'), run_tests(language_client), halt"
```

### Run specific test
```bash
swipl -s load.pl -g "load_files('prolog/language_client.plt'), run_tests(language_client:textDocument_hover_with_project_file), halt"
```

### Run with verbose output
```bash
swipl -s load.pl -g "set_prolog_flag(plunit_options, [verbose(true)]), load_files('prolog/language_client.plt'), run_tests(language_client), halt"
```

## Test Infrastructure

### Helper Predicates

The test file includes several helper predicates for constructing LSP data structures:

- **`init_params/1`**: Creates initialization parameters with workspace
- **`position/3`**: Constructs a `{line, character}` position
- **`range/5`**: Constructs a `{start, end}` range
- **`text_document/2`**: Creates a text document identifier
- **`text_document_item/5`**: Creates a complete text document with content
- **`content_change/2`**: Creates a content change event

### Setup/Teardown

Tests support both `stdio` and `tcp` server modes:
- **stdio**: Launches server as subprocess via stdio
- **tcp**: Starts server on port 3403 in separate thread

The test framework handles:
- Server startup/shutdown
- Connection management
- Stream cleanup
- Process management

## Test Data

Tests use:
1. **Project source code**: Actual LSP implementation files as indexed documents
2. **Test content**: Small Prolog modules for document sync tests
3. **Real workspace**: Indexes the entire project directory structure

## Known Issues & Notes

1. **Document sync notifications** (didOpen, didChange, didClose) may generate warnings
   - These are notifications (not requests), so errors don't fail the test
   - The server may not be fully processing all parameters currently
   - Ongoing development may improve handler robustness

2. **Indexing delay**: Tests include `sleep/1` calls
   - Allows asynchronous indexing to complete
   - Duration (0.2-0.5s) may need tuning on slow systems

3. **Position-based queries** may return null
   - Positions in the middle of tokens may not match indexed terms
   - This is expected behavior for unmapped positions

## Adding New Tests

To add a new language feature test:

```prolog
test(feature_name, [
    forall(member(Type, [stdio])), 
    setup(setup(Type, Connection)), 
    cleanup(teardown(Type, Connection))
    ]) :-
  % Initialize
  init_params(InitParams),
  call_method(Connection, initialize, InitParams, _),
  notify_method(Connection, initialized, _{}),
  sleep(0.5),
  
  % Your test code here
  ProjectFile = 'file:///Users/phil/src/prolog_lsp/prolog/methods.pl',
  text_document(ProjectFile, TextDoc),
  position(Line, Char, Pos),
  Params = _{textDocument: TextDoc, position: Pos},
  call_method(Connection, 'method/name', Params, Result),
  
  % Assertions
  (Result = null -> true ; is_list(Result)).
```

## Future Test Improvements

- [ ] Add tests for multiple open documents simultaneously
- [ ] Test workspace symbol filtering with various queries
- [ ] Add performance benchmarks for indexing large projects
- [ ] Test error handling for malformed documents
- [ ] Add integration tests with external LSP clients
- [ ] Test concurrent edits to same document
- [ ] Add test fixtures for more complex Prolog code
