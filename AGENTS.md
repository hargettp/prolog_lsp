# AGENTS.md for Prolog LSP Server

## 1. Project Overview

This project is a **Prolog Language Server Protocol (LSP) server** implemented in Prolog. Its primary goal is to provide intelligent language support features (such as auto-completion, go-to-definition, diagnostics, etc.) for Prolog code.

## 2. Architecture

The Prolog LSP server is structured in modular layers, each with distinct responsibilities:

### Core Layers

*   **Entry Point (`language_server.pl`):** Provides two modes of operation:
    - `run_stdio_language_server/0`: Serves LSP clients via stdin/stdout
    - `run_tcp_language_server/1`: Runs a TCP server on a specified port

*   **JSON-RPC Communication Layer (`jsonrpc/`):**
    - `protocol.pl`: Parses and serializes JSON-RPC 2.0 messages
    - `server.pl`: Routes requests to handlers, manages connections, and formats responses
    - `connectors/stdio.pl` and `connectors/tcp.pl`: Pluggable transport mechanisms
    - `methods.pl` and `hooks.pl`: Registry system for binding LSP methods to Prolog predicates

*   **LSP Method Handlers (`methods.pl` and supporting modules):**
    - Implements the complete LSP specification (initialize, textDocument/*, workspace/*, etc.)
    - Manages server lifecycle state (initializing → initialized → shutting_down)
    - Coordinates between JSON-RPC layer and language feature modules
    - Includes modules: `capabilities.pl`, `hover.pl`, `symbols.pl`, `errors.pl`

### Language Intelligence Layer

*   **Document Management (`pls_index/documents.pl`):**
    - Maintains in-memory store of open documents with URI, language, version, and content
    - Tracks document items (ranges and values) for cross-references, hover info, definitions, etc.
    - Provides document properties and line position tracking

*   **Indexing System (`pls_index/indexing.pl`, `terms.pl`, `lines.pl`):**
    - Uses SWI-Prolog's `xref_source` for semantic analysis of Prolog source
    - Indexes workspace roots (project, SWI-Prolog library, installed packages) asynchronously
    - Builds term indexes and line position mappings for fast lookups
    - Triggered on document open/change events and during initialization

*   **Language Features (`pls_index/definitions.pl`, `references.pl`, `completions.pl`, `docs.pl`, `profiles.pl`):**
    - Implement individual LSP features (hover, go-to-definition, find-references, completion)
    - Query the document and term indexes to provide language intelligence
    - Support language profiles for different Prolog dialects or configurations

## 3. Project Structure

`pack.pl` - Provides package metadata
`load.pl` - Helper module for loading the project in a `swipl` console: just enter `[load]` in the console
`test.pl` - Contains help for running unit tests defined in `*.plt` files within the project; just enter `[test]` in a `swipl` console
`prolog/` - All project source files
`prolog/jsonrpc/` - Defines the basic JSONRPC protocol used in LSP
`docs/` - documents useful for agents or developers to understand the project
`specs` - specs for features and fixes implemented for this project`

## 4. Development Guidelines

1.  **Prolog Idioms:** All core logic and parsing should adhere to idiomatic Prolog practices.
2.  **AST First:** Design the system around a solid AST to decouple semantic analysis from the parsing mechanism.
3.  **LSP Compliance:** Strictly follow the [LSP specification](https://microsoft.github.io/language-server-protocol/overviews/lsp/overview/) for request/response formats.
4.  **Test Coverage:** Every core component (Parser, Semantic Analyzer, Diagnostics) must have comprehensive unit tests. Run this command to execute unit tests:
    ```bash
    swipl -s test
    ```
5.  **Modularity:** Maintain a high degree of modularity so that agents can be developed, tested, and swapped out independently.

## 5. Known Knowledge Base (LLM Wiki Integration)

*   **[To be populated]:** Document initial design decisions, specific Prolog parsing techniques chosen, and architectural trade-offs.
*   **[To be populated]:** Define the specific Prolog features (e.g., metaprogramming capabilities) that the LSP server must support.
