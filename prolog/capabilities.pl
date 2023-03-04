:- module(lsp_capabilities,[
  server_capabilities/1
  ]).

server_capabilities(Capabilities) :-
  Capabilities = _{
    textDocumentSync: _{
      openClose: true,
      % This means the client sends the full content on each change
      change: 1
    },
    workspaceSymbolProvider: true,
    documentSymbolProvider: true
  }.

