# ocaml-lsp Plugin

LSP integration for OCaml via ocamllsp, providing real-time code intelligence for OCaml files in Claude Code.

## Features

- **Instant diagnostics**: See OCaml errors and warnings immediately after each edit
- **Code navigation**: Go to definition, find references, and hover information
- **Type information**: View type annotations and documentation for code symbols

## Prerequisites

Before using this plugin, you must install `ocamllsp`:

```bash
opam install ocaml-lsp-server
```

**Important**: Install `ocaml-lsp-server` in each opam switch where you want to use it.

## Installation

This plugin is designed for project-local installation. It's already located in `.claude-plugin/` at your project root.

To enable it, ensure your project's Claude Code settings reference the plugin:

```bash
# The plugin will be automatically discovered from .claude-plugin/
```

## Usage

Once installed, the LSP server automatically connects when you open `.ml` or `.mli` files in your OCaml project.

### Supported File Extensions

- `.ml` - OCaml implementation files
- `.mli` - OCaml interface files

## Configuration

The plugin uses the default `ocamllsp` configuration with no additional arguments. The LSP server communicates via stdio (standard input/output).

### Default Configuration

```json
{
  "lspServers": {
    "ocaml": {
      "command": "ocamllsp",
      "extensionToLanguage": {
        ".ml": "ocaml",
        ".mli": "ocaml"
      }
    }
  }
}
```

## Troubleshooting

### ocamllsp not found

If Claude Code cannot find `ocamllsp`:

1. Verify installation: `which ocamllsp`
2. Ensure you're using the correct opam switch: `opam switch`
3. Reinstall if needed: `opam install ocaml-lsp-server`

### LSP not starting

Enable LSP logging to debug issues:

```bash
claude --enable-lsp-logging
```

Logs will be written to `~/.claude/debug/`.

## Resources

- [ocaml-lsp GitHub Repository](https://github.com/ocaml/ocaml-lsp)
- [Language Server Protocol Specification](https://microsoft.github.io/language-server-protocol/)
- [Claude Code LSP Plugins Documentation](https://code.claude.com/docs/en/plugins-reference#lsp-servers)

## License

MIT
