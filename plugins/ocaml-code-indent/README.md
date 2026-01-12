# ocaml-code-indent

A Claude Code plugin that automatically formats OCaml source files using `ocp-indent` after every edit.

## Overview

This plugin runs `ocp-indent` automatically on `.ml` and `.mli` files whenever you edit them in Claude Code, ensuring your OCaml code is always properly indented according to official conventions.

## Prerequisites

- **ocp-indent**: Install via OPAM:
  ```bash
  opam install ocp-indent
  ```
- **jq**: JSON processor (usually pre-installed on most systems)

## Installation

1. Copy or clone this plugin to your desired location
2. Install the plugin:
   ```bash
   cc --plugin-dir /path/to/ocaml-code-indent
   ```

## Usage

Once installed, the plugin works automatically:

- Edit any `.ml` or `.mli` file in Claude Code
- The plugin will automatically format the file with `ocp-indent` after each edit
- No manual intervention required

## Troubleshooting

### "ocp-indent not found" message

If you see a warning that `ocp-indent` is not found, install it:

```bash
opam install ocp-indent
```

### Formatting not working

1. Verify `ocp-indent` is installed: `command -v ocp-indent`
2. Test `ocp-indent` manually:
   ```bash
   echo "let x=1 in x+2" > test.ml
   ocp-indent --inplace test.ml
   cat test.ml
   ```
3. Check that the plugin is enabled in Claude Code settings

### Only `.ml` and `.mli` files are formatted

The plugin only formats OCaml source files. Other file types are silently ignored.

## Configuration

The plugin respects your `.ocp-indent` configuration file for custom indentation settings. See `ocp-indent --help` for configuration options.

## License

This plugin is provided as-is for use with Claude Code.
