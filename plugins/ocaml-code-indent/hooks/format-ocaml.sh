#!/bin/bash
set -euo pipefail

# Read hook context from stdin
CONTEXT=$(cat)
FILE_PATH=$(echo "$CONTEXT" | jq -r '.tool_input.file_path // empty')

# Skip if no file path
if [[ -z "$FILE_PATH" ]]; then
  exit 0
fi

# Check if OCaml file
if [[ "$FILE_PATH" =~ \.(ml|mli)$ ]]; then
  # Check if ocp-indent is installed
  if ! command -v ocp-indent &> /dev/null; then
    echo "⚠️  ocaml-code-indent: ocp-indent not found. Install with: opam install ocp-indent" >&2
    exit 0
  fi

  # Format the file
  ocp-indent --inplace "$FILE_PATH" 2>&1 || true
fi
