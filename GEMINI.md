# Project Overview

This project, `alsdiff`, is a command-line tool written in OCaml for working with Ableton Live Set (`.als`) files. It provides utilities to parse, analyze, and manipulate the structure of these files, which are essentially compressed XML. The primary goal of this project is to provide a way to use Git for version control of Ableton Live projects, by making the `.als` files more diff-friendly.

## Key Technologies

*   **OCaml:** The core language for the project.
*   **Dune:** The build system used for OCaml projects.
*   **xmlm:** A library for parsing and manipulating XML in OCaml.
*   **camlzip:** A library for working with compressed files, used to decompress the `.als` files.
*   **angstrom:** Parser combinators for Upath.
*   **eio:** Effects-based IO.
*   **alcotest:** Testing framework.
*   **yojson:** JSON parsing and serialization.
*   **ppx_deriving.eq:** PPX extension for deriving equality functions.
*   **ppx_deriving_jsonschema:** PPX extension for JSON schema generation.
*   **ppx_deriving_yojson:** PPX extension for Yojson serialization.
*   **fmt:** OCaml formatting library.

## Architecture

The project is structured into a library (`lib`) and a binary (`bin`).

*   **`lib`:** Contains the core logic for parsing and manipulating Ableton Live Sets, organized into sublibraries:
    *   `lib/base/` - Base functionality modules: `xml.ml`, `upath.ml`, `file.ml`, `equality.ml`, `diff.ml`.
    *   `lib/live/` - Ableton Live specific modules: `automation.ml`, `clip.ml`, `track.ml`, `device.ml`, `liveset.ml`.
    *   `lib/output/` - Output formatting and view models: `output.ml`, `text_renderer.ml`, `view_model.ml`.
*   **`bin`:** Contains the executable entry point.
    *   `alsdiff.ml`: The main executable.

# Building and Running

This project uses the `dune` build system.

## Building

To build the project, run the following command:

```bash
dune build
```

This will compile the source code and create an executable in the `_build` directory.

## Running

To run the main executable, use the following command:

```bash
dune exec alsdiff
```

To run specific tests, use commands like:

```bash
dune exec test/test_upath.exe
dune exec test/test_diff_list.exe
dune exec test/test_diff_automation.exe
```

## Testing

To run all tests, use the following command:

```bash
dune runtest
```

# Project Structure

- `bin/alsdiff.ml` - Main executable entry point
- `lib/` - Core library modules organized into sublibraries:
  - `lib/base/` - Base functionality modules:
    - `xml.ml` - XML parsing and data structures
    - `upath.ml` - XPath-like query language for XML
    - `file.ml` - File handling and .als file decompression
    - `equality.ml` - Equality checking utilities
    - `diff.ml` - Core diffing algorithms including Myers O(ND) implementation
  - `lib/live/` - Ableton Live specific modules:
    - `automation.ml` - Automation envelope handling
    - `clip.ml` - Clip and mixer functionality
    - `track.ml` - Track handling and management
    - `device.ml` - Device and plugin functionality
    - `liveset.ml` - LiveSet structure and locator handling
  - `lib/output/` - Output formatting:
    - `output.ml` - Output interface definitions
    - `text_renderer.ml` - Plain text output rendering
    - `view_model.ml` - Intermediate representation for UI rendering
- `plugins/` - Project plugins:
  - `ableton-test-generator`
  - `ocaml-code-indent`
  - `ocaml-lsp`
  - `security-data-leak-scanner`
  - `xml-element-extractor`
  - `zen-code-reviewer`
- `test/` - Test suites (all use specific module opens for cleaner code):
  - `test_upath.ml` - Tests for XPath-like functionality
  - `test_xml.ml` - Tests for XML parsing
  - `test_wildcard.ml` - Tests for wildcard matching
  - `test_complex.ml` - Complex integration tests
  - `test_diff_automation.ml` - Tests for diffing automation envelopes
  - `test_diff_list.ml` - Tests for list diffing algorithms (Myers)
  - `test_diff_mixer.ml` - Tests for mixer diffing functionality
  - `test_audio_clip.ml` / `test_midi_clip.ml` - Tests for clip functionality
  - `test_diff_audio_clip.ml` / `test_diff_midi_clip.ml` - Tests for clip diffing
  - `test_audio_track.ml` / `test_midi_track.ml` - Tests for track functionality
  - `test_diff_track.ml` - Tests for track diffing
  - `test_device.ml` - Tests for device functionality
  - `test_diff_device.ml` - Tests for device diffing
  - `test_real_devices.ml` - Tests with real Ableton device XMLs
  - `test_liveset.ml` - Tests for full LiveSet parsing and diffing
  - `utils.ml` - Shared test utilities

## Architecture Overview

This is a Git helper tool for Ableton Live Set (.als) files. The core functionality:

1.  **File Handling**: Decompress .als files (which are gzipped XML)
2.  **XML Processing**: Parse and navigate XML structure of Live sets
3.  **Upath**: Custom XPath-like query language for finding elements in XML
4.  **Diffing**: Compare Live set objects to detect changes with advanced algorithms
5.  **Output Formatting**: Format and display results via ViewModels

The `Upath` module provides a subset of XPath functionality with support for:

*   Tag names with attributes (`tag@attr="value"`)
*   Indexing with optional tag matching (`[0]`, `[1]`, `tag[0]`, `tag[1]`)
*   Wildcards (`*`, `**`)
*   Path navigation (`/tag1/tag2`)

The `Diff` module implements multiple diffing algorithms:

*   Ordered diffing for sequential data
*   Myers O(ND) algorithm for optimal diffing performance
*   Specialized diffing for automation envelopes in Live sets

## Library Organization

The project is organized into three main sublibraries and a wrapper library:

1.  **alsdiff_base** (`lib/base/`) - Core functionality and diffing algorithms
2.  **alsdiff_live** (`lib/live/`) - Ableton Live specific types and logic
3.  **alsdiff_output** (`lib/output/`) - Output formatting and view models
4.  **alsdiff_lib** (`lib/`) - Wrapper library combining the above

### Module Access Patterns

When working with the libraries, use specific module opens for cleaner code:

```ocaml
(* Base modules *)
open Alsdiff_base.Xml
open Alsdiff_base.Upath
open Alsdiff_base.Diff

(* Live modules *)
open Alsdiff_live.Automation
open Alsdiff_live.Clip
open Alsdiff_live.Track
open Alsdiff_live.Device
open Alsdiff_live.Liveset

(* Output modules *)
open Alsdiff_output.Text_renderer
```

This allows you to write `Automation.t` instead of `Alsdiff_lib_live.Automation.Automation.t` and `Xml.read_file` instead of `Alsdiff_lib_base.Xml.read_file`.

### Refactored Modules

The project has undergone significant refactoring:

*   `lib/diff/` sublibrary was merged into `lib/base/`.
*   Specialized patching logic (formerly `clip_patch.ml`, etc.) is now integrated into the respective modules' `Patch` submodule.
*   `live.ml` was split into `automation.ml`, `clip.ml`, `track.ml`, etc.

## Development Commands

*   `dune build @fmt` - Format code
*   `dune promote` - Promote generated files
*   `dune clean` - Clean build artifacts
*   `dune utop` - Load this library into Utop REPL
*   `dune utop . -- -emacs` - Load this library into Utop REPL for Emacs utop-mode
