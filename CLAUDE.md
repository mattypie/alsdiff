# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Structure

- `bin/main.ml` - Main executable entry point
- `lib/` - Core library modules organized into sublibraries:
  - `lib/base/` - Base functionality modules:
    - `xml.ml` - XML parsing and data structures
    - `upath.ml` - XPath-like query language for XML
    - `file.ml` - File handling and .als file decompression
    - `equality.ml` - Equality checking utilities
    - `diff.ml` - List diffing algorithms
  - `lib/live/` - Ableton Live specific modules:
    - `automation.ml` - Automation envelope handling
    - `clip.ml` - Clip and mixer functionality
    - `track.ml` - Track handling and management
    - `device.ml` - Device functionality (includes plugin, regular, and group devices)
    - `liveset.ml` - Live set management
    - `main_track.ml` - Main track handling
    - `mixer.ml` - Mixer functionality
  - `lib/output/` - Output formatting:
    - `output.ml` - Output interface definitions
    - `text_output.ml` - Plain text output rendering
- `test/` - Test suites (all use specific module opens for cleaner code):
  - `test_upath.ml` - Tests for XPath-like functionality
  - `test_xml.ml` - Tests for XML parsing
  - `test_wildcard.ml` - Tests for wildcard matching
  - `test_wildcard_debug.ml` - Debug tests for wildcard matching
  - `test_complex.ml` - Complex integration tests
  - `test_audio_clip.ml` - Tests for audio clip functionality
  - `test_midi_clip.ml` - Tests for MIDI clip functionality
  - `test_midi_track.ml` - Tests for MIDI track functionality
  - `test_device.ml` - Tests for device functionality
  - `test_plugin_device.ml` - Tests for plugin device functionality
  - `test_group_device.ml` - Tests for group device functionality
  - `test_m4l_device.ml` - Tests for Max for Live device functionality
  - `test_real_devices.ml` - Tests for real device implementations
  - `test_diff_automation.ml` - Tests for diffing automation envelopes
  - `test_diff_list.ml` - Tests for list diffing functionality
  - `test_diff_audio_clip.ml` - Tests for audio clip diffing
  - `test_diff_midi_clip.ml` - Tests for MIDI clip diffing
  - `test_diff_mixer.ml` - Tests for mixer diffing functionality
  - `test_clip_patch.ml` - Tests for clip patching functionality
  - `test_current_parent.ml` - Tests for current parent tracking
  - `test_regex_match.ml` - Tests for regex matching functionality
  - `utils.ml` - Shared test utilities

## Key Dependencies

- `xmlm` - XML parsing
- `camlzip` - Gzip decompression for .als files
- `angstrom` - Parser combinators for Upath
- `eio` - Effects-based IO
- `alcotest` - Testing framework
- `yojson` - JSON parsing and serialization
- `ppx_deriving.eq` - PPX extension for deriving equality functions
- `ppx_deriving_jsonschema` - PPX extension for JSON schema generation
- `ppx_deriving_yojson` - PPX extension for Yojson serialization
- `re` - Regular expression library
- `cmdliner` - Command line interface library

## Third-Party Dependency Repositories

- `camlzip` - https://github.com/xavierleroy/camlzip
- `xmlm` - https://github.com/dbuenzli/xmlm
- `angstrom` - https://github.com/inhabitedtype/angstrom
- `eio` - https://github.com/ocaml-multicore/eio
- `eio_main` - https://github.com/ocaml-multicore/eio
- `alcotest` - https://github.com/mirage/alcotest
- `ppx_deriving` - https://github.com/ocaml-ppx/ppx_deriving
- `ppx_deriving_yojson` - https://github.com/ocaml-ppx/ppx_deriving_yojson
- `ppx_deriving_jsonschema` - https://github.com/ahrefs/ppx_deriving_jsonschema
- `cmdliner` - https://github.com/dbuenzli/cmdliner
- `re` - https://github.com/ocaml/ocaml-re

## Architecture Overview

This is a Git helper tool for Ableton Live Set (.als) files. The core functionality:

1. **File Handling**: Decompress .als files (which are gzipped XML)
2. **XML Processing**: Parse and navigate XML structure of Live sets
3. **Upath**: Custom XPath-like query language for finding elements in XML
4. **Diffing**: Compare Live set objects to detect changes with advanced algorithms
5. **Patch Management**: Handle patches and modifications
6. **Output Formatting**: Format and display results

The `Upath` module provides a subset of XPath functionality with support for:
- Tag names with attributes (`tag@attr="value"`)
- Indexing with optional tag matching (`[0]`, `[1]`, `tag[0]`, `tag[1]`)
- Wildcards (`*`, `**`)
- Path navigation (`/tag1/tag2`)

The `Diff` module implements multiple diffing algorithms:
- Ordered diffing for sequential data
- Myers O(ND) algorithm for optimal diffing performance
- Specialized diffing for automation envelopes in Live sets
- Structured diffing with patch generation for complex data types

### Device Support

The project includes comprehensive support for Ableton Live devices:
- **Device Types**: Regular devices, Plugin devices (VST2, VST3, AUv2), and Group devices
- **Device Architecture**: Consolidated in device.ml with unified device type system
- **Device Mapping**: Type-safe mapping from XML to OCaml data structures
- **Real Device Support**: Support for actual Ableton Live device implementations
- **Test Coverage**: Extensive test suite covering all device types and edge cases
- **Diffing Support**: Advanced diffing algorithms for all device types with proper patch generation

## Library Organization

The project is organized into four main libraries:

1. **alsdiff_base** (`lib/base/`) - Core functionality
2. **alsdiff_live** (`lib/live/`) - Ableton Live specific types and logic
4. **alsdiff_output** (`lib/output/`) - Output formatting

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
open Alsdiff_live.Mixer

(* Output modules *)
open Alsdiff_output.Text_output.TextOutput
```

This allows you to write `Automation.t` instead of `Alsdiff_live.Automation.Automation.t` and `Xml.read_file` instead of `Alsdiff_base.Xml.read_file`.

## Development Commands

This is an OCaml project using Dune build system.

### Build and Test Commands
- `dune build` - Build the project
- `dune runtest` - Run all tests
- `dune runtest --force` - Force rerun all tests
- `dune exec alsdiff` - Run the main executable
- `dune exec test/test_upath.exe` - Run specific test `test/test_upath.ml` (Upath tests)

### Development Utilities
- `dune build @fmt` - Format code
- `dune promote` - Promote generated files
- `dune clean` - Clean build artifacts
- `dune utop` - Load this library into Utop REPL
- `dune utop . -- -emacs` - Load this library into Utop REPL for Emacs utop-mode

### PPX Code Inspection
To inspect code after PPX rewriting (e.g., to see generated equality functions from `[@@deriving eq]`):
- `dune describe pp lib/live/device.ml` - Show PPX-expanded code for a specific file\n\n## Git commits conventions

## Git commit message convention
- Using Conventional Commits, https://www.conventionalcommits.org/en/v1.0.0/
- Always adding the texts inside the code block to the end of the git commit message,
```

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude <noreply@anthropic.com>
```
