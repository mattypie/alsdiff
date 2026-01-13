# ALSDiff Agent Instructions

## Build, Lint, and Test Commands

### Core Commands
- `dune build` - Build the entire project
- `dune runtest` - Run all tests
- `dune runtest --force` - Force rerun all tests
- `dune build @fmt` - Format code with OCamlFormat
- `dune exec alsdiff -- <file1.als> <file2.als>` - Run main CLI

### CLI Options
- `alsdiff FILE1.als FILE2.als` - Compare two ALS files
- `--preset PRESET` - Use output preset (compact, full, midi, quiet, verbose)
- `--config FILE` - Load configuration from JSON file
- `--dump-schema [FILE]` - Dump JSON schema for configuration
- `--validate-config FILE` - Validate a config file against schema
- `--prefix-added/removed/modified/unchanged` - Custom change prefixes
- `--note-name-style Sharp|Flat` - Note naming convention
- `--max-collection-items N` - Limit collection output size

### Running Single Tests
- `dune exec test/test_upath.exe` - Run specific test (upath)
- `dune exec test/test_xml.exe` - Run specific test (xml)
- `dune exec test/test_device.exe` - Run specific test (device)
- `dune exec test/test_view_model.exe` - Run view model tests
- `dune exec test/test_text_renderer.exe` - Run text renderer tests
- General pattern: `dune exec test/test_<module>.exe`

### Development Utilities
- `dune clean` - Clean build artifacts
- `dune promote` - Promote generated files
- `dune utop` - Load library into Utop REPL
- `dune describe pp lib/<path>.ml` - Inspect PPX-generated code

## Code Style Guidelines

### Imports and Module Opens
**Library code** (`lib/`): Use opens at top of files for cleaner code
```ocaml
open Alsdiff_base
open Alsdiff_base.Diff
open Alsdiff_live
open Alsdiff_output
```

**Test code** (`test/`): Use specific opens
```ocaml
open Alsdiff_base.Xml
open Alsdiff_base.Upath
open Test_utils.Utils
```

For submodules (e.g., from `lib/live/track.ml`):
```ocaml
open Alsdiff_live.Track.Routing
open Alsdiff_live.Track.Mixer
open Alsdiff_live.Track.MidiTrack
```

### Formatting
- Line length: 100 characters
- Format command: `dune build @fmt`
- Configured in `.ocamlformat` - do not modify without understanding impact
- Use functional style, avoid mutable state

### Type Definitions and PPX
- Always include PPX derives: `[@@deriving eq|yojson|jsonschema]`
- Use `[@@deriving eq]` for equality checking
- Use phantom types for type safety (e.g., `type atomic`, `type structured`)
- Keep type definitions concise with inline comments

### Naming Conventions
- Modules: `module Foo` in `foo.ml` (standard OCaml convention)
- Functions: snake_case for multi-word names (e.g., `get_attr`, `parse_route_type`)
- Types: PascalCase, module-specific suffixes for variants (e.g., `route_type`, `atomic_patch`)
- Record fields: snake_case
- Exception types: CamelCase with `exception` keyword (e.g., `exception Xml_error of t * string`)

### Error Handling
- Define custom exceptions: `exception Xml_error of t * string`
- Use `raise (Exception_name (context, "message"))` pattern
- Provide descriptive error messages with context
- Use `failwith` for programmer errors (e.g., type mismatches)
- Use Option types (`get_attr_opt`) for optional values

### Record Pattern Matching
```ocaml
match xml with
| Element { name; childs; _ } -> (* use name and childs *)
| Data _ as xml -> raise (Xml_error (xml, "Cannot get children from Data node"))
```

### List Operations
- Use `List.map`, `List.filter`, `List.iter` for transformations
- Use `List.sort` with `String.compare` for consistent ordering
- Prefer `Option.bind` over manual `match` for option chaining
- Use `Fiber.pair` for parallel operations (Eio concurrency)

### Test Structure
- Tests in `test/` directory with pattern `test_<module>.ml`
- Use Alcotest framework: `Alcotest.check`, `let test_cases = [...]`
- Define testable types: `let foo_testable = Alcotest.(pair string int)`
- Test helpers in `test_utils` module
- Keep tests focused and independent

### MainTrack Singleton Pattern
`MainTrack` is a singleton (master output track):
- No `id` field
- `has_same_id _ _ = true`
- `id_hash _ = Hashtbl.hash 0`
- Skip ID validation in diff functions

### Concurrency (Eio)
- Use `Eio_main.run` in entry points
- Domain manager: `Eio.Stdenv.domain_mgr env`
- Parallel work: `Eio.Domain_manager.run domain_mgr (fun () -> ...)`
- Preserve `Eio_main.run` structure when modifying parallel workloads

## Architecture

### Library Structure
- `lib/base/` (alsdiff_base) - Core utilities
  - `file.ml` - ALS file decompression and loading
  - `xml.ml` - XML parsing and manipulation
  - `upath.ml` - μpath query language (XPath-like syntax for XML)
  - `equality.ml` - Equality and identity traits (EQUALABLE, IDENTIFIABLE)
  - `diff.ml` - Diff infrastructure with phantom types (atomic/structured changes)
- `lib/live/` (alsdiff_live) - Ableton Live types
  - `automation.ml` - Automation envelopes and events
  - `clip.ml` - MIDI and Audio clips (MidiClip, AudioClip, Loop, MidiNote, SampleRef)
  - `device.ml` - All device types (Regular, Plugin, Max4Live, Group)
  - `track.ml` - Track types (MidiTrack, AudioTrack, MainTrack, Routing, Mixer, Send)
  - `liveset.ml` - Top-level LiveSet structure (Version, Locator, pointees)
- `lib/output/` (alsdiff_output) - Output formatting
  - `output.ml` - Output interface definition
  - `view_model.ml` - View model types and builders (Field, Item, Collection, ViewBuilder)
  - `text_renderer.ml` - Text rendering with detail configs and JSON schema support
- `bin/` - CLI entry point (alsdiff.ml with cmdliner)

### Device Hierarchy
All device types in `lib/live/device.ml`:
- `RegularDevice` - Built-in Ableton effects (Compressor, EQ8, etc.)
- `PluginDevice` - External plugins (VST2, VST3, AUv2)
- `GroupDevice` - Nested device chains with branches
- `Max4LiveDevice` - Max for Live devices
- Supporting types: `PluginDesc`, `PluginParam`, `GenericParam`, `MIDIMapping`, `PresetRef`, `PatchRef`, `Macro`, `Snapshot`, `Branch`, `MixerDevice`

### Track Types (in `lib/live/track.ml`)
- `Routing` / `RoutingSet` - I/O routing (singleton pattern)
- `Send` - Track-to-track sends
- `Mixer` - Volume/pan/mute/solo
- `MidiTrack` / `AudioTrack` / `GroupTrack` / `ReturnTrack` - Standard tracks
- `MainMixer` / `MainTrack` - Master output (singleton pattern)

### View Model Pattern (in `lib/output/view_model.ml`)
Unified view type system for output rendering:
```ocaml
type view = Field of field | Item of item | Collection of collection
```
- `ViewBuilder` module with combinators for building views
- `unified_field_spec` for declarative field definitions
- Domain types enumeration (DTLiveset, DTTrack, DTDevice, etc.)

### JSON Configuration Support
Config auto-discovery order:
1. CLI `--config FILE`
2. CLI `--preset PRESET`
3. `.alsdiff.json` in git repository root
4. `.alsdiff.json` in `$HOME`
5. `quiet` preset (default)

Detail levels: `DLNone`, `Summary`, `Compact`, `Full`
Presets: `compact`, `full`, `midi_friendly`, `quiet`, `verbose`

## Do / Don't

### Do
- Run `dune build` and `dune runtest` after behavioral changes
- Add focused tests in `test/` for new functionality
- Follow existing patterns for similar constructs
- Use `[@@deriving ...]` annotations
- Format code with `dune build @fmt` before committing
- Use `dune describe pp` to inspect PPX output if needed

### Don't
- Modify code in `plugins/` without explicit instruction
- Remove `[@@deriving ...]` annotations without understanding PPX impact
- Use mutable state or imperative patterns
- Add code comments unless explicitly requested
- Change `.ocamlformat` configuration
- Break singleton pattern for `MainTrack`
