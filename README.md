# alsdiff

![Build status](https://github.com/krfantasy/alsdiff/actions/workflows/build.yml/badge.svg)
![GitHub Release](https://img.shields.io/github/v/release/krfantasy/alsdiff)
[![License](https://img.shields.io/badge/license-THE_LICENSE-magenta)](https://github.com/krfantasy/the-license)

<details>
<summary>README by Claude Opus 4.5</summary>

A semantic diff tool for Ableton Live Set (.als) files that makes version control meaningful for music producers.

## The Problem

Ableton Live Set files are gzip-compressed XML. Standard `git diff` treats them as binary blobs:

```
Binary files a/project.als and b/project.als differ
```

This makes version control nearly useless for tracking changes in music projects.

## The Solution

alsdiff parses the internal structure of .als files and produces human-readable diffs showing actual musical changes:

```
* LiveSet
  * Tracks
    * MidiTrack: "Lead Synth"
      * DeviceChain
        + Compressor2
        * Eq8
          * Bands
            * Band.4
              * Freq: 2500.0 -> 3200.0
              * Gain: 0.0 -> 2.5
      * Clips
        * MidiClip: "Verse"
          * Notes (3 Added, 1 Removed)
```

## Features

- Semantic diff output showing actual musical changes
- Git integration as external diff driver
- Configurable output verbosity (quiet to verbose)
- Built-in presets for different workflows
- JSON configuration with schema validation
- Parallel file loading for performance
- MIDI note rendering with configurable sharp/flat notation

## Installation

### Pre-built Binaries

Download from [GitHub Releases](https://github.com/krfantasy/alsdiff/releases):

```bash
# macOS
chmod +x alsdiff-macos-*
mv alsdiff-macos-* /usr/local/bin/alsdiff

# Or to ~/bin
mkdir -p ~/bin
mv alsdiff-macos-* ~/bin/alsdiff
echo 'export PATH="$HOME/bin:$PATH"' >> ~/.zshrc
source ~/.zshrc
```

### Build from Source

Requires OCaml 5.3.0+ and opam:

```bash
# Install dependencies
opam install . --deps-only

# Build
dune build

# Install
dune install

# Or run directly
dune exec alsdiff -- file1.als file2.als
```

## Quick Start

```bash
# Compare two .als files
alsdiff old-version.als new-version.als

# Use a preset for more detail
alsdiff old.als new.als --preset full

# Quiet mode for CI/scripts
alsdiff old.als new.als --preset quiet
```

## Git Integration

### Quick Setup

Run from your music project directory:

```bash
curl -sSL https://raw.githubusercontent.com/krfantasy/alsdiff/master/setup-git.sh | bash
```

### Manual Setup

1. Add to `.gitattributes` in your repository:

```
*.als diff=alsdiff
```

2. Configure git (global):

```bash
git config --global diff.alsdiff.command "alsdiff"
```

3. Now `git diff` works with .als files:

```bash
git diff                      # Shows semantic diff for staged .als files
git diff HEAD~1 -- song.als   # Compare with previous commit
git log -p -- song.als        # Show history with diffs
```

## Configuration

### Presets

| Preset | Description | Use Case |
|--------|-------------|----------|
| `quiet` | Minimal output, change counts only | CI/CD, quick checks |
| `compact` | Structure overview with summaries | Daily workflow |
| `inline` | Single-line field display | Compact yet informative |
| `full` | All details, multiline format | Code review |
| `verbose` | Everything including unchanged | Debugging, audits |
| `mixing` | Optimized for mixer changes | Audio engineering |
| `composer` | MIDI composition focus | Music composition |

```bash
alsdiff old.als new.als --preset compact
```

### Config File Discovery

alsdiff automatically discovers configuration in this order:

1. `--config FILE` (CLI argument)
2. `--preset NAME` (CLI argument)
3. `.alsdiff.json` in the .als file's directory
4. `.alsdiff.json` in git repository root
5. `.alsdiff.json` in `$HOME`
6. `quiet` preset (default)

### Custom Configuration

Create `.alsdiff.json`:

```json
{
  "$schema": "https://raw.githubusercontent.com/krfantasy/alsdiff/master/docs/config.schema.json",
  "added": ["Inline"],
  "removed": ["Summary"],
  "modified": ["Full"],
  "unchanged": ["Ignore"],
  "max_collection_items": 50,
  "type_overrides": [
    {
      "domain_type": ["DTTrack"],
      "override": {
        "added": ["Full"],
        "modified": ["Inline"]
      }
    },
    {
      "domain_type": ["DTNote"],
      "override": {
        "added": ["Summary"],
        "removed": ["Summary"]
      }
    }
  ]
}
```

### Detail Levels

| Level | Description |
|-------|-------------|
| `Ignore` | Hide completely |
| `Summary` | Name + change symbol + count |
| `Compact` | Name + change symbol only |
| `Inline` | Name + fields on single line |
| `Full` | Name + all fields (multiline) |

### Domain Types

Configuration can be customized per domain type:

- `DTLiveset` - Top-level project
- `DTTrack` - All track types
- `DTDevice` - Devices and plugins
- `DTClip` - MIDI and audio clips
- `DTNote` - MIDI notes
- `DTAutomation` - Automation envelopes
- `DTEvent` - Automation events
- `DTOther` - Everything else

## CLI Reference

```
alsdiff [OPTIONS] FILE1.als FILE2.als

OPTIONS:
  --preset NAME           Use output preset (compact, full, quiet, etc.)
  --config FILE           Load configuration from JSON file
  --dump-schema           Dump JSON schema for configuration
  --dump-preset NAME      Dump preset as JSON
  --validate-config FILE  Validate a config file against schema
  --prefix-added STR      Custom prefix for added items (default: +)
  --prefix-removed STR    Custom prefix for removed items (default: -)
  --prefix-modified STR   Custom prefix for modified items (default: *)
  --prefix-unchanged STR  Custom prefix for unchanged items (default: =)
  --note-name-style       Note naming: Sharp or Flat
  --max-collection-items  Limit collection output size
  --git                   Git diff driver mode
  --version               Show version
  --help                  Show help
```

### Git Diff Driver Mode

When used as a git diff driver, alsdiff receives 7 arguments:

```bash
alsdiff --git path old-file old-hex old-mode new-file new-hex new-mode
```

## Architecture

```
alsdiff/
├── bin/                    # CLI entry point
│   └── alsdiff.ml
├── lib/
│   ├── base/               # Core utilities (alsdiff_base)
│   │   ├── file.ml         # .als file decompression
│   │   ├── xml.ml          # XML parsing
│   │   ├── upath.ml        # μpath query language
│   │   ├── equality.ml     # EQUALABLE/IDENTIFIABLE traits
│   │   └── diff.ml         # Diff algorithm with phantom types
│   ├── live/               # Ableton domain types (alsdiff_live)
│   │   ├── liveset.ml      # Top-level structure
│   │   ├── track.ml        # Track types, routing, mixer
│   │   ├── device.ml       # Devices, plugins, parameters
│   │   ├── clip.ml         # MIDI/audio clips, notes
│   │   └── automation.ml   # Automation envelopes
│   └── output/             # Output formatting (alsdiff_output)
│       ├── view_model.ml   # View types (Field, Item, Collection)
│       ├── config.ml       # Configuration, presets
│       └── text_renderer.ml # Text rendering engine
├── presets/                # Built-in JSON presets
├── test/                   # Test suite
└── docs/                   # Documentation and schemas
```

### Key Concepts

**μpath**: A custom XPath-like query language for navigating Ableton's XML:
```
/LiveSet/Tracks/MidiTrack[0]/DeviceChain
/**@type="AudioEffect"
```

**Phantom Types**: The diff system uses phantom types (`atomic`/`structured`) to distinguish between primitive value changes and complex nested object patches.

**View Model**: A unified type system for rendering output, with `Field`, `Item`, and `Collection` types that can be rendered at different detail levels.

## Building & Testing

```bash
# Build
dune build

# Run all tests
dune runtest

# Run specific test
dune exec test/test_upath.exe
dune exec test/test_device.exe
dune exec test/test_view_model.exe

# Format code
dune build @fmt

# Clean
dune clean
```

## Dependencies

- OCaml 5.3.0+
- camlzip - Gzip decompression
- xmlm - XML parsing
- angstrom - Parser combinators
- cmdliner - CLI parsing
- eio/eio_main - Concurrent file loading
- ppx_deriving_yojson - JSON serialization
- ppx_deriving_jsonschema - JSON schema generation
- alcotest - Testing framework
- fmt - Pretty-printing

</details>

<details>
<summary>README by Gemini 3 Pro</summary>

> A precise, structure-aware diff tool for Ableton Live Set (`.als`) files, written in OCaml.

`alsdiff` allows you to compare two versions of an Ableton Live project and visualize the changes in a human-readable format. Unlike standard text diff tools which fail on the complex XML structure of `.als` files, `alsdiff` understands the musical context—tracks, devices, clips, and automation.

## Features

- **Semantic Comparison**: Understands Ableton Live constructs (Tracks, Devices, Clips, Automation) rather than just XML lines.
- **Device Support**: Deep inspection of:
    - Native Devices (Compressor, EQ8, etc.)
    - Plugins (VST2, VST3, AU)
    - Max for Live Devices
    - Rack Devices (Groups/Chains)
- **Musical Detail**: Diff MIDI notes, velocities, audio warping, and automation envelopes.
- **Configurable Output**:
    - Multiple detail levels (Compact, Full, Verbose, etc.)
    - JSON configuration files (`.alsdiff.json`)
    - Custom change prefixes
- **Git Integration**: Designed to work seamlessly as a git `diff` driver.

## Installation

### Prerequisites
- OCaml 5.3+
- opam (OCaml Package Manager)

### Build from Source

```bash
# Clone the repository
git clone https://github.com/krfantasy/alsdiff.git
cd alsdiff

# Install dependencies
dune build

# Run the executable
dune exec alsdiff -- --help
```

To install globally:
```bash
cp _build/default/bin/alsdiff.exe /usr/local/bin/alsdiff
```

## Usage

### Basic CLI

```bash
alsdiff old.als new.als
```

### Presets

`alsdiff` comes with several built-in presets to tailor the output verbosity:

- `--preset quiet` (Default): Shows only essential changes.
- `--preset compact`: Structural overview, ideal for quick checks.
- `--preset full`: Detailed multiline diff of all changed properties.
- `--preset inline`: Detailed single-line diffs.
- `--preset verbose`: exhaustive output including unchanged elements.
- `--preset composer`: Focuses on musical changes (MIDI notes, clips).
- `--preset mixing`: Focuses on signal flow (Volume, Pan, Sends, Returns).

Example:
```bash
alsdiff project_v1.als project_v2.als --preset composer
```

### Git Integration

To use `alsdiff` effectively with git, configure it as a diff driver.

1.  **Configure Git Global/Local Config**:
    ```ini
    [diff "als"]
        command = alsdiff --preset quiet --git
        trustExitCode = true
    ```

2.  **Set Git Attributes** (in `.gitattributes`):
    ```
    *.als diff=als
    ```

Now `git diff` will automatically use `alsdiff` for Ableton Live files.

### Configuration

You can customize `alsdiff` using a `.alsdiff.json` file. The tool searches for this file in:
1.  The CLI `--config` argument.
2.  The directory of the second file being compared.
3.  The git repository root.
4.  The user's home directory.

**Example `.alsdiff.json`**:
```json
{
  "prefix_added": "[+]",
  "prefix_removed": "[-]",
  "prefix_modified": "[*]",
  "note_name_style": "Sharp",
  "max_collection_items": 100
}
```

To generate a starting config:
```bash
alsdiff --dump-preset full > .alsdiff.json
```

## Architecture

The project is structured into three main libraries found in `lib/`:

### 1. `alsdiff_base` (`lib/base`)
Core infrastructure independent of Ableton Live domain logic.
- **XML Parsing**: Robust handling of `.als` XML structure.
- **μpath**: A custom XPath-like query language for traversing the project tree.
- **Diff Logic**: Generic diffing algorithms for atomic and structured types.

### 2. `alsdiff_live` (`lib/live`)
Domain-specific types and logic for Ableton Live.
- **Liveset**: Top-level structure parsing.
- **Tracks**: MIDI, Audio, Group, Return, and Master tracks.
- **Devices**: Hierarchy of Regular, Plugin, Max4Live, and Group devices.
- **Clips**: MIDI and Audio clip analysis, including warping and notes.

### 3. `alsdiff_output` (`lib/output`)
Rendering engine for the diff results.
- **View Model**: A unified representation (`Field`, `Item`, `Collection`) for rendering.
- **Text Renderer**: Generates the final CLI output based on detail configurations.

## Development

### Commands

- **Build**: `dune build`
- **Test**: `dune runtest`
- **Format**: `dune build @fmt` (Uses `ocamlformat`)
- **Run Specific Test**: `dune exec test/test_device.exe`

### Code Style
- **Functional First**: Immutable data structures are preferred.
- **Type Safety**: Extensive use of phantom types and PPX derivings (`[@@deriving eq, yojson]`) to ensure correctness.
- **Module Structure**: Library code uses explicit opens at the top; Test code uses scoped opens.

### Project Structure
```
├── bin/          # CLI entry point
├── lib/          # Core libraries
│   ├── base/     # Generic utilities
│   ├── live/     # Ableton domain logic
│   └── output/   # Rendering logic
├── test/         # Unit and integration tests
├── plugins/      # Experimental plugins/tools
└── presets/      # JSON configuration presets
```

</details>

<details>
<summary>README by GLM 4.7</summary>
A diff tool for Ableton Live Set (.als) files. Compare two versions of your Ableton project and see exactly what changed.

## Features

- **Track-level comparison** - See added, removed, or modified tracks (MIDI, Audio, Group, Return, and Master tracks)
- **Device detection** - Compare Regular devices, Plugin devices (VST2/VST3/AUv2), Max for Live devices, and Group devices with branching
- **MIDI clip support** - Compare note data, velocities, timing, and clip properties
- **Audio clip support** - See changes to audio samples, warp markers, and clip settings
- **Automation tracking** - Compare envelope changes across all automatable parameters
- **Multiple output formats** - 7 presets ranging from quiet summaries to full detailed diffs
- **Git integration** - Use as a git external diff driver with `trustExitCode` support
- **JSON configuration** - Customize output with fine-grained control over detail levels
- **Customizable prefixes** - Set custom indicators for added, removed, modified, and unchanged items
- **Note naming styles** - Choose between Sharp (C#) or Flat (Db) notation for MIDI notes

## Installation

### From Source

Requires OCaml 5.3+ and opam:

```bash
git clone https://github.com/krfantasy/alsdiff.git
cd alsdiff
dune build
```

Install to your PATH:

```bash
# Copy the binary to a directory in your PATH
cp _build/default/bin/alsdiff.exe ~/bin/alsdiff
```

## Usage

### Basic Usage

Compare two .als files:

```bash
alsdiff version1.als version2.als
```

### Output Presets

Choose how much detail you want:

```bash
alsdiff v1.als v2.als --preset quiet      # Minimal output (default)
alsdiff v1.als v2.als --preset compact    # Show structure only
alsdiff v1.als v2.als --preset full       # All details, multi-line
alsdiff v1.als v2.als --preset inline     # All details, single line
alsdiff v1.als v2.als --preset verbose    # Everything including unchanged
alsdiff v1.als v2.als --preset composer   # MIDI composition focused
alsdiff v1.als v2.als --preset mixing     # Stem track mixing focused
```

### Git Integration

Configure alsdiff as your git diff driver for .als files:

**~/.gitconfig:**
```ini
[diff "als"]
    command = alsdiff --preset quiet --git
    trustExitCode = true
```

**.gitattributes:**
```
*.als diff=als
```

Now `git diff` will show human-readable changes between Ableton project versions.

### Configuration

Create `.alsdiff.json` in your project root or home directory:

```json
{
  "prefix_added": "+",
  "prefix_removed": "-",
  "prefix_modified": "*",
  "prefix_unchanged": "",
  "note_name_style": "Sharp",
  "max_collection_items": 50
}
```

Or dump a preset as a starting point:

```bash
alsdiff --dump-preset full > .alsdiff.json
```

Configuration discovery order:
1. `--config FILE` (CLI flag)
2. `.alsdiff.json` in the same directory as FILE2.als
3. `.alsdiff.json` in git repository root
4. `.alsdiff.json` in home directory
5. Default `quiet` preset

## CLI Options

```
--config FILE              Load configuration from JSON file
--preset PRESET            Use output preset (quiet, compact, full, inline, verbose, composer, mixing)
--prefix-added PREFIX      Custom prefix for added items
--prefix-removed PREFIX    Custom prefix for removed items
--prefix-modified PREFIX   Custom prefix for modified items
--prefix-unchanged PREFIX  Custom prefix for unchanged items
--note-name-style STYLE    Note naming: Sharp (C#) or Flat (Db)
--max-collection-items N   Limit collection output size
--dump-preset PRESET       Output preset as JSON
--dump-schema              Output JSON schema for configuration
--validate-config FILE     Validate a configuration file
--git                      Enable git diff driver mode
```

### Example Commands

```bash
# Compare with compact output
alsdiff v1.als v2.als --preset compact

# Customize prefixes
alsdiff v1.als v2.als --prefix-added "[+] " --prefix-removed "[-] " --prefix-modified "[*] "

# Use flat note names for MIDI
alsdiff v1.als v2.als --note-name-style Flat

# Limit collection items to 100
alsdiff v1.als v2.als --max-collection-items 100

# Combine multiple options
alsdiff v1.als v2.als --preset compact --prefix-added "ADD " --max-collection-items 50

# Load configuration from JSON file
alsdiff v1.als v2.als --config myconfig.json

# Use config file with CLI override
alsdiff v1.als v2.als --config myconfig.json --max-collection-items 100

# Dump configuration JSON schema
alsdiff --dump-schema

# Validate a configuration file
alsdiff --validate-config myconfig.json

# Dump preset configuration to file
alsdiff --dump-preset compact > mypreset.json
```

## Development

```bash
# Build the project
dune build

# Run all tests
dune runtest

# Force rerun all tests
dune runtest --force

# Format code
dune build @fmt

# Run specific test
dune exec test/test_upath.exe
dune exec test/test_xml.exe
dune exec test/test_device.exe
dune exec test/test_view_model.exe
dune exec test/test_text_renderer.exe

# Clean build artifacts
dune clean

# Load library into Utop REPL
dune utop
```

## Architecture

ALSDiff is organized into three main libraries:

### lib/base (alsdiff_base)
- **file.ml** - ALS file decompression and loading
- **xml.ml** - XML parsing and manipulation
- **upath.ml** - μpath query language (XPath-like syntax for XML)
- **equality.ml** - Equality and identity traits (EQUALABLE, IDENTIFIABLE)
- **diff.ml** - Diff infrastructure with phantom types (atomic/structured changes)

### lib/live (alsdiff_live)
Ableton Live type definitions:
- **automation.ml** - Automation envelopes and events
- **clip.ml** - MIDI and Audio clips (MidiClip, AudioClip, Loop, MidiNote, SampleRef)
- **device.ml** - All device types (Regular, Plugin, Max4Live, Group)
- **track.ml** - Track types (MidiTrack, AudioTrack, MainTrack, Routing, Mixer, Send)
- **liveset.ml** - Top-level LiveSet structure (Version, Locator, pointees)

### lib/output (alsdiff_output)
- **output.ml** - Output interface definition
- **view_model.ml** - View model types and builders (Field, Item, Collection, ViewBuilder)
- **text_renderer.ml** - Text rendering with detail configs and JSON schema support

## Device Types Supported

- **RegularDevice** - Built-in Ableton effects (Compressor, EQ8, etc.)
- **PluginDevice** - External plugins (VST2, VST3, AUv2)
- **GroupDevice** - Nested device chains with branches
- **Max4LiveDevice** - Max for Live devices
- **MixerDevice** - Track mixer settings
- Supporting types: PluginDesc, PluginParam, GenericParam, MIDIMapping, PresetRef, PatchRef, Macro, Snapshot, Branch

## Track Types Supported

- **MidiTrack** / **AudioTrack** / **GroupTrack** / **ReturnTrack** - Standard tracks
- **MainMixer** / **MainTrack** - Master output (singleton pattern)
- **Routing** / **RoutingSet** - I/O routing
- **Send** - Track-to-track sends
- **Mixer** - Volume/pan/mute/solo

</details>

<details>
<summary>README by Kimi K2.5</summary>

A diff tool for Ableton Live Set (.als) files. Compare two versions of your Ableton project and see exactly what changed.

## Features

- **Track-level comparison** - See added, removed, or modified tracks
- **Device detection** - Compare plugins, Max for Live devices, and native Ableton devices
- **MIDI clip support** - Compare note data, velocities, and timing
- **Audio clip support** - See changes to audio samples and warping
- **Automation tracking** - Compare envelope changes
- **Multiple output formats** - From quiet summaries to full detailed diffs
- **Git integration** - Use as a git external diff driver

## Installation

### From Source

Requires OCaml 5.3+ and opam:

```bash
git clone https://github.com/krfantasy/alsdiff.git
cd alsdiff
dune build
```

Install to your PATH:

```bash
# Copy the binary to a directory in your PATH
cp _build/default/bin/alsdiff.exe ~/bin/alsdiff
```

## Usage

### Basic Usage

Compare two .als files:

```bash
alsdiff version1.als version2.als
```

### Output Presets

Choose how much detail you want:

```bash
alsdiff v1.als v2.als --preset quiet      # Minimal output (default)
alsdiff v1.als v2.als --preset compact    # Show structure only
alsdiff v1.als v2.als --preset full       # All details, multi-line
alsdiff v1.als v2.als --preset inline     # All details, single line
alsdiff v1.als v2.als --preset verbose    # Everything including unchanged
alsdiff v1.als v2.als --preset composer   # MIDI composition focused
alsdiff v1.als v2.als --preset mixing     # Stem track mixing focused
```

### Git Integration

Configure alsdiff as your git diff driver for .als files:

**~/.gitconfig:**
```ini
[diff "als"]
    command = alsdiff --preset quiet --git
    trustExitCode = true
```

**.gitattributes:**
```
*.als diff=als
```

Now `git diff` will show human-readable changes between Ableton project versions.

### Configuration

Create `.alsdiff.json` in your project root or home directory:

```json
{
  "prefix_added": "+",
  "prefix_removed": "-",
  "prefix_modified": "*",
  "note_name_style": "Sharp",
  "max_collection_items": 50
}
```

Or dump a preset as a starting point:

```bash
alsdiff --dump-preset full > .alsdiff.json
```

Configuration discovery order:
1. `--config FILE` (CLI flag)
2. `.alsdiff.json` in the same directory as FILE2.als
3. `.alsdiff.json` in git repository root
4. `.alsdiff.json` in home directory
5. Default `quiet` preset

## CLI Options

```
--config FILE              Load configuration from JSON file
--preset PRESET            Use output preset (quiet, compact, full, inline, verbose, composer, mixing)
--prefix-added PREFIX      Custom prefix for added items
--prefix-removed PREFIX    Custom prefix for removed items
--prefix-modified PREFIX   Custom prefix for modified items
--note-name-style STYLE    Note naming: Sharp (C#) or Flat (Db)
--max-collection-items N   Limit collection output size
--dump-preset PRESET       Output preset as JSON
--dump-schema              Output JSON schema for configuration
--validate-config FILE     Validate a configuration file
--git                      Enable git diff driver mode
```

## Development

```bash
# Build
dune build

# Run tests
dune runtest

# Format code
dune build @fmt

# Run specific test
dune exec test/test_device.exe
```

</details>

## License

It's public domain, see [THE-LICENSE.txt](THE-LICENSE.txt) for details.

## Links

- [Live Object Model](https://docs.cycling74.com/apiref/lom/)
- [Unofficial Live API documentation](https://structure-void.com/ableton-live-midi-remote-scripts/)
- [Ableton's maxdiff tool for Max patches and Max for Live devices](https://github.com/Ableton/maxdevtools/tree/main/maxdiff)
