# alsdiff

![Build status](https://github.com/krfantasy/alsdiff/actions/workflows/build.yml/badge.svg)
[![GitHub Release](https://img.shields.io/github/v/release/krfantasy/alsdiff)](https://github.com/krfantasy/alsdiff/releases/latest)
[![License](https://img.shields.io/badge/license-THE_LICENSE-magenta)](https://github.com/krfantasy/the-license)

> `alsdiff` is a semantic diff tool for Ableton Live Set (.als) files that makes version control meaningful for music producers.

## The Problem

Ableton Live Set files (`.als`) are gzip-compressed XML. Because standard `git diff` treats them as binary blobs, using version control to track changes in music projects is nearly impossible.

While existing workarounds allow decompressing `.als` files to `.xml` for diffing, the raw output is overwhelmingly verbose. A typical 20-track project can result in a 200,000-line XML file, making generic text diffs unreadable.

`alsdiff` solves this problem. It decompresses `.als` files and parses the XML to perform a **semantic diff**:

- **Identity-based matching**: Tracks, clips, and devices are matched by their internal IDs, not by position. Renaming or reordering doesn't create false diffs.
- **Structured hierarchy**: Changes are organized by Live's structure (LiveSet > Tracks > Devices > Parameters).
- **Human-readable output**: Instead of XML tag changes, you see "Freq: 2500.0 -> 3200.0" or "Notes (3 Added, 1 Removed)".
- **Noise filtering**: Internal metadata changes (like timestamps or cache data) are ignored by default.

The output of `alsdiff` looks like this:

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

## Installation

### Install from pre-built binaries

Download from [GitHub Releases](https://github.com/krfantasy/alsdiff/releases):

```bash
chmod +x ./alsdiff
mv alsdiff ~/.local/bin  # or any directory in your PATH
```

> **Note**: The Windows build is currently **untested** as I don't have access to a Windows machine. Feedback welcome!

### Git Integration

A helper script is provided at [./scripts/setup-git.sh](./scripts/setup-git.sh). Simply download it to your git repository and run `./setup-git.sh`. This script will automatically configure git to use `alsdiff` for `.als` files.

Alternatively, you can perform the setup manually.

1. Add to `.gitattributes` in your repository:

```bash
cd /path/to/live_projects/
echo "*.als diff=alsdiff" >> .gitattributes
```

2. Configure git:

```bash
git config --global diff.alsdiff.command "alsdiff <put_extra_args_here> --git"
```

3. Now `git diff` will use `alsdiff` to compare `.als` files:

```bash
git diff HEAD~1 -- your-project.als
```

### Build from source

<details>
<summary>Click to expand</summary>

#### Prerequisites

 - [opam](https://opam.ocaml.org/doc/Install.html): OCaml package manager

#### Setup (first time only)

```bash
# Initialize opam if not done before
opam init

# Create a local switch with the correct OCaml version
opam switch create . --deps-only --with-test

# Activate the environment
eval $(opam env)
```

#### Build

```bash
# Install dependencies (if not using local switch)
opam install . --deps-only

# Build
dune build

# Run tests (optional)
dune runtest

# Install to ~/.local/bin
dune install alsdiff --prefix ~/.local/
```

#### Run without installing

```bash
dune exec alsdiff -- file1.als file2.als
```

</details>

### Custom build via GitHub Action

<details>
<summary>Click to expand</summary>

You can build custom binaries from any branch or commit using the provided GitHub Actions workflow, removing the need for a local OCaml environment.

#### How to use

1. Fork this repository
2. Go to the Action tab in your fork
3. Select "Custom Build" workflow
4. Click "Run workflow"
5. Choose options:
   - **Branch**: Any branch (e.g., `master`, `develop`)
   - **Commit SHA** (optional): Specific commit to build
6. Click "Run workflow"

#### Output

The workflow builds binaries for Ubuntu, Windows, and macOS (Intel & Apple Silicon). It automatically creates a GitHub Release tagged {branch}-{commit-sha} containing the compiled assets.

#### Download

After the workflow completes, find the release at the bottom of the [Releases page](https://github.com/krfantasy/alsdiff/releases) named like:
```
Build master-ca82745
```

Download the appropriate file for your system:
```
alsdiff-ubuntu-x86_64-master-ca82745.zip
alsdiff-macos-arm64-master-ca82745.zip
alsdiff-macos-x86_64-master-ca82745.zip
alsdiff-windows-x86_64-master-ca82745.zip
```

</details>

## Configuration

`alsdiff` uses a flexible configuration system to control output detail levels. You can use built-in presets, custom JSON configuration files, or individual command-line options.

### Quick Start with Presets

The easiest way to get started is using one of the built-in presets:

| Preset | What it feels like | When to use it |
|---|---|---|
| `quiet` | Minimal output | Quick checks, very large sets |
| `compact` | Balanced summary (recommended) | Daily use |
| `inline` | Detailed, but dense | When you want detail without big blocks |
| `full` | Very detailed | Deep review / “what exactly changed?” |
| `mixing` | Emphasizes mixer/devices/automation | Mixing / sound design changes |
| `composer` | Emphasizes clips/notes | MIDI writing / arrangement changes |
| `verbose` | Includes unchanged items (can get huge) | Debugging / auditing |


```bash
alsdiff v1.als v2.als --preset quiet      # Minimal output
alsdiff v1.als v2.als --preset compact    # Balanced overview
alsdiff v1.als v2.als --preset full       # All details for changed items
alsdiff v1.als v2.als --preset inline     # Compact single-line format
alsdiff v1.als v2.als --preset verbose    # Everything including unchanged
alsdiff v1.als v2.als --preset mixing     # Stem mixing focus
alsdiff v1.als v2.als --preset composer   # MIDI composition focus
```

<details>
<summary>Preset output comparison</summary>

**quiet** - Shows only top-level structure:
```
* LiveSet
  * Tracks (1 Modified)
```

**compact** - Shows structure with change counts:
```
* LiveSet
  * Tracks
    * MidiTrack: "Lead Synth"
      * Clips (1 Modified)
      * DeviceChain (1 Added, 1 Modified)
```

**full** - Expands all details:
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
```

</details>

### Command-Line Options

#### Configuration Selection

- `--preset PRESET` - Use built-in preset (compact, full, inline, mixing, composer, quiet, verbose)
- `--config FILE` - Load custom configuration from JSON file

#### Display Customization

- `--prefix-added PREFIX` - Custom prefix for added items (default: "+")
- `--prefix-removed PREFIX` - Custom prefix for removed items (default: "-")
- `--prefix-modified PREFIX` - Custom prefix for modified items (default: "*")
- `--prefix-unchanged PREFIX` - Custom prefix for unchanged items (default: "=")
- `--note-name-style STYLE` - MIDI note display: Sharp or Flat (default: Sharp)
- `--max-collection-items N` - Limit items shown in collections

#### Git Integration

- `--git` - Enable git diff driver mode (expects 7 positional arguments from git)

#### Utility Commands

- `--dump-preset PRESET` - Output preset as JSON to stdout
- `--dump-schema` - Output JSON schema to stdout
- `--validate-config FILE` - Validate config file against schema


### Advanced configurations
<details>
<summary>Click to expand</summary>

### Configuration File Format

Configuration files use JSON format with the following structure:

```json
{
  "$schema": "https://raw.githubusercontent.com/krfantasy/alsdiff/master/docs/config.schema.json",
  "added": ["Summary"],
  "removed": ["Summary"],
  "modified": ["Summary"],
  "unchanged": ["Ignore"],
  "type_overrides": [
    {
      "domain_type": ["DTTrack"],
      "override": {
        "added": ["Inline"],
        "removed": ["Summary"],
        "modified": ["Compact"]
      }
    }
  ],
  "max_collection_items": 20,
  "prefix_added": "+",
  "prefix_removed": "-",
  "prefix_modified": "*",
  "prefix_unchanged": "=",
  "note_name_style": ["Sharp"],
  "indent_width": 2
}
```

#### Configuration Options Reference

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `added` | detail_level | varies | Detail level for added items (one of: Ignore, Summary, Compact, Inline, Full) |
| `removed` | detail_level | varies | Detail level for removed items |
| `modified` | detail_level | varies | Detail level for modified items |
| `unchanged` | detail_level | varies | Detail level for unchanged items |
| `type_overrides` | array | `[]` | Per-type detail level overrides (see below) |
| `max_collection_items` | integer/null | varies | Maximum items to show in collections (0 = none, omitted = unlimited) |
| `prefix_added` | string | `"+"` | Prefix symbol for added items |
| `prefix_removed` | string | `"-"` | Prefix symbol for removed items |
| `prefix_modified` | string | `"*"` | Prefix symbol for modified items |
| `prefix_unchanged` | string | `"="` | Prefix symbol for unchanged items |
| `note_name_style` | note_style | `["Sharp"]` | MIDI note display style: `["Sharp"]` (C#, D#) or `["Flat"]` (Db, Eb) |
| `indent_width` | integer | `2` | Number of spaces per indentation level |

**JSON Format Notes:**
- Detail levels and enums use JSON array format: `["Full"]`, `["Sharp"]`
- `max_collection_items` can be `null` for unlimited, or omitted to use preset default
- Preset defaults vary: `quiet` uses 0, `compact` uses 20, `full` uses 100, `verbose` uses null

### Detail Levels

The configuration system uses five detail levels to control how items are displayed:

| Level | Description |
|-------|-------------|
| `Ignore` | Completely hide the item |
| `Summary` | Show name + change symbol + counts (e.g., "Notes (3 Added, 1 Removed)") |
| `Compact` | Show name + change symbol (same as Summary for elements/collections) |
| `Inline` | Show name + change symbol + all fields on single line |
| `Full` | Show name + change symbol + all fields/sub-views (multiline) |

### Understanding the View Model

`alsdiff` represents Live sets using a hierarchical view model with three types of views:

| Type | Description | Ableton Live Examples |
|------|-------------|-----------------------|
| **Field** | A single value with old/new values | Track volume, pan position, note pitch, loop length |
| **Item** | A named entity that contains child views | A track, a clip, a device, a mixer |
| **Collection** | A list of items (possibly with counts) | Tracks list, Notes list, Devices list, Locators |

### Auto-Discovery

When no `--config` or `--preset` is specified, alsdiff automatically searches for `.alsdiff.json` configuration files in the following order:

1. Directory of the second (new) .als file
2. Git repository root (if in a git repo)
3. User's home directory (`~/.alsdiff.json`)
4. Falls back to `quiet` preset

This allows project-specific or user-specific defaults without manual configuration.

### Type-Based Overrides

You can customize display behavior for specific types of elements using `type_overrides`:

#### Available Domain Types

| Type | Description |
|------|-------------|
| `DTLiveset` | Root level of the Live set |
| `DTTrack` | MIDI/Audio/Group tracks |
| `DTDevice` | Plugin, Group, and Max for Live devices |
| `DTClip` | Audio and MIDI clips |
| `DTAutomation` | Automation envelopes |
| `DTMixer` | Track mixer settings |
| `DTRouting` | Input/output routing |
| `DTLocator` | Scene markers |
| `DTParam` | Device parameters |
| `DTNote` | MIDI notes |
| `DTEvent` | Automation events |
| `DTSend` | Track sends |
| `DTPreset` | Device presets |
| `DTMacro` | Macros |
| `DTSnapshot` | Device snapshots |
| `DTLoop` | Clip loop settings |
| `DTSignature` | Time signatures |
| `DTSampleRef` | Audio sample references |
| `DTVersion` | File version |
| `DTOther` | Any other elements |

#### Override Format

```json
"type_overrides": [
  {
    "domain_type": ["DTTrack"],
    "override": {
      "added": ["Inline"],
      "removed": ["Summary"],
      "modified": ["Compact"],
      "unchanged": ["Ignore"]
    }
  },
  {
    "domain_type": ["DTNote", "DTEvent"],
    "override": {
      "added": ["Summary"],
      "removed": ["Summary"],
      "modified": ["Summary"]
    }
  }
]
```

</details>

## Current Status

`alsdiff` is designed for Ableton Live 12+. Due to changes in the XML schema, it may not function correctly with older versions. There are currently no plans to implement backward compatibility.

<details open>
<summary>Click to expand</summary>

 * Tracks
   + [x] MIDI track & Audio track
   + [x] Group track
   + [x] Return track
   + [x] Main track
   + [ ] Take lanes & Comping
 * Devices
   + [x] Built+in devices
   + [x] Plugin devices (VST/VST3/AU)
   + [x] Max for Live devices
   + [x] Rack devices, Branches, Macros and Snapshots
   + [x] Presets
   + [ ] Sidechain support
 * Clips
   + MIDI Clip
     - [x] MIDI Note (pitch, velocity, time, duration, off velocity)
     - [x] Loop
     - [ ] MPE
     - [ ] Scale
   + Audio Clip
     - [x] External audio file changes
     - [x] Loop
     - [ ] Fade
     - [ ] Warp markers & settings
 * Automation
   + [x] Basic
   + [ ] Curve
 * Global settings
   - [x] Tempo
   - [ ] Time signature
   - [ ] Scale
 * Utils
   - [x] Mixer
   - [x] Send
   - [x] Routing (MIDI in, Audio out, etc.)
 * [ ] Session View
 * [ ] Groove Pool

</details>

## Known Limitations

### Values represented differently than in the Ableton Live GUI

`alsdiff` outputs the raw internal values stored in the `.als` file, which often differ from the visual representation in Ableton Live (e.g., an internal value of `0.75` might appear as `75%` or `-2.5 dB` in the GUI). Since the formatting logic is internal to the Ableton Live application and not stored in the project file, it is not possible to infer exactly how a value is displayed in the GUI solely from the `.als` data.

## License

It's public domain, see [THE-LICENSE.txt](THE-LICENSE.txt) for details.

## Links

- [Live Object Model](https://docs.cycling74.com/apiref/lom/) - Official Max/MSP Live API reference
- [Unofficial Live API documentation](https://structure-void.com/ableton-live-midi-remote-scripts/) - Community API docs
- [Ableton's maxdiff](https://github.com/Ableton/maxdevtools/tree/main/maxdiff) - Similar tool for Max patches
