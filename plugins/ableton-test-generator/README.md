# Ableton Test Generator

Generate Ableton Live project files (.als) for testing by using natural language commands and Ableton MCP tools.

## Overview

This skill enables you to create test Ableton Live Set files (.als) with specific configurations through natural language commands. It parses your requests, applies changes via Ableton MCP tools, and saves the project automatically.

## Usage

Use the skill with natural language commands:

```
Create a test file with tempo 145 BPM, save to test_als/test.als
```

```
Set time signature to 7/8 and save the live set to test_als/78.als
```

```
Create 2 MIDI tracks with clips, tempo 120, save to /tmp/test_set.als
```

## Features

- **Tempo control**: Set BPM to any value
- **Time signature**: Change numerator and denominator
- **Track creation**: Create MIDI and audio tracks
- **Clip management**: Add clips to tracks
- **Device parameters**: Modify device settings
- **Automation**: Write automation envelopes
- **Automatic saving**: Uses ableton-save skill for project management

## Prerequisites

- **Ableton Live** (running)
- **Ableton MCP server** (configured and connected)
- **ableton-save** skill or plugin (for saving projects)
- **macOS** (for ableton-save automation)

## Installation

This plugin is automatically installed when trusting the alsdiff repository.

To manually install:

```bash
/plugin marketplace add ./
/plugin install ableton-test-generator@alsdiff-tools
```

## How It Works

1. **Parse Request**: Extract file path and desired changes from your prompt
2. **Apply Changes**: Use Ableton MCP tools to modify the Live set
3. **Save Project**: Call ableton-save skill to save the file
4. **Return Result**: Display the saved file path

## Supported Changes

| Change Type | Example |
|------------|---------|
| Tempo | "tempo 145", "145 BPM" |
| Time signature | "7/8 time", "4/4" |
| Tracks | "create 2 MIDI tracks", "add audio track" |
| Clips | "add 4-bar clip", "create clip" |
| Devices | "set filter to 500 Hz" |
| Automation | "automate volume from -inf to 0 dB" |

## Examples

### Simple Tempo Change
```
Create a test file with tempo 145 BPM, save to test_als/test.als
```
**Result**: Creates test_als/test Project/test.als with tempo set to 145 BPM

### Time Signature
```
Set time signature to 7/8 and save to test_als/78.als
```
**Result**: Creates test_als/78 Project/78.als with 7/8 time signature

### Multiple Tracks
```
Create 2 MIDI tracks with clips, tempo 120, save to /tmp/test_set.als
```
**Result**: Creates /tmp/test_set.als with 2 MIDI tracks, each with a 4-bar clip, tempo 120

## Error Handling

The skill handles:
- **Missing file path**: Prompts you to provide an output location
- **Ableton Live not running**: Informs you that Live must be running
- **MCP connection issues**: Reports connection problems
- **Invalid changes**: Describes what went wrong

## Dependencies

- **ableton-save** skill/plugin (for saving .als files)
- **Ableton MCP server** (for controlling Ableton Live)
- **Ableton Live Remote Script** (loaded in Live's MIDI Remote Scripts directory)

## Tips

- Always specify a file path ending in `.als`
- Be specific about track types (MIDI vs audio)
- Use descriptive clip lengths (e.g., "4-bar clip", "8-bar clip")
- Tempo is specified in BPM (e.g., "120 BPM", "tempo 120")
- Time signatures use "numerator/denominator" format (e.g., "7/8", "4/4")

## See Also

- **ableton-save** plugin: Saves Ableton Live projects via AppleScript
- **Ableton MCP** documentation: Complete reference of 76+ available tools
