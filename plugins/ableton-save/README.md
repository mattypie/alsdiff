# Ableton Save Plugin

Save Ableton Live projects via natural language or slash command using AppleScript automation on macOS.

## Features

- **Natural language triggering**: Say "save the ableton project to /path/to/file.als"
- **Slash command**: Use `/ableton-save:save-live-set /path/to/file.als`
- **Automatic navigation**: Script navigates to the target directory
- **Filename entry**: Automatically enters the filename in the Save As dialog
- **Relative and absolute paths**: Supports both path formats
- **Returns saved path**: After saving, returns the full path of the saved .als file

## Requirements

- macOS (AppleScript support required)
- Ableton Live installed and running
- Terminal app with Accessibility permissions
- Python 3

## Installation

This plugin is part of the alsdiff-tools marketplace. Install via:

```bash
cc --plugin-dir /path/to/alsdiff
```

Or enable in Claude Code settings under the alsdiff-tools marketplace.

## Usage

### Natural Language

```
Save the ableton project to ~/Music/Ableton/Projects/song.als
Save my live set to ../backups/test.als
```

### Slash Command

```bash
/ableton-save:save-live-set /Users/jane/Music/Ableton/Projects/song.als
/ableton-save:save-live-set ~/Music/Projects/song.als
/ableton-save:save-live-set ../backups/song.als
```

## How It Works

1. The script activates Ableton Live
2. Opens the Save As dialog (Cmd+Shift+S)
3. Navigates to your target directory using Cmd+Shift+G
4. Enters the filename automatically
5. You press Enter to complete the save
6. **Script waits for the dialog to close**
7. **Finds and returns the full path of the saved .als file**

**Example output:**
```
Saved to: /Users/jane/Music/Ableton/Projects/song Project/song.als
```

Note: Ableton Live automatically creates a project folder (e.g., "song Project/") and the script correctly detects this.

## Permissions

You must grant Accessibility permissions to Terminal (or your terminal app):

1. Open **System Settings** → **Privacy & Security** → **Accessibility**
2. Find **Terminal** (or iTerm, etc.)
3. Toggle it **ON**
4. Restart your terminal

## Known Limitations

- **macOS only**: Requires AppleScript support
- **Ableton must be running**: Shows error if Ableton Live is not open
- **Manual completion**: You must press Enter in the Save As dialog (script waits up to 30 seconds)
- **Project folders**: Ableton automatically creates a project folder (e.g., "song Project/") - script now correctly detects this and returns the full path

## Troubleshooting

**"Error: osascript is not allowed to send keystrokes"**
- Grant Accessibility permissions to Terminal (see Permissions above)

**"Error: Ableton Live is not running"**
- Open Ableton Live before using this plugin

**Save As dialog doesn't navigate to the folder**
- Check the path is valid
- Try using an absolute path instead of relative

## Development

This plugin was created as part of the alsdiff project for Ableton Live Set development and diffing tools.

## License

MIT License - See alsdiff repository for details.
