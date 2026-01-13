---
description: Save the current Ableton Live project to a specified path
allowed-tools: Bash
argument-hint: <path>
---

# Ableton Live Save Command

This command saves the current Ableton Live project to a specified file path using AppleScript automation on macOS.

## When to Use

Use this command when you need to:
- Save the current Ableton Live project to a specific location
- Automate saving Ableton projects as part of a workflow
- Create backups of your Live sets

## How It Works

The command uses Python and AppleScript to:
1. Activate Ableton Live
2. Open the Save As dialog (Cmd+Shift+S)
3. Navigate to the target directory using Cmd+Shift+G
4. Enter the filename automatically
5. Prompt you to press Enter to complete the save

## Usage

```bash
/ableton-save:save-live-set /path/to/project.als
/ableton-save:save-live-set ~/Music/Ableton/Projects/song.als
/ableton-save:save-live-set ../backups/my-song.als
```

## Arguments

- `path` (required): The file path where you want to save the Ableton Live project
  - Can be absolute or relative path
  - Will automatically create a project folder (Ableton's default behavior)
  - The `.als` extension is optional

## Implementation Steps

1. **Parse the path argument** from the user's request
   - Extract the file path from the command arguments
   - Convert relative paths to absolute paths

2. **Execute the save script**
   - Run: `python3 /Users/krfantasy/.claude/plugins/cache/claude-plugins-official/plugin-dev/f1be96f0fb58/scripts/ableton_save.py <path>`
   - The script will open Ableton Live's Save As dialog and navigate to the target directory

3. **Handle the result**
   - Display the result message from the script
   - If successful, remind the user to press Enter in the Save As dialog
   - If Ableton Live is not running, show an error message

## Error Handling

- **Ableton Live not running**: The script will return "Error: Ableton Live is not running"
- **Invalid path**: The script will still open the Save As dialog but won't navigate to the folder
- **macOS only**: This command only works on macOS with AppleScript support

## Examples

**Save to an absolute path:**
```
User: /ableton-save:save-live-set /Users/jane/Music/Ableton/Projects/song-v2.als
→ Opens Save As dialog, navigates to the Projects folder, enters "song-v2"
```

**Save to a relative path:**
```
User: /ableton-save:save-live-set ../backups/test.als
→ Opens Save As dialog, navigates to ../backups, enters "test"
```

## Notes

- The Save As dialog will remain open after the script completes
- You must press Enter to complete the save operation
- Ableton Live automatically creates a project folder (e.g., `song-v2 Project/`)
- The filename field is automatically populated with the base name (without `.als`)
- This command requires macOS Accessibility permissions for Terminal
