#!/usr/bin/env python3
"""
Helper script to save Ableton Live projects via AppleScript.

This script bridges the gap between the Ableton MCP (which can't save)
and the actual save operation by triggering keyboard shortcuts.

Usage:
    python ableton_save.py                    # Regular save (Cmd+S)
    python ableton_save.py save_as           # Opens Save As dialog
    python ableton_save.py /path/to/file.als # Save As with path
"""

import subprocess
import sys
from pathlib import Path


def get_script_path() -> Path:
    """Get the path to the AppleScript file."""
    # Both scripts are in the same directory
    script_dir = Path(__file__).parent
    return script_dir / "save_ableton_project.scpt"


def save_ableton_project(path: str | None = None) -> str:
    """
    Save the current Ableton Live project.

    Args:
        path: Optional path for "Save As". If None, does regular save.
              If "save_as", opens Save As dialog without path.
              Can be relative or absolute path.

    Returns:
        Result message from the AppleScript.
    """
    script_path = get_script_path()

    if not script_path.exists():
        return f"Error: AppleScript not found at {script_path}"

    # Build command
    cmd = ["osascript", str(script_path)]

    if path and path != "save_as":
        # Convert relative path to absolute path
        path_obj = Path(path).expanduser().resolve()

        # Split into directory and filename for AppleScript
        dir_path = str(path_obj.parent)
        file_name = path_obj.name

        # Pass as DIR:/path/to/dir|filename format
        cmd.append(f"DIR:{dir_path}|{file_name}")
    elif path:
        cmd.append(path)

    # Execute AppleScript
    try:
        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            check=True,
        )
        return result.stdout.strip()
    except subprocess.CalledProcessError as e:
        return f"Error running AppleScript: {e.stderr}"
    except FileNotFoundError:
        return "Error: osascript not found (this script only works on macOS)"


def main() -> int:
    """Main entry point for CLI usage."""
    if len(sys.argv) > 1:
        path = sys.argv[1]
    else:
        path = None

    result = save_ableton_project(path)
    print(result)

    # Return non-zero exit code on error
    return 0 if not result.startswith("Error") else 1


if __name__ == "__main__":
    sys.exit(main())
