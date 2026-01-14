-- AppleScript to save the current Ableton Live project with path support
-- Navigates to directory, enters filename, waits for dialog close, and returns saved file path

on run argv
	tell application "System Events"
		-- Check if Ableton Live is running
		if not (exists process "Live") then
			return "Error: Ableton Live is not running"
		end if

		-- Activate Ableton Live and bring it to front
		set frontmost of process "Live" to true

		-- Wait for application to be ready
		delay 0.5

		-- Determine action based on arguments
		if (count of argv) > 0 then
			set actionString to item 1 of argv as string

			if actionString is "save_as" then
				-- Open Save As dialog (Cmd+Shift+S)
				keystroke "S" using {command down, shift down}
				delay 1.0
				return "Save As dialog opened. Please complete the save manually."

			else if actionString starts with "DIR:" then
				-- Extract directory path (format: DIR:/path/to/dir|filename)
				set savePath to text 5 thru -1 of actionString -- Remove "DIR:" prefix

				-- Split by | to get directory and filename
				set oldDelims to AppleScript's text item delimiters
				set AppleScript's text item delimiters to "|"
				set pathParts to every text item of savePath
				set AppleScript's text item delimiters to oldDelims

				-- First part is the directory, second is filename
				set dirPath to item 1 of pathParts
				if (count of pathParts) > 1 then
					set fileName to item 2 of pathParts
				else
					set fileName to ""
				end if

				-- Open Save As dialog first
				keystroke "S" using {command down, shift down}
				delay 1.0

				-- Navigate to the directory using Cmd+Shift+G (Go to Folder)
				if dirPath is not "" then
					-- Open Go to Folder sheet
					keystroke "G" using {command down, shift down}
					delay 0.6

					-- Type the directory path
					keystroke dirPath
					delay 0.3

					-- Press Enter to go to folder
					keystroke return
					delay 0.5
				end if

				-- Enter the filename if provided
				if fileName is not "" then
					-- Select all text in filename field (Cmd+A)
					keystroke "a" using command down
					delay 0.2

					-- Type the new filename (without .als extension if present)
					if fileName ends with ".als" then
						set baseName to text 1 thru -5 of fileName
					else
						set baseName to fileName
					end if

					keystroke baseName
					delay 0.2
				end if

				-- Wait for Save As dialog to close (user presses Enter or cancels)
				-- Check for sheet existence (the Save As dialog appears as a sheet in newer macOS)
				set dialogClosed to false
				repeat 300 times -- Wait up to 30 seconds (300 * 0.1s)
					try
						-- Look for the Save sheet/dialog
						set saveSheet to missing value
						tell process "Live"
							try
								-- Try to find sheet by various identifiers
								if (exists sheet 1) then
									set saveSheet to sheet 1
								else if (exists sheet "Save") then
									set saveSheet to sheet "Save"
								else if (exists window "Save") then
									set saveSheet to window "Save"
								end if
							end try
						end tell

						-- If no sheet/window found, dialog has closed
						if saveSheet is missing value then
							set dialogClosed to true
							exit repeat
						end if
					on error
						-- If there's an error accessing the sheet, assume it's closed
						set dialogClosed to true
						exit repeat
					end try
					delay 0.1
				end repeat

				-- Find the most recently created .als file in the target directory
				if dialogClosed and dirPath is not "" then
					-- Use python -c to get absolute path since realpath is not available on macOS
					set recentFile to (do shell script "python3 -c \"import os, glob, time; files = glob.glob('" & dirPath & "' + '/**/*.als', recursive=True); files = [(os.path.getmtime(f), f) for f in files]; print(max(files)[1]) if files else ''\"")

					if recentFile is not "" then
						return "Saved to: " & recentFile
					else
						return "Save completed, but could not locate the .als file in: " & dirPath
					end if
				else if dialogClosed then
					return "Save completed"
				else
					return "Save operation timed out or was cancelled"
				end if

			else if actionString contains ".als" then
				-- Simple path - just open dialog
				keystroke "S" using {command down, shift down}
				delay 1.0
				return "Save As dialog opened for: " & actionString & ". Please complete manually."

			else
				-- Default to regular save for unknown commands
				keystroke "s" using command down
				delay 0.2
				return "Ableton Live project saved"
			end if

		else
			-- No arguments: regular save (Cmd+S)
			keystroke "s" using command down
			delay 0.2
			return "Ableton Live project saved"
		end if
	end tell
end run
