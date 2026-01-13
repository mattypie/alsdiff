-- AppleScript to save the current Ableton Live project with path support
-- Navigates to directory and enters the filename

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

					return "Navigated to: " & dirPath & " and entered filename '" & fileName & "'. Press Enter to save."
				else
					return "Navigated to folder: " & dirPath & ". Please enter filename and save."
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
