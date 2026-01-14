# ALSDiff Claude Code Plugins

This directory contains custom Claude Code plugins for the ALSDiff project, providing project-specific tools for Ableton Live Set development and diffing.

## Available Plugins

### 🛠️ xml-element-extractor
- **Purpose**: Extract XML elements from source files using Python
- **Category**: Utilities
- **Use Case**: Extracting specific elements from Ableton Live Set XML files
- **Skill**: `xml-element-extractor`

### 🧪 ableton-test-generator
- **Purpose**: Generate test cases for Ableton Live Set devices and components
- **Category**: Development
- **Use Case**: Creating OCaml tests from real XML data
- **Agent**: `ableton-test-generator`

### 🔒 security-data-leak-scanner
- **Purpose**: Scan codebases for potential personal data leaks
- **Category**: Security
- **Use Case**: Ensuring no sensitive data is committed to repository
- **Agent**: `security-data-leak-scanner`

### 🔍 zen-code-reviewer
- **Purpose**: Comprehensive code review with quality assurance
- **Category**: Development
- **Use Case**: Thorough analysis of code changes and improvements
- **Agent**: `zen-code-reviewer`

### 🔧 ocaml-lsp
- **Purpose**: LSP integration for OCaml via ocamllsp
- **Category**: Development
- **Use Case**: Real-time code intelligence for OCaml files
- **LSP Server**: `ocamllsp`

### ✨ ocaml-code-indent
- **Purpose**: Automatically format OCaml files with ocp-indent after edits
- **Category**: Development
- **Use Case**: Maintain consistent OCaml code indentation automatically
- **Hook**: PostToolUse (Edit/Write operations)

### 💾 ableton-save
- **Purpose**: Save Ableton Live projects via AppleScript automation
- **Category**: Utilities
- **Use Case**: Automated saving of Ableton Live projects with natural language or slash command
- **Command**: `/ableton-save:save-live-set`
- **Requirements**: macOS, Ableton Live, Terminal Accessibility permissions

## Installation

These plugins are automatically installed when team members trust the ALSDiff repository folder in Claude Code.

### Manual Installation

If automatic installation doesn't work:

1. Start Claude Code in the project directory
2. Add the local marketplace:
   ```
   /plugin marketplace add ./
   ```
3. Install specific plugins:
   ```
   /plugin install xml-element-extractor@alsdiff-tools
   /plugin install ableton-test-generator@alsdiff-tools
   /plugin install security-data-leak-scanner@alsdiff-tools
   /plugin install zen-code-reviewer@alsdiff-tools
   /plugin install ocaml-lsp@alsdiff-tools
   /plugin install ocaml-code-indent@alsdiff-tools
   /plugin install ableton-save@alsdiff-tools
   ```

## Usage Examples

### XML Element Extraction
```
Extract the <MxDeviceAudioEffect Id="5"> element from t.xml to test/device.xml
```

### Test Generation
```
Generate test cases for GroupDevice.t type using test_group_device.xml
```

### Security Scanning
```
Check if there are any personal data references in my recent changes
```

### Code Review
```
Review my recent changes to the device diffing module
```

### OCaml LSP
```
Show me the type information for this function
Go to definition of the Xml.read_file function
```

### OCaml Code Indent
```
Edit the Xml.read_file function to add error handling
(add this code in a new OCaml file)
```
The ocaml-code-indent plugin automatically formats your OCaml code after each edit using ocp-indent.

### Ableton Save
```
Save the ableton project to ~/Music/Ableton/Projects/song.als
/ableton-save:save-live-set ../backups/test.als
```
The ableton-save plugin automates saving Ableton Live projects using AppleScript on macOS.

## Configuration

The plugins are configured through:
- `.claude/settings.json` - Repository-level settings
- `./.claude-plugin/marketplace.json` - Marketplace configuration
- Individual `plugin.json` files for each plugin

## Development

### Adding New Plugins

1. Create plugin directory: `plugins/your-plugin/`
2. Add plugin manifest: `.claude-plugin/plugin.json`
3. Add plugin components (skills, agents, commands)
4. Update marketplace: `./.claude-plugin/marketplace.json`
5. Update settings: `.claude/settings.json`

### Plugin Structure

```
plugins/
├── ../.claude-plugin/
│   └── marketplace.json
├── your-plugin/
│   ├── .claude-plugin/
│   │   └── plugin.json
│   ├── skills/           # Agent skills
│   ├── agents/           # Custom agents
│   ├── commands/         # Slash commands
│   ├── hooks/            # Event hooks (optional)
│   └── README.md         # Plugin documentation
```

## Support

For issues or questions about these plugins:
1. Check individual plugin README files
2. Review the [Claude Code documentation](https://docs.claude.com/en/docs/claude-code/plugins)
3. Create an issue in the project repository

## Version History

- **v1.0.0**: Initial plugin setup with four core tools
  - XML Element Extractor
  - Ableton Test Generator
  - Security Data Leak Scanner
  - Zen Code Reviewer

- **v1.1.0**: Added OCaml LSP integration
  - ocaml-lsp plugin for real-time OCaml code intelligence

- **v1.2.0**: Added OCaml code formatting
  - ocaml-code-indent plugin for automatic OCaml indentation

- **v1.3.0**: Added Ableton Live project automation
  - ableton-save plugin for macOS AppleScript automation
  - Natural language and slash command support for saving projects
  - Automatic directory navigation and filename entry

- **v1.3.1**: Enhanced ableton-save plugin
  - Returns full path of saved .als file after Save As dialog closes
  - Correctly handles Ableton's automatic "Project" folder creation
