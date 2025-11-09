# Security Data Leak Scanner

Scan codebases for potential personal data leaks and sensitive information exposure.

## Overview

This agent specializes in identifying potential data leaks in codebases, particularly when working with real project data that might contain sensitive information.

## Usage

Use the agent when you need to check for data leaks:

```
I just added some test XML files from my actual Ableton projects and want to make sure I haven't accidentally committed any personal data
```

```
Before I push this to GitHub, can you check if there are any references to my username or personal data in the files?
```

## Features

- **Pattern matching**: Identifies common patterns of personal data
- **File path analysis**: Checks for personal information in file paths
- **Username detection**: Finds usernames and personal identifiers
- **Content scanning**: Analyzes file contents for sensitive information
- **Project safety**: Ensures code repository is safe for sharing

## Installation

This plugin is automatically installed when trusting the alsdiff repository.

## Detection Capabilities

### Personal Identifiers
- Usernames and account names
- Real names and email addresses
- File paths with personal information
- System-specific paths

### Sensitive Data Patterns
- API keys and tokens
- Passwords and credentials
- Database connection strings
- Private keys and certificates

### Project-Specific Risks
- Ableton project file names
- User-specific device configurations
- Personal plugin settings
- Custom sample paths

## Usage Examples

```bash
# Scan entire codebase
Scan for potential personal data leaks in my codebase

# Check specific files
Check test files for any personal information before sharing

# Pre-commit validation
Verify no sensitive data exists before committing changes
```

## Best Practices

1. **Scan before committing**: Always check changes before git commits
2. **Review test files**: Pay special attention to test data files
3. **Check XML files**: Ableton XML files can contain user-specific paths
4. **Validate configs**: Configuration files may contain sensitive data

## Recommendations

- Use generic test data instead of real project files
- Sanitize file paths before committing
- Use environment variables for sensitive configuration
- Implement pre-commit hooks for automatic scanning

## False Positives

The scanner may flag:
- Example data that happens to match patterns
- Generic placeholder text
- Public API endpoints
- Documentation examples

Review flagged items carefully to determine if they pose actual security risks.