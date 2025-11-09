# Zen Code Reviewer

Comprehensive code review agent with thorough analysis and quality assurance.

## Overview

This agent provides systematic, step-by-step code review with expert validation, focusing on quality, security, performance, and architecture. Use it for comprehensive analysis of code changes.

## Usage

Use the agent when you need thorough code review:

```
I just finished refactoring the automation diffing algorithm. Can you review my changes?
```

```
Please review my changes since the last commit with emphasis on logic verification and correctness checking
```

## Features

- **Multi-dimensional analysis**: Covers quality, security, performance, and architecture
- **Step-by-step approach**: Systematic investigation with structured findings
- **Expert validation**: Optional expert analysis for comprehensive review
- **Context-aware**: Understands project structure and requirements
- **Actionable feedback**: Provides specific recommendations and improvements

## Installation

This plugin is automatically installed when trusting the alsdiff repository.

## Review Process

### Phase 1: Analysis Planning
- Defines review strategy based on code complexity
- Identifies focus areas and potential risks
- Establishes review scope and priorities

### Phase 2: Investigation
- Examines code changes systematically
- Analyzes impact on related components
- Identifies potential issues and improvements

### Phase 3: Validation
- Cross-checks findings with best practices
- Validates against project standards
- Ensures consistency with architecture

### Phase 4: Reporting
- Documents all findings clearly
- Provides actionable recommendations
- Highlights critical issues and improvements

## Review Dimensions

### Code Quality
- Readability and maintainability
- Adherence to coding standards
- Proper documentation
- Error handling

### Security
- Input validation
- Data sanitization
- Access control
- Secure coding practices

### Performance
- Algorithm efficiency
- Resource usage
- Scalability considerations
- Optimization opportunities

### Architecture
- Design patterns
- Separation of concerns
- Modularity
- Integration points

## Usage Examples

```bash
# Comprehensive review
Review the latest changes to the device diffing module

# Focused review
Review my authentication changes focusing on security

# Quick validation
Do a quick code review of my recent commits
```

## Review Outputs

The agent provides:
- **Summary**: Overview of changes and overall assessment
- **Issues**: Categorized list of found problems with severity
- **Recommendations**: Specific improvement suggestions
- **Positive findings**: Well-implemented solutions and good practices

## Best Practices

- **Be specific**: Provide context about what you changed
- **State goals**: Mention what you want to focus on
- **Provide background**: Explain why changes were made
- **Follow up**: Address identified issues appropriately

## Integration

Works seamlessly with:
- Git workflows (commits, PRs, branches)
- Project structure (OCaml modules, tests)
- Development standards (coding conventions, documentation)