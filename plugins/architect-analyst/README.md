# Architect Analyst

Deep architectural analysis and solution design agent for complex problems.

## Overview

This agent provides comprehensive architectural analysis by evaluating multiple solution approaches, analyzing trade-offs, and providing well-researched recommendations for complex software engineering problems. It excels at problems requiring deep consideration of design patterns, performance implications, and long-term maintainability.

## Usage

Use the agent when you need deep architectural analysis:

```
I need to add support for processing nested structures with recursive chains. How should I approach this?
```

```
This module is getting large. Should I split it into smaller modules or use a different architectural pattern?
```

```
The data processing is slow on large projects. How can I optimize this?
```

```
I want to redesign the output API to support both text and JSON rendering. What's the best approach?
```

## Features

- **Multi-solution analysis**: Generates 3-4 distinct approaches with detailed evaluation
- **Comprehensive trade-off analysis**: Evaluates correctness, robustness, elegance, maintainability, performance, integration, and testing
- **Research capabilities**: Uses advanced tools for documentation research and best practices
- **Sequential thinking**: Step-by-step reasoning for complex architectural problems
- **Deep research**: Comprehensive investigation across multiple sources when needed

## Agent Capabilities

The architect-analyst agent uses:

- **Sequential thinking** for complex reasoning through multi-step problems
- **Exa deep research** for comprehensive investigation of complex topics
- **Context7** for querying official library and framework documentation
- **DeepWiki** for understanding open-source project internals
- **Exa web search** for articles, blog posts, and discussions
- **Standard code analysis tools** (Read, Glob, Grep, etc.)

## When to Use

This agent is ideal for:

1. **New Feature Implementation**: Adding significant functionality requiring architectural decisions
2. **Multiple Valid Approaches**: Problems that can be solved in several different ways
3. **Code Modifications**: Changes affecting existing behavior or structure
4. **Architectural Decisions**: Choosing between patterns or technologies
5. **Multi-File Changes**: Tasks affecting more than 2-3 files
6. **Unclear Requirements**: Need to explore before understanding full scope
7. **Performance Issues**: Problems requiring evaluation of optimization strategies

## Analysis Framework

The agent follows a systematic approach:

1. **Problem Analysis**: Deep dive into understanding the issue
2. **Context Review**: Examination of relevant codebase patterns using Explore agent
3. **Solution Options**: 3-4 distinct approaches with full analysis
4. **Recommendation**: Clear choice with justification and implementation notes

## Installation

This plugin is automatically installed when trusting the alsdiff repository.

## Example Scenarios

### API Design
```
I want to redesign the output API to support both text and JSON rendering. What's the best approach?
```

### Performance Optimization
```
The data processing is slow on large projects with thousands of items. How can I optimize this?
```

### Modularization
```
This module is getting large. Should I split it into smaller modules or use a different architectural pattern?
```

### Complex Feature Implementation
```
I need to add support for processing nested structures with recursive chains. How should I approach this?
```

## Output Format

The agent provides structured responses:

- **Problem Analysis**: Understanding of requirements and constraints
- **Context Review**: Relevant codebase patterns and conventions
- **Solution Options**: Multiple approaches with:
  - Description
  - Analysis (correctness, robustness, elegance, maintainability, performance, integration, testing)
  - Pros and cons
- **Recommendation**: Clear choice with reasoning and implementation notes

## Best Practices

- **Be specific**: Provide context about the problem and constraints
- **Mention scope**: Indicate if this affects multiple components
- **State goals**: What are you trying to achieve?
- **Provide background**: Why are you considering this change?

## Integration

Works seamlessly with any codebase and complements other development tools by providing deep architectural thinking for complex decisions.
