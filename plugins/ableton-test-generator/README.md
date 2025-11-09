# Ableton Test Generator

Generate comprehensive test cases for Ableton Live Set devices and components by extracting expected values from XML files.

## Overview

This agent specializes in creating OCaml test cases for Ableton Live device mapping types by analyzing large XML files and extracting realistic test data.

## Usage

Use the agent with natural language commands:

```
Generate test cases for GroupDevice.t type using test_group_device.xml file
```

```
I need tests for the DeviceMapping type using real XML data from test_devices/
```

## Features

- **Intelligent extraction**: Automatically identifies relevant test values from XML
- **Type-aware**: Generates tests matching OCaml type definitions
- **Real data**: Uses actual Ableton Live XML files for authentic test cases
- **Comprehensive coverage**: Creates tests for various scenarios and edge cases

## Installation

This plugin is automatically installed when trusting the alsdiff repository.

## Supported Types

- `GroupDevice.t` - Group device mappings with macros and snapshots
- `DeviceMapping` - General device mapping types
- `MidiClip` - MIDI clip data structures
- `AudioClip` - Audio clip configurations
- And more Ableton Live-specific types

## Test Generation Process

1. **Analyze XML**: Parse the provided XML file structure
2. **Identify patterns**: Find elements matching the target type
3. **Extract values**: Pull realistic data for test cases
4. **Generate code**: Create proper OCaml test syntax
5. **Validate**: Ensure generated tests compile and pass

## Example Output

```ocaml
(* Generated test for GroupDevice *)
let test_group_device_create () =
  let xml_content = "<GroupDevice Id=\"42\">..." in
  let expected = { id = 42; name = "Test Device"; ... } in
  let result = GroupDevice.create xml_content in
  Alcotest.(check (option GroupDevice.pp) "GroupDevice creation" expected result)
```

## Usage Tips

- Provide representative XML files for best results
- Specify the target type clearly
- Use multiple XML files for comprehensive coverage
- Review generated tests before committing

## Error Handling

The agent handles:
- Malformed XML files
- Missing required elements
- Type mismatches
- File access issues