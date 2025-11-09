# XML Element Extractor

Extract specific XML elements from source files using Python's standard library with optional xmllint formatting.

## Overview

This skill enables precise extraction of XML elements from source files while maintaining their complete structure. It's particularly useful for working with Ableton Live Set (.als) files.

## Usage

Use the skill with natural language commands:

```
I need to extract the <MxDeviceAudioEffect Id="5"> element from t.xml to test/device.xml
```

```
Extract the <InstrumentVector Id="0"> element from source.xml to output.xml
```

## Features

- **Robust XML parsing**: Uses Python's standard library for reliable parsing
- **Attribute order insensitive**: Works regardless of attribute order in XML tags
- **Cross-platform compatibility**: Works on Windows, macOS, and Linux
- **Optional formatting**: Uses xmllint when available for clean output
- **Error handling**: Provides clear error messages for common issues

## Installation

This plugin is automatically installed when trusting the alsdiff repository.

## Technical Details

- **Language**: Python 3
- **Dependencies**: xml.etree.ElementTree (standard library)
- **Optional**: xmllint for formatting
- **Platform**: Cross-platform

## Parameters

The skill takes three parameters:
1. **Source file**: Path to input XML file
2. **Destination file**: Path for extracted element output
3. **Element tag**: Exact opening tag including attributes

## Examples

```bash
# Basic extraction
Extract <Device Id="42"> from live_set.xml to device.xml

# Complex extraction with quotes
Extract <MxDeviceAudioEffect Id="5"> from t.xml to test/m4l_device.xml
```

## Error Handling

The skill handles common issues:
- Missing source files
- Malformed XML
- Elements not found
- File permission errors