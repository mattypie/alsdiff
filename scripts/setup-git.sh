#!/bin/bash
# One-line setup for alsdiff with git

set -e

echo "🔧 Setting up alsdiff with Git..."
echo ""

# Check if alsdiff is in PATH
if ! command -v alsdiff &> /dev/null; then
    echo "❌ alsdiff not found in PATH"
    echo ""
    echo "Please install alsdiff first. See the Installation section in README.md"
    echo ""
    echo "Quick install for macOS:"
    echo "  1. Download from https://github.com/krfantasy/alsdiff/releases"
    echo "  2. Run: chmod +x alsdiff-macos-*"
    echo "  3. Run: mkdir -p ~/bin && mv alsdiff-macos-* ~/bin/alsdiff"
    echo "  4. Add to PATH: echo 'export PATH=\"\$HOME/bin:\$PATH\"' >> ~/.zshrc"
    echo "  5. Reload: source ~/.zshrc"
    exit 1
fi

# Check if we're in a git repository
if [ ! -d ".git" ]; then
    echo "⚠️  No git repository found in current directory"
    echo ""
    echo "To use this script:"
    echo "  1. Navigate to your music project folder"
    echo "  2. Make sure it has a .git folder (run 'git init' if needed)"
    echo "  3. Run this script again from that folder"
    exit 1
fi

# Setup .gitattributes
if [ -f ".gitattributes" ]; then
    if grep -q "*.als diff=alsdiff" .gitattributes; then
        echo "✅ .gitattributes already configured"
    else
        echo "*.als diff=alsdiff" >> .gitattributes
        echo "✅ Added to .gitattributes"
    fi
else
    echo "*.als diff=alsdiff" > .gitattributes
    echo "✅ Created .gitattributes"
fi

# Ask user for extra command line arguments
echo ""
echo "🔧 Optional: Add extra command line arguments for alsdiff?"
echo ""
echo "Common options:"
echo "  --preset PRESET        Output preset (compact, composer, full, inline, mixing, quiet, verbose)"
echo "  --config FILE          Load configuration from JSON file"
echo "  --prefix-added PREFIX  Custom prefix for added items (default: '+')"
echo "  --prefix-removed PREFIX Custom prefix for removed items (default: '-')"
echo "  --note-name-style STYLE Note name style (Sharp, Flat)"
echo "  --max-collection-items N Max items to show in collections"
echo ""
read -p "Enter extra arguments (or press Enter to skip): " extra_args

# Build the command with arguments
if [ -z "$extra_args" ]; then
    ALSDIFF_CMD="alsdiff --git"
else
    ALSDIFF_CMD="alsdiff $extra_args --git"
fi

# Configure git diff driver
git config --global diff.alsdiff.command "$ALSDIFF_CMD"
echo "✅ Configured git to use: $ALSDIFF_CMD"

echo ""
echo "🎉 Done! Git configured with:"
echo "   $ALSDIFF_CMD"
