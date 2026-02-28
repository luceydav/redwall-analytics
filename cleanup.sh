#!/bin/bash
# Post-render cleanup: remove Hugo files that got copied to _site/
# First strip macOS provenance attributes that prevent deletion
find _site -type f -exec xattr -d com.apple.provenance {} \; 2>/dev/null

for dir in content public static themes layouts resources blogdown; do
  if [ -d "_site/$dir" ]; then
    rm -rf "_site/$dir"
    echo "Removed _site/$dir"
  fi
done
