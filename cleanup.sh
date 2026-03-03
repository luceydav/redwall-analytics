#!/bin/bash
# Post-render cleanup: remove Hugo files that got copied to _site/
for dir in content static themes layouts resources blogdown; do
  if [ -d "_site/$dir" ]; then
    rm -rf "_site/$dir"
    echo "Removed _site/$dir"
  fi
done
