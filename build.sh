#!/bin/bash
# Redwall Analytics - local build script
# Replaces 'quarto render' directly to handle Hugo legacy static assets
# Usage: ./build.sh
set -e

echo "==> Rendering Quarto site..."
quarto render

echo "==> Copying Hugo static assets into _site/..."
# Old post HTML references /post/SLUG_files/ and /rmarkdown-libs/ at absolute paths.
# Copy them to the root of _site/ so those references resolve correctly.
cp -r static/post _site/post
cp -r static/rmarkdown-libs _site/rmarkdown-libs

# Copy images and img dirs too (some posts reference /images/ or /img/)
[ -d static/images ] && cp -r static/images _site/images || true
[ -d static/img ] && cp -r static/img _site/img || true

echo "==> Build complete!"
echo "    Run: quarto preview"
