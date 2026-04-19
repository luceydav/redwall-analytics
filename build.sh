#!/bin/bash
# Redwall Analytics - local/Netlify build script
# Renders the Quarto site and copies legacy static assets needed by old posts.
set -euo pipefail

temp_dir=""
cleanup() {
  if [ -n "${temp_dir}" ] && [ -d "${temp_dir}" ]; then
    rm -rf "${temp_dir}"
  fi
}
trap cleanup EXIT

ensure_quarto() {
  if command -v quarto >/dev/null 2>&1; then
    return
  fi

  local version platform install_dir archive_url temp_dir
  version="${QUARTO_VERSION:-1.8.27}"

  case "$(uname -s)-$(uname -m)" in
    Linux-x86_64)
      platform="linux-amd64"
      ;;
    Linux-aarch64|Linux-arm64)
      platform="linux-arm64"
      ;;
    *)
      echo "ERROR: quarto not found and automatic install is unsupported on $(uname -s)-$(uname -m)." >&2
      exit 1
      ;;
  esac

  install_dir="${HOME}/.local/quarto/${version}"
  export PATH="${install_dir}/bin:${PATH}"

  if [ -x "${install_dir}/bin/quarto" ]; then
    return
  fi

  archive_url="https://github.com/quarto-dev/quarto-cli/releases/download/v${version}/quarto-${version}-${platform}.tar.gz"
  temp_dir="$(mktemp -d)"

  echo "==> Installing Quarto ${version} for ${platform}..."
  curl -fsSL "${archive_url}" -o "${temp_dir}/quarto.tar.gz"
  mkdir -p "${install_dir}"
  tar -xzf "${temp_dir}/quarto.tar.gz" -C "${install_dir}" --strip-components=1
}

ensure_quarto

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
