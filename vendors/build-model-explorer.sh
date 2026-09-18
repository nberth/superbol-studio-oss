#!/bin/bash
# Rebuilds the "ai-edge-model-explorer-visualizer" npm package from the
# `vendors/model-explorer` submodule, and copies its output into
# `assets/vendor/model-explorer/` -- the pre-built bundle SuperBOL Studio
# actually ships (see `vendors/README.md`).
#
# Usage (from anywhere): vendors/update-model-explorer.sh
# Requires: network access (to fetch/update the submodule), Node.js and npm.

set -e

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
SUBMODULE_DIR="$ROOT_DIR/vendors/model-explorer"
UI_DIR="$SUBMODULE_DIR/src/ui"
DEST_DIR="$ROOT_DIR/assets/vendor/model-explorer"

# git -C "$ROOT_DIR" submodule update --init "$SUBMODULE_DIR"

# # Only the Angular workspace that builds the custom element is needed: skip
# # the rest of the upstream monorepo (Python package, adapters, demos, CI).
# git -C "$SUBMODULE_DIR" sparse-checkout init --cone
# git -C "$SUBMODULE_DIR" sparse-checkout set src/ui

( cd "$UI_DIR" && npm ci && npm run build-npm )

BUILT_DIR="$UI_DIR/custom_element_npm/dist"

rm -rf "$DEST_DIR"
mkdir -p "$DEST_DIR"
cp "$BUILT_DIR/main_browser.js" "$DEST_DIR/"
cp "$BUILT_DIR/worker.js" "$DEST_DIR/"
cp -r "$BUILT_DIR/static_files" "$DEST_DIR/"
LICENSE_FILE="$ROOT_DIR/assets/vendor/model-explorer.LICENSE"
REF="$(git -C "$SUBMODULE_DIR" describe --tags --always)"
cp "$SUBMODULE_DIR/LICENSE" "$LICENSE_FILE"
sed -i "$LICENSE_FILE" \
    -e 's/\[yyyy\]/2024/' \
    -e 's/\[name of copyright owner\]/The Model Explorer Authors/'
cat >> "$LICENSE_FILE" <<EOF

---

This directory vendors the "ai-edge-model-explorer-visualizer" npm package
(main_browser.js, worker.js, static_files/), built from
https://github.com/google-ai-edge/model-explorer, $REF. The license above is
that repository's own top-level LICENSE, which aggregates the Apache-2.0
license for Model Explorer itself along with the (MIT/BSD) licenses of the
third-party code (Angular, d3, dagre, three.js) statically bundled into
main_browser.js.
EOF

echo "Updated $DEST_DIR from vendors/model-explorer@$REF"
