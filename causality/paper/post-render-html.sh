#!/usr/bin/env bash
# Copia figuras junto al HTML y reescribe rutas para vista local y GitHub Pages.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
CAUSALITY="$(cd "$SCRIPT_DIR/.." && pwd)"

mkdir -p "$SCRIPT_DIR/figuras"
rsync -a "$CAUSALITY/output/figuras/" "$SCRIPT_DIR/figuras/"

HTML="$SCRIPT_DIR/paper.html"
if [[ -f "$HTML" ]]; then
  # Rutas legacy (../output/figuras) y rutas absolutas → figuras/ junto al HTML
  if sed --version 2>/dev/null | grep -q GNU; then
    sed -i -e 's|\.\./output/figuras/|figuras/|g' \
           -e "s|file://[^\"']*/paper/figuras/|figuras/|g" \
           -e "s|${SCRIPT_DIR}/figuras/|figuras/|g" "$HTML"
  else
    sed -i '' -e 's|\.\./output/figuras/|figuras/|g' \
              -e "s|file://[^\"']*/paper/figuras/|figuras/|g" \
              -e "s|${SCRIPT_DIR}/figuras/|figuras/|g" "$HTML"
  fi
fi
