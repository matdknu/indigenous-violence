#!/usr/bin/env bash
# Publica el paper en docs/ para GitHub Pages (matdknu.github.io/indigenous-violence/)
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
CAUSALITY="$(cd "$SCRIPT_DIR/.." && pwd)"
REPO_ROOT="$(cd "$CAUSALITY/.." && pwd)"
DOCS="$REPO_ROOT/docs"

cd "$SCRIPT_DIR"
if [[ "${1:-}" == "--render" ]]; then
  ./render_html.sh
fi

if [[ ! -f paper.html ]]; then
  echo "No existe paper.html. Ejecuta: quarto render paper.qmd --to html" >&2
  exit 1
fi

mkdir -p "$DOCS/figuras" "$DOCS/tablas"
sed 's|\.\./output/figuras/|figuras/|g' paper.html > "$DOCS/index.html"
rm -rf "$DOCS/paper_files"
cp -R paper_files "$DOCS/"
if [[ -d "$SCRIPT_DIR/figuras" ]]; then
  rsync -a "$SCRIPT_DIR/figuras/" "$DOCS/figuras/"
else
  rsync -a "$CAUSALITY/output/figuras/" "$DOCS/figuras/"
fi
rsync -a "$CAUSALITY/output/tablas/" "$DOCS/tablas/"

echo "✓ GitHub Pages: $DOCS/index.html ($(wc -l < "$DOCS/index.html") líneas)"
echo "  Figuras: $(ls "$DOCS/figuras" | wc -l | tr -d ' ') archivos"
