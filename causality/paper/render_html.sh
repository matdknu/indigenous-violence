#!/usr/bin/env bash
# Render HTML y sincroniza figuras/ junto al paper (vista local + deploy_pages).
set -euo pipefail
cd "$(dirname "$0")"
quarto render paper.qmd --to html --cache-refresh
./post-render-html.sh
