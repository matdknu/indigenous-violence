#!/usr/bin/env bash
# Genera paper.docx con todas las tablas embebidas (flextable).
# Ejecutar desde la raíz del proyecto causality/:
#   bash paper/render_docx.sh

set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

echo "==> Pipeline de datos (01–05)..."
for s in 01_limpieza 02_descriptivos 03_modelos 04_robustez 05_mecanismo; do
  Rscript "R/${s}.R"
done

echo "==> Render DOCX..."
cd paper
quarto render paper.qmd --to docx --cache-refresh

echo ""
echo "✓ Listo: paper/paper.docx"
