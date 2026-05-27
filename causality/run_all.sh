#!/usr/bin/env bash
# Pipeline completo: datos (01–09) + paper_results + render HTML/DOCX + GitHub Pages.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")" && pwd)"
cd "$ROOT"

echo "═══════════════════════════════════════════════════════════"
echo "  Pipeline ELRI — indigenous-violence/causality"
echo "═══════════════════════════════════════════════════════════"

SCRIPTS=(
  01_limpieza
  02_descriptivos
  03_modelos
  04_robustez
  05_mecanismo
  06_mapa
  07_likert_collapse
  08_identidad_ingroup
  08b_tablas_heterogeneidad_identidad
  09_diagramas
)

for s in "${SCRIPTS[@]}"; do
  echo ""
  echo ">>> R/${s}.R"
  Rscript "R/${s}.R"
done

echo ""
echo ">>> paper_results.rds"
Rscript -e 'source("R/paper_results.R"); refresh_paper_results()'

echo ""
echo ">>> Render HTML"
bash paper/render_html.sh

echo ""
echo ">>> Render DOCX"
cd paper
quarto render paper.qmd --to docx --cache-refresh
cd "$ROOT"

echo ""
echo ">>> Deploy GitHub Pages (docs/)"
bash paper/deploy_pages.sh

echo ""
echo "✓ Pipeline completo."
echo "  HTML:  paper/paper.html"
echo "  DOCX:  paper/paper.docx"
echo "  Pages: ../docs/index.html"
