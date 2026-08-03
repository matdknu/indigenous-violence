#!/usr/bin/env bash
# Pipeline LCGA — trayectorias de violencia por cambio social (ELRI)
set -euo pipefail

ROOT="$(cd "$(dirname "$0")" && pwd)"
cd "$ROOT"

echo "═══════════════════════════════════════════════════════════"
echo "  deneken_lcga_cambio — LCGA violencia cambio social ELRI"
echo "═══════════════════════════════════════════════════════════"

for s in 00_denek_preparar_panel 01_denek_estimar_lcga 02_denek_graficar_clases; do
  echo ""
  echo ">>> R/${s}.R"
  Rscript "R/${s}.R"
done

echo ""
echo "✓ Pipeline LCGA denek completo."
echo "  Figura: output/figuras/denek_trayectorias_vio_camb_lcga.png"
echo "  Modelos: data/data_proc/denek_lcga_resultados.rds"
