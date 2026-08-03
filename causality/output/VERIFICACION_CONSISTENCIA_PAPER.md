# Verificación de consistencia — paper.qmd vs pipeline

**Generado:** 2026-08-03 16:51
**Estado global:** ✅ CONSISTENTE

## 1. Especificación analítica

| Elemento | Valor |
|----------|-------|
| idx_vio_control | d3_1 (ítem único) |
| idx_vio_resguardo | promedio d4_2 + d4_3 |
| indigeneous | a1 ∈ 1–11 vs 12 |
| perc_injusticia | excluida de modelos principales |

## 2. Muestra

| Métrica | Valor |
|---------|-------|
| Folios únicos | 1580 |
| Persona-olas (olas 2–4) | 4740 |
| Indígenas baseline (ola 2) | 845 |
| No indígenas baseline (ola 2) | 735 |
| N Modelo C (por VD) | 1844 |
| Duplicados folio×ola | 0 |

## 3. Modelo C — coeficientes principales

| Término | VD | β | SE | p | En paper_results | ✓ |
|---------|----|---|----|---|------------------|---|
| DiD decreto (τ₄) | Control | 0,476 | 0,435 | ,273 | 0,476 | ✓ |
| DiD decreto (τ₄) | Cambio | 0,822 | 0,389 | ,035 | 0,822 | ✓ |
| DiD estallido (τ₃) | Cambio | -0,090 | 0,331 | ,786 | — | — |
| periodoestallido | Cambio | 0,283 | 0,092 | ,002 | 0,283 | ✓ |
| periododecreto | Control | 0,316 | 0,115 | ,006 | 0,316 | ✓ |

## 4. Mediación

| Paso | β | p | % mediación control | % supresión cambio |
|------|---|----|---------------------|--------------------|
| Decreto → ingroup | 0,745 | ,035 | ~30% | ~15% |

## 5. Descriptivos baseline (ola 2)

| Grupo | N | Control | Cambio | Just. ingroup |
|-------|---|---------|--------|---------------|
| no_indi, lejos | 568 | 2,32 | 1,85 | 2,96 |
| no_indi, cerca | 167 | 2,26 | 1,28 | 3,34 |
| indi, lejos | 656 | 2,13 | 2,10 | 2,62 |
| indi, cerca | 189 | 2,16 | 1,36 | 3,15 |

**Patrón tendencias paralelas (indi):** cambio social zona = 1,36 vs fuera = 2,10 (dirección opuesta al τ₄ post-decreto).

## 6. Robustez (τ₄ decreto)

| Especificación | Control | Cambio |
|----------------|---------|--------|
| C — DiD decreto | 0,476  | 0,822 * |
| B — Decreto (3→4) | 0,300  | 0,864 * |
| PSM | 0,782  | 0,717  |
| IPW original | 0,699 * | 1,060 ** |
| IPW trim 5–95% | 0,639 + | 0,987 * |
| Placebo real (ola1→2) | -0,132  | 0,160  |

## 7. Chequeos automáticos paper_results ↔ modelos

| Chequeo | Pipeline | paper_results | OK |
|---------|----------|---------------|-----|
| τ₄ control (Modelo C) | 0,476 | 0,476 | ✓ |
| τ₄ cambio (Modelo C) | 0,822 | 0,822 | ✓ |
| τ₃ control (estallido DiD) | 0,063 | 0,063 | ✓ |
| τ₃ cambio (estallido DiD) | -0,090 | -0,090 | ✓ |
| Efecto período ola 3 — cambio | 0,283 | 0,283 | ✓ |
| Efecto período ola 4 — control | 0,316 | 0,316 | ✓ |
| Mediación paso 1 — ingroup | 0,745 | 0,745 | ✓ |
| Mediación control (%) | 30,104 | 30,104 | ✓ |
| Supresión cambio (%) | 15,006 | 15,006 | ✓ |
| Baseline indi zona — cambio social | 1,364 | 1,364 | ✓ |
| Baseline indi fuera — cambio social | 2,099 | 2,099 | ✓ |
| N Modelo C | 1844,000 | 1844,000 | ✓ |

## 8. Inconsistencias detectadas

_Ninguna._

## 9. Notas narrativas (recordatorio)

- τ₄ control n.s. (p = ,273): narrativa debe enfatizar regularización (Paso 1), no efecto directo.
- τ₄ cambio significativo (p = ,035): hallazgo central confirmado.
- Mecanismo usa VD continua 1–5 (alineado con Modelo C).
- IPW estimator: feols_weights_cluster_comuna

## 10. Cómo reproducir

```bash
cd causality/
bash run_all.sh                    # pipeline completo 01→09
Rscript R/verificar_paper_consistencia.R
Rscript -e 'source("R/paper_results.R"); refresh_paper_results()'
cd paper && quarto render paper.qmd --cache-refresh
```

Scripts clave: `R/01_limpieza.R`, `R/03_modelos.R`, `R/04_robustez.R`,
`R/05_mecanismo.R`, `R/paper_results.R`.

