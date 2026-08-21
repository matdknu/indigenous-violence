# Verificación de consistencia — paper.qmd vs pipeline

**Generado:** 2026-08-17 14:13
**Estado global:** ✅ CONSISTENTE

## 1. Especificación analítica

| Elemento | Valor |
|----------|-------|
| idx_vio_control | d3_1 (ítem único) |
| idx_vio_resguardo | d4_3 (ítem único) |
| indigeneous | a1 ∈ 1–11 vs 12 |
| perc_injusticia | excluida de modelos principales |

## 2. Muestra

| Métrica | Valor |
|---------|-------|
| Folios únicos | 1580 |
| Persona-olas (olas 2–4) | 4740 |
| Indígenas baseline (ola 2) | 845 |
| No indígenas baseline (ola 2) | 735 |
| N Modelo C (por VD) | 4482 |
| N FE magro (por VD) | 4685 |
| Duplicados folio×ola | 0 |

## 3. τ₄ decreto — especificaciones

| Especificación | Control β (p) | Cambio β (p) |
|----------------|---------------|--------------|
| FE magro (principal) | 0,313 (,062) | 0,413 (,009) |
| Modelo C (RE + controles) | 0,298 (,153) | 0,494 (,009) |

## 4. Mediación

| Paso | β | p | % mediación control | % supresión cambio |
|------|---|----|---------------------|--------------------|
| Decreto → ingroup | 0,602 | < .001 | ~43% | ~13% |

## 5. Descriptivos baseline (ola 2)

| Grupo | N | Control | Cambio | Just. ingroup |
|-------|---|---------|--------|---------------|
| no_indi, lejos | 568 | 2,32 | 1,65 | 2,96 |
| no_indi, cerca | 167 | 2,26 | 1,17 | 3,34 |
| indi, lejos | 656 | 2,13 | 1,92 | 2,62 |
| indi, cerca | 189 | 2,16 | 1,30 | 3,15 |

**Patrón tendencias paralelas (indi):** cambio social zona = 1,30 vs fuera = 1,92 (dirección opuesta al τ₄ post-decreto).

## 6. Robustez (τ₄ decreto)

| Especificación | Control | Cambio |
|----------------|---------|--------|
| Principal FE magro | 0,313 + | 0,413 ** |
| C — DiD decreto | 0,298  | 0,494 ** |
| PSM | 0,449  | 0,534 * |
| IPW original | 0,387 * | 0,385 * |
| IPW trim 5–95% | 0,328 + | 0,403 * |
| FE + controles (fixest) | 0,375 * | 0,488 ** |
| OLS cluster comuna | 0,288 + | 0,497 ** |

## 7. Chequeos automáticos paper_results ↔ modelos

| Chequeo | Pipeline | paper_results | OK |
|---------|----------|---------------|-----|
| τ₄ control (FE magro principal) | 0,313 | 0,313 | ✓ |
| τ₄ cambio (FE magro principal) | 0,413 | 0,413 | ✓ |
| τ₄ control (Modelo C sensibilidad) | 0,298 | 0,298 | ✓ |
| τ₄ cambio (Modelo C sensibilidad) | 0,494 | 0,494 | ✓ |
| τ₃ control (estallido DiD) | 0,050 | 0,050 | ✓ |
| τ₃ cambio (estallido DiD) | 0,093 | 0,093 | ✓ |
| Efecto período ola 3 — cambio | 0,311 | 0,311 | ✓ |
| Efecto período ola 4 — control | 0,101 | 0,104 | ✓ |
| Mediación paso 1 — ingroup | 0,602 | 0,602 | ✓ |
| Mediación control (%) | 42,636 | 42,636 | ✓ |
| Supresión cambio (%) | 12,962 | 12,962 | ✓ |
| Baseline indi zona — cambio social | 1,303 | 1,303 | ✓ |
| Baseline indi fuera — cambio social | 1,921 | 1,921 | ✓ |
| N Modelo C | 4482,000 | 4482,000 | ✓ |
| N FE magro | 4685,000 | 4685,000 | ✓ |

## 8. Inconsistencias detectadas

_Ninguna._

## 9. Notas narrativas (recordatorio)

- τ₄ control FE n.s. (p = ,062): narrativa debe enfatizar regularización (Paso 1), no efecto directo.
- τ₄ cambio FE significativo (p = ,009): hallazgo central confirmado.
- Mecanismo usa VD continua 1–5 (alineado con Modelo C).
- IPW estimator: feols_weights_cluster_comuna

## 10. Tabla antes/después del fix de tablas

### τ₄ (decreto × indígena × zona)

| Especificación | Control (antes → después) | Cambio (antes → después) |
|----------------|---------------------------|--------------------------|
| FE magro (principal) | 0,313+ → 0,313 (+) | 0,413** → 0,413 (**) |
| Modelo C (antes citado como principal) | 0,298 ns → 0,298 (ns) | 0,494** → 0,494 (**) |
| FE + controles (04 vs 03) | 0,375 → 0,375 | 0,488 → 0,488 |

### DR-DiD (decreto ola 2→4)

| VD | ATT indi | ATT no indi | Δ ATT | p(Δ) |
|----|----------|-------------|-------|------|
| Control social | 1,571 | 1,241 | 0,330 | ,235 |
| Cambio social | 1,150 | 0,830 | 0,320 | ,129 |

### CLMM ordinal A — cambio social

- **Ordinal A (simétrico):** β = 0,057, p = ,919, Conv = ⚠ no converge
- **Lineal cód. A:** β = 0,256, p = ,018, Conv = —

## 11. Cómo reproducir

```bash
cd causality/
bash run_all.sh                    # pipeline completo 01→09
Rscript R/verificar_paper_consistencia.R
Rscript -e 'source("R/paper_results.R"); refresh_paper_results()'
cd paper && quarto render paper.qmd --cache-refresh
```

Scripts clave: `R/01_limpieza.R`, `R/03_modelos.R`, `R/04_robustez.R`,
`R/05_mecanismo.R`, `R/paper_results.R`.

