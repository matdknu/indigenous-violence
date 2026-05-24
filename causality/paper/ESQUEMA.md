# Esquema de paper — ELRI

## Variables dependientes

| Índice | Concepto | Ítems | Sentido |
|--------|----------|-------|---------|
| **idx_vio_control** | Violencia de **control social** | d3_1 + d3_2 | Carabineros/civiles sobre indígenas |
| **idx_vio_cambio** | Violencia de **cambio social** | d4_2 + d4_3 | Acciones pro-indígenas (tomas, cortes) |

*Excluido del análisis:* justicia procedimental (d5_1/d5_2) — no es variable dependiente.

## Título tentativo

**"Estado de excepción, identidad étnica y justificación de la violencia: evidencia cuasi-experimental de un panel longitudinal en Chile (2016–2022)"**

## Abstract (estructura)

Panel longitudinal espejo (ELRI, N=1.592, 4 olas 2016–2022) + DiD exploratorio. Shock: estado de excepción oct. 2021. VDs: justificación de violencia de **control social** (Estado/Carabineros → indígenas) y de **cambio social** (indígenas → transformación territorial). Resultado: brecha actitudinal ampliada con efecto rezagado (ola 4). Vio. control predice voto Rechazo (OR≈1.36); vio. cambio lo reduce (OR≈0.81).

## Hipótesis

- **H1:** Indígenas justifican más cambio social y menos control social.
- **H2:** Justicia procedimental (teórica; no estimada como VD).
- **H3–H4:** Efecto DiD del estado de excepción, rezagado en ola 4.
- **H5:** Control → Rechazo (+); cambio → Rechazo (−).

## Render

```bash
cd causality
Rscript R/01_limpieza.R
Rscript R/03_modelos.R
quarto render paper/paper.qmd
```
