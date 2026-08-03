# Esquema de paper — ELRI

## Variables dependientes

| Índice | Concepto | Ítems | Sentido |
|--------|----------|-------|---------|
| **idx_vio_control** | Justificación de **represión estatal** | d3_1 (ítem único) | Carabineros repriman protestas |
| **idx_vio_cambio** | Violencia de **cambio social** | d4_2 + d4_3 | Acciones pro-indígenas (tomas, cortes) |

*Excluido del análisis:* justicia procedimental (d5_1/d5_2) — no es variable dependiente.

## Título tentativo

**"Apertura normativa y contención fallida: estallido social, estado de excepción y justificación de la violencia entre personas indígenas y no indígenas en Chile (2018–2023)"**

## Abstract (estructura)

Panel longitudinal espejo (ELRI, N=1.578 panel balanceado olas 2–4, 4.742 persona-olas) + DiD exploratorio. Shock: estado de excepción oct. 2021 (53 comunas, La Araucanía + Biobío). VDs: represión estatal (d3_1, ítem único) y cambio social (d4_2+d4_3). Modelo C τ₄: control β≈0,46 (n.s.), cambio β≈0,82* (p≈,035).

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
