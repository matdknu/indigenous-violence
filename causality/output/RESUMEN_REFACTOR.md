# Resumen del Refactor — VD ítem único (patch medición)

**Fecha:** 2026-08-09  
**Cambio clave:** ambas VD pasan a **ítem único** (validez de contenido). Se elimina cualquier índice compuesto / alfa de Cronbach para estas VD.

---

## Definición de variables dependientes

| VD | Código | Ítem ELRI | Contenido | Tipo |
|---|---|---|---|---|
| **Control social estatal** | `idx_vio_control` | **d3_1** | Fuerza de Carabineros (coerción estatal) | Ítem único |
| **Cambio social** | `idx_vio_resguardo` | **d4_3** | Cortes de camino (protesta disruptiva) | Ítem único |

**Excluidos de las VD (solo sensibilidad/apéndice):**
- `vio_priv_agric` = d3_2 (vigilantismo privado / agricultores armados)
- `vio_ocup_tierras` = d4_2 (ocupación territorial / tomas)

**Nota de medición** (en `analysis_metadata$vd_definicion`): ítems únicos elegidos por validez de contenido — no contaminar coerción estatal con vigilantismo ni protesta disruptiva con ocupación. **No alfa de Cronbach.**

### Descriptivos (subset_data)

| VD | Media | SD | % NA | N válido |
|---|---|---|---|---|
| Control (d3_1) | 2.360 | 1.353 | 1.10% | 4688 |
| Cambio (d4_3) | 1.893 | 1.230 | 0.82% | 4701 |

---

## Coeficiente triple DiD decreto (Ola4 × indígena × zona)

| Especificación | Control (d3_1 Carabineros) | | Cambio (d4_3 cortes) | |
|---|---|---|---|---|
| | β | Sig | β | Sig |
| **FE magro + cluster comuna (PRINCIPAL)** | **0.313** | **+** | **0.413** | **\*\*** |
| DR-DiD Δ ATT (conservador) | 0.330 | — | 0.320 | — |
| RE lmer Modelo C (sensibilidad) | 0.298 | — | 0.494 | ** |
| PSM (caliper 0.2) | 0.449 | — | 0.534 | * |
| IPW (feols + pesos + cluster) | 0.387 | * | 0.385 | * |
| Ordinal clmm esquema A | 0.430 | — | 0.057 | — |
| Placebo real (ola 1→2) | −0.058 | — | 0.181 | — |

### Lectura

- **Cambio social (cortes de camino):** efecto del decreto robusto en FE principal (β=0.413, p=.009), lmer, PSM e IPW. Placebo ns. DR-DiD Δ ATT en la misma dirección (0.32) pero no alcanza significancia (p=.13). El clmm ordinal **no** replica el efecto (posible pérdida de potencia por asimetría del ítem: media≈1.9, masa en “Rechaza”).
- **Control social (Carabineros):** efecto borderline en FE magro (β=0.313, p=.062); significativo en IPW y en FE-con-controles de 04; no significativo en lmer C ni placebo. Patrón más frágil que en cambio social.
- **Proceso de politización (ola 3):** nulo en FE y en DR-DiD (Δ ATT ns) — el efecto es específico del decreto.

---

## DR-DiD completo (ítem único)

| Transición | VD | Δ ATT | SE(Δ) | p | IC 95% boot |
|---|---|---|---|---|---|
| Decreto (2→4) | Control (d3_1) | 0.330 | 0.278 | .235 | [−0.212, 0.835] |
| Decreto (2→4) | Cambio (d4_3) | 0.320 | 0.211 | .129 | [−0.077, 0.721] |
| Proceso pol. (2→3) | Control | 0.117 | 0.251 | .643 | [−0.373, 0.594] |
| Proceso pol. (2→3) | Cambio | 0.012 | 0.197 | .951 | [−0.384, 0.358] |
| Placebo (1→2) | Control | −0.149 | 0.224 | .507 | [−0.555, 0.344] |
| Placebo (1→2) | Cambio | 0.193 | 0.153 | .208 | [−0.128, 0.521] |

---

## Checklist de aceptación

- [x] `idx_vio_control = as.numeric(d3_1)`; `idx_vio_resguardo = as.numeric(d4_3)`
- [x] Eliminado `idx_vio_control_dual` y todo `rowMeans` de d3_1+d3_2 / d4_2+d4_3
- [x] `vio_priv_agric` (d3_2) y `vio_ocup_tierras` (d4_2) solo en sensibilidad/apéndice
- [x] Sin α de Cronbach para las VD principales (reemplazado por nota de medición)
- [x] FE magro + cluster comuna = modelo principal; lmer = sensibilidad
- [x] DR-DiD por grupo (decreto / politización / placebo)
- [x] Regla NA 5%; sin análisis electoral; estallido = proceso de politización
- [x] Pipeline re-ejecutado: 01, 02, 03, 03b, 04, 05, 07, 09

---

## Archivos clave regenerados

| Output | Contenido |
|---|---|
| `output/tablas/tabla_fe_principal.html` | FE magro (principal) |
| `output/tablas/tabla_drdid.html` | DR-DiD Δ ATT |
| `output/tablas/tabla_robustez_ordinal.html` | clmm vs lineal |
| `output/tablas/tabla_nota_medicion_vd.html` | Nota de medición (sin α) |
| `output/tablas/tabla_sensibilidad_apendice.html` | d3_2 y d4_2 |
| `data/analysis_metadata.rds` | `vd_definicion` |
| `data/drdid.rds` / `modelos.rds` / `robustez.rds` | Objetos actualizados |
| `output/tablas/tabla_demandas.html` | DiD demandas (redistrib / reconoc) |
| `output/figuras/fig_demandas.png` | Medias predichas FE demandas |
| `data/demandas.rds` | Modelos + α + tabla demandas |

---

## Extensión: demandas indígenas (securitización)

**Script:** `R/08c_demandas.R`  
**Missing:** centralizado en `01_limpieza.R` (`66/77/88/99/8888/9999`); no en 08c.

### Índices (z-score sobre distribución de `subset_data`)

| Índice | Ítems | α | Nota |
|---|---|---|---|
| `idx_dem_redistrib` | e4_2, e4_4, e4_5, e3_5, e3_4 | **0.812** | Principal |
| `idx_dem_redistrib_sinescaduplic` | e4_2, e4_4, e4_5, e3_5 | 0.778 | Sensibilidad (sin doble escaños) |
| `idx_dem_reconoc` | e5_1–e5_4, e4_3 | **0.827** | Principal |

- r(e3_4, e4_5) = 0.541 → ambos en índice; sensibilidad sin e3_4.
- e3_5: **no diluye** α (con=0.812; sin=0.805). Se mantiene por validez de contenido pese a planitud descriptiva.
- Índices **protegidos**: nunca como controles en modelos de violencia.

### Triple DiD (período × indígena × zona)

| Spec | Shock | Redistrib β (sig) | Reconoc β (sig) |
|---|---|---|---|
| **FE magro + cluster (PRINCIPAL)** | Decreto | **−0.001** (—) | **0.094** (—) |
| FE magro | Proceso | −0.019 (—) | −0.157 (—) |
| RE lmer | Decreto | 0.015 (—) | 0.127 (—) |
| RE lmer | Proceso | −0.033 (—) | −0.181 (—) |
| DR-DiD Δ ATT | Decreto | −0.068 (—) | −0.021 (—) |
| DR-DiD Δ ATT | Proceso | −0.049 (—) | −0.243 (+) |
| DR-DiD Δ ATT | Placebo 1→2 | −0.034 (—) | −0.047 (—) |
| FE sin e3_4 | Decreto | 0.027 (—) | — |
| PSM / IPW | Decreto | 0.028 / −0.134 (—) | 0.027 / 0.045 (—) |

### Lectura

- Triple DiD **≈ 0** en ambos índices (FE, RE, PSM, IPW, placebo). No hay efecto territorial del decreto sobre apoyo a demandas.
- Única señal borderline: DR-DiD proceso en reconocimiento (Δ ATT = −0.24, p≈.07); no se replica en FE.
- Efectos de período (nacionales, no triple): redistrib sube en ola 3 (`periodoestallido` β=0.276\*\*\*) y baja levemente en ola 4 (ns). Reconocimiento sin movimiento de período claro.
- Patrón compatible con **cambio macro-nacional** (si lo hay), no con securitización territorial del D.S. 418.
