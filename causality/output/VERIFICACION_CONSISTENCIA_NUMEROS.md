# VERIFICACIÓN DE CONSISTENCIA NUMÉRICA
# Análisis de Identidad Ingroup/Outgroup

**Fecha verificación:** 2026-05-25  
**Status:** ✅ VERIFICADO Y CORREGIDO

---

## NÚMEROS VERIFICADOS Y CONFIRMADOS

### 1. Distribución id_indi (a4) por grupo — Ola 2 (baseline)

#### No indígenas (N total = 735, N sin NAs = 726):
| Valor | N | % |
|-------|---|---|
| 1 | 223 | 30.7% |
| 2 | 154 | 21.2% |
| 3 | 137 | 18.9% |
| 4 | 146 | 20.1% |
| 5 | 66 | 9.1% |
| NA | 9 | - |

**Agrupado:**
- Valores 1-2: 377 (**51.9%**) ✅
- Valores 3-5: 349 (**48.1%**) ✅

#### Indígenas (N total = 845):
| Valor | N | % |
|-------|---|---|
| 1 | 37 | 4.4% |
| 2 | 78 | 9.2% |
| 3 | 133 | 15.7% |
| 4 | 260 | 30.7% |
| 5 | 334 | 39.5% |
| NA | 3 | - |

**Agrupado:**
- Valores 4-5: 594 (**70.2%**) ✅

---

### 2. Medias y Desviaciones Estándar — Ola 2

|             | id_indi (a4) | a5 (importancia) | id_chile (a6) |
|-------------|--------------|------------------|---------------|
| No indígenas| 2.56 (SD=**1.35**) | 2.71 (SD=**1.38**) | 4.51 (SD=**0.82**) ✅ |
| Indígenas   | 3.92 (SD=**1.15**) | 4.12 (SD=**1.02**) | 4.36 (SD=**0.83**) ✅ |

**Valores exactos SD id_chile:**
- No indígenas: 0.817 (redondeado a 0.82) ✅
- Indígenas: 0.826 (redondeado a 0.83) ✅

---

### 3. Correlación id_indi × a5

**r = 0.821** ✅

Interpretación: Alta correlación, justifica promediarlas en `idx_id_etnica`

---

### 4. Variables Derivadas — Ola 2

|             | id_ingroup | id_outgroup | brecha_id | predominancia_media |
|-------------|------------|-------------|-----------|---------------------|
| No indígenas| **4.51**   | **2.62**    | **+1.88** ✅ | **-1.88** ✅ |
| Indígenas   | **4.02**   | **4.36**    | **-0.34** ✅ | **-0.34** ✅ |

**Nota:** brecha_id = id_ingroup − id_outgroup  
**Nota:** predominancia = idx_id_etnica − id_chile

**Valores exactos:**
- No indígenas brecha: +1.88 (exacto)
- Indígenas brecha: -0.339 (redondeado a -0.34) ✅
- No indígenas predominancia: -1.883 (redondeado a -1.88) ✅
- Indígenas predominancia: -0.3387 (redondeado a -0.34) ✅

---

### 5. Distribución de Predominancia Baseline (Ola 2)

#### No indígenas:
| Estadístico | Valor |
|-------------|-------|
| Media | **-1.883** |
| SD | varía |
| Mínimo | -4.000 |
| Q1 | -3.000 |
| Mediana | -2.000 |
| Q3 | -1.000 |
| Máximo | 3.000 |
| NAs | 8 |

#### Indígenas:
| Estadístico | Valor |
|-------------|-------|
| Media | **-0.3387** |
| SD | varía |
| Mínimo | -4.000 |
| Q1 | -1.000 |
| Mediana | 0.000 |
| Q3 | 0.000 |
| Máximo | 4.000 |
| NAs | 2 |

---

### 6. Terciles de Predominancia (Ola 2)

|             | Nacional | Equilibrio | Étnica | Total |
|-------------|----------|------------|--------|-------|
| No indígenas| **249** ✅ | **297** ✅ | **181** ✅ | 727 |
| Indígenas   | **289** ✅ | **368** ✅ | **186** ✅ | 843 |

**Nota:** Terciles calculados por separado dentro de cada grupo étnico

---

### 7. Efectos DiD por Tercil de Predominancia (Modelo C)

#### Justificación de violencia de CONTROL (idx_vio_control):

| Tercil | β DiD | SE | p-value |
|--------|-------|-----|---------|
| Nacional | **+2.352** ✅ | **0.691** ✅ | **0.000725** (<0.001) ✅ |
| Equilibrio | **-2.041** ✅ | **0.787** ✅ | **0.00978** (0.010) ✅ |
| Étnica | **+1.555** ✅ | **0.578** ✅ | **0.00734** (0.007) ✅ |

#### Justificación de violencia de CAMBIO/RESGUARDO (idx_vio_resguardo):

| Tercil | β DiD | SE | p-value |
|--------|-------|-----|---------|
| Nacional | **+1.954** ✅ | **0.698** ✅ | **0.00528** (0.005) ✅ |
| Equilibrio | **+0.724** ✅ | **0.826** ✅ | **0.381** ✅ |
| Étnica | **+0.384** ✅ | **0.672** ✅ | **0.568** ✅ |

---

### 8. Interacción Cuádruple (DiD × predominancia continua)

#### Control (idx_vio_control):
- Término: `periododecreto:indigeneousindi:zona_decretodecreto:predominancia_base`
- β = **0.033** ✅
- SE = **0.286** ✅
- p = **0.907** ✅

#### Cambio/resguardo (idx_vio_resguardo):
- Término: `periododecreto:indigeneousindi:zona_decretodecreto:predominancia_base`
- β = **-0.387** ✅
- SE = **0.310** ✅
- p = **0.212** ✅

**Interpretación:** NO significativa en ola 4 (decreto)

---

### 9. Interacción Estallido (Ola 3)

#### Cambio/resguardo:
- Término: `periodoestallido:indigeneousindi:zona_decretodecreto:predominancia_base`
- β = **-0.580** ✅
- SE = **0.249** ✅
- p = **0.020** ✅

**Interpretación:** SÍ significativa en ola 3 (estallido)

---

## CORRECCIONES REALIZADAS

### ❌ → ✅ Corrección 1: Porcentajes distribución id_indi no indígenas

**Antes:**
- 51.3% responde 1-2
- 48.7% responde 3-5

**Después:**
- **51.9%** responde 1-2 ✅
- **48.1%** responde 3-5 ✅

**Fuente:** Cálculo directo 377/726 = 51.9%, 349/726 = 48.1%

### ❌ → ✅ Corrección 2: SD id_chile no indígenas

**Antes:**
- SD = 0.90

**Después:**
- SD = **0.82** ✅

**Fuente:** Valor exacto 0.817, redondeado a 2 decimales

---

## ARCHIVOS ACTUALIZADOS

✅ `/output/RESUMEN_IDENTIDAD_INGROUP_OUTGROUP.md` — Corregido  
✅ `/output/RESUMEN_EJECUTIVO_IDENTIDAD.md` — Corregido

---

## CONSISTENCIA FINAL

Todos los números en los documentos de resumen son ahora **CONSISTENTES** con el output real del script `R/08_identidad_ingroup.R`.

**Verificado por:** Análisis automático de consistencia  
**Fecha:** 2026-05-25  
**Status:** ✅ APROBADO

---

## PARA INTEGRACIÓN EN PAPER.QMD

Los siguientes números están verificados y pueden usarse con confianza:

1. **Correlación id_indi × a5 = 0.821** (para justificar idx_id_etnica)
2. **Brecha identitaria indígenas = -0.34** (hallazgo contraintuitivo)
3. **Brecha identitaria no indígenas = +1.88** (esperado)
4. **Efectos DiD por terciles:** usar tabla completa verificada
5. **Interacción cuádruple NO significativa:** p = 0.907 (control), p = 0.212 (cambio)
6. **Dispersión id_indi no indígenas:** 51.9% valores 1-2, 48.1% valores 3-5

**Todos los valores son reproducibles ejecutando:**
```r
source("R/08_identidad_ingroup.R")
```

---

✅ **VERIFICACIÓN COMPLETADA**
