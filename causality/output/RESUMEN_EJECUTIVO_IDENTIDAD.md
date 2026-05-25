# RESUMEN EJECUTIVO: Análisis de Identidad Ingroup/Outgroup Completado

**Fecha:** 2026-05-25  
**Status:** ✅ Completado exitosamente

---

## ARCHIVOS GENERADOS

### 📊 Figuras (PNG, 300 DPI)

1. **`output/figuras/fig_identidad_distribucion.png`** (82 KB)
   - Distribución de id_indi (a4) e id_chile (a6) por grupo étnico
   - Muestra varianza suficiente en ambas variables para análisis ingroup/outgroup

2. **`output/figuras/fig_predominancia_trayectorias.png`** (227 KB)
   - Trayectorias temporales de predominancia identitaria (étnica − nacional)
   - Por grupo étnico y zona decreto
   - Demuestra ausencia de post-tratamiento (trayectorias paralelas)

3. **`output/figuras/fig_identidad_ingroup_outgroup.png`** (252 KB)
   - Identidad con MI grupo vs. OTRO grupo
   - Revela estructura asimétrica: indígenas con identidad dual, no indígenas con identidad exclusiva

### 📋 Tablas (HTML y DOCX)

1. **`output/tablas/tabla_heterog_predominancia.html`**
   - Tabla principal: efectos DiD por tercil de predominancia
   - Muestra heterogeneidad significativa (efectos no monótonos)

2. **`output/tablas/tabla_heterog_predominancia.docx`**
   - Misma tabla en formato Word para inserción directa en manuscrito

3. **`output/tablas/tabla_predominancia_descriptiva.html`**
   - Estadísticas descriptivas de predominancia por grupo
   - Medias, SD, cuartiles

4. **`output/tablas/tabla_terciles_grupo.html`**
   - Distribución de respondentes por tercil y grupo étnico
   - N (%) en cada categoría

### 📜 Scripts R

1. **`R/08_identidad_ingroup.R`** — Script diagnóstico principal
   - Carga datos y variables de identidad (id_indi/a4, a5, id_chile/a6)
   - Crea variables derivadas (predominancia, ingroup/outgroup, terciles)
   - Genera figuras diagnósticas
   - Modelos DiD por tercil de predominancia
   - Modelos de interacción continua

2. **`R/08b_tablas_heterogeneidad_identidad.R`** — Tablas publicables
   - Regenera variables para consistencia
   - Genera tablas HTML y DOCX con formato profesional
   - Tablas descriptivas adicionales

### 📄 Documentación

1. **`output/RESUMEN_IDENTIDAD_INGROUP_OUTGROUP.md`** — Documento analítico completo
   - Respuestas a las 5 preguntas clave
   - Interpretación de resultados
   - Hallazgos contraintuitivos
   - Recomendaciones para publicación
   - Limitaciones y próximos pasos

---

## HALLAZGOS PRINCIPALES

### ✅ 1. Viabilidad técnica del análisis

- **id_indi (a4) tiene varianza entre no indígenas:** 51.9% responde 1-2, pero 48.1% responde 3-5
  → Suficiente dispersión para análisis ingroup/outgroup

- **Correlación id_indi × a5 = 0.821** (muy alta)
  → Se pueden promediar en `idx_id_etnica`

- **No hay evidencia de post-tratamiento:** trayectorias de predominancia son paralelas entre zona decreto y fuera

### 🔍 2. Hallazgo principal: Heterogeneidad no monótona

**Efecto DiD sobre justificación de violencia de CONTROL por tercil:**

| Tercil          | β DiD   | p-value | Interpretación                    |
|-----------------|---------|---------|-----------------------------------|
| Nacional        | +2.352  | <0.001  | Efecto POSITIVO fuerte            |
| Equilibrio      | -2.041  | 0.010   | Efecto NEGATIVO fuerte (invertido)|
| Étnica          | +1.555  | 0.007   | Efecto POSITIVO moderado          |

**Interpretación:**
- El decreto incrementa justificación de violencia entre quienes tienen predominancia identitaria CLARA (nacional o étnica)
- El efecto se INVIERTE entre quienes tienen identidad equilibrada/dual
- La ambivalencia identitaria protege contra legitimación de coerción estatal

### ⚠️ 3. Hallazgo contraintuitivo: Identidad dual indígena

**Brecha identitaria baseline (ola 2):**
- **No indígenas:** brecha_id = +1.88 (se identifican MÁS con Chile que con pueblos originarios) ✅ esperado
- **Indígenas:** brecha_id = -0.34 (se identifican LIGERAMENTE más con Chile que con su pueblo) ⚠️ contraintuitivo

**Implicación:** Los indígenas chilenos muestran identidad dual con leve predominancia de identidad nacional, no étnica. Esto puede reflejar:
1. Presión asimilacionista histórica
2. Identificación nacional no excluyente de identidad étnica
3. Efecto de panel balanceado (sobreviven quienes están más integrados)

### ❌ 4. Interacción continua NO significativa

- La interacción cuádruple `DiD × predominancia_continua` NO es significativa (p = 0.907 para control)
- La heterogeneidad es NO LINEAL, mejor capturada por terciles que por modelo continuo
- **Excepción:** La interacción SÍ es significativa para el estallido social (ola 3), no el decreto (ola 4)
  - `periodoestallido:indi:decreto:predom` → β = -0.580, p = 0.020 para resguardo
  - Sugiere que predominancia condicionó respuesta al estallido, no al decreto

---

## RECOMENDACIÓN ANALÍTICA

### ✅ Contribución publicable: Análisis de heterogeneidad por terciles

**Narrativa sugerida:**

> "El efecto del decreto sobre justificación de violencia varía según el perfil identitario baseline de los individuos. Entre quienes tienen predominancia identitaria clara (sea nacional o étnica), el decreto incrementa la justificación de violencia de control (β = +2.35 para predominancia nacional, β = +1.56 para predominancia étnica, ambos p < 0.01). Sin embargo, entre quienes tienen identidad equilibrada/dual, el efecto se invierte significativamente (β = -2.04, p = 0.01), sugiriendo que la ambivalencia identitaria protege contra la legitimación de coerción estatal hacia pueblos originarios."

### 📊 Tablas/figuras a incluir en manuscrito

1. **Tabla 1 (descriptiva):** `tabla_predominancia_descriptiva.html`
   - Distribución de predominancia por grupo étnico

2. **Tabla 2 (principal):** `tabla_heterog_predominancia.docx`
   - Efectos DiD por tercil de predominancia

3. **Figura 1:** `fig_identidad_ingroup_outgroup.png`
   - Estructura identitaria asimétrica (indígenas dual, no indígenas exclusiva)

4. **Figura 2 (appendix):** `fig_predominancia_trayectorias.png`
   - Ausencia de post-tratamiento (trayectorias paralelas)

5. **Figura 3 (appendix):** `fig_identidad_distribucion.png`
   - Distribución de variables de identidad (validación de varianza)

---

## DECISIONES ANALÍTICAS

### ✅ Usar predominancia por terciles (NO continua)
- Razón: captura heterogeneidad no lineal
- Método: estratificar modelos DiD por tercil de predominancia baseline

### ❌ NO usar estructura ingroup/outgroup simétrica
- Razón: estructura identitaria es asimétrica entre grupos
- Indígenas tienen identidad dual (alto en ambas), no indígenas tienen identidad exclusiva

### ✅ Fijar predominancia al baseline (ola 2)
- Razón: evitar sesgo de post-tratamiento
- Método: crear `predominancia_base` fijada en ola 2, usar para terciles en todas las olas

---

## LIMITACIONES

1. **Identidad dual indígena:** No captura "fortaleza étnica" sino "balance entre identidades"
2. **No linealidad:** Relación entre predominancia y efecto DiD no es lineal
3. **Efecto en estallido, no decreto:** Predominancia condicionó respuesta a estallido (ola 3), no a decreto (ola 4)

---

## PRÓXIMOS PASOS (OPCIONAL)

Si se desea profundizar:

1. **Análisis solo entre indígenas:** Verificar si heterogeneidad se mantiene dentro de grupo étnico
2. **Explorar efectos del estallido:** Analizar `periodoestallido` como tratamiento primario
3. **Descomponer idx_id_etnica:** Explorar efectos diferenciados de a4 (identificación) vs. a5 (importancia)
4. **Variable de identidad dual:**
   - `id_dual = (idx_id_etnica + id_chile) / 2`
   - Testar si alta identidad dual modera efecto DiD

---

## EJECUCIÓN

Para reproducir todos los resultados:

```r
# 1. Diagnóstico completo y figuras
source("R/08_identidad_ingroup.R")

# 2. Tablas publicables
source("R/08b_tablas_heterogeneidad_identidad.R")
```

**Tiempo de ejecución:** ~5 segundos total

---

## CONTACTO Y SOPORTE

Para preguntas sobre interpretación o extensiones del análisis, consultar:
- `output/RESUMEN_IDENTIDAD_INGROUP_OUTGROUP.md` — análisis detallado
- Scripts con comentarios in-line
- Output de consola guardado en workspace

---

**✅ Análisis completado exitosamente el 2026-05-25**
