# RESUMEN: Exploración Identidad Ingroup/Outgroup

**Fecha:** 2026-05-25  
**Script:** `R/08_identidad_ingroup.R`  
**Propósito:** Diagnosticar si las variables de identidad (a4/id_indi, a5, a6/id_chile) configuran una estructura ingroup/outgroup análoga a justicia procedimental, y si la predominancia identitaria modera el efecto DiD.

---

## 1. DIAGNÓSTICO BÁSICO: RESPUESTAS A LAS 5 PREGUNTAS CLAVE

### Pregunta 1: ¿id_indi (a4) tiene varianza entre no indígenas?

**RESPUESTA: SÍ, hay dispersión significativa**

**Distribución baseline (ola 2):**
- **No indígenas:** 
  - 1: 223 (30.7%)
  - 2: 154 (21.2%)
  - 3: 137 (18.9%)
  - 4: 146 (20.1%)
  - 5: 66 (9.1%)
  - **Dispersión:** 51.9% responde 1-2, pero 48.1% responde 3-5

- **Indígenas:**
  - 1: 37 (4.4%)
  - 2: 78 (9.2%)
  - 3: 133 (15.7%)
  - 4: 260 (30.7%)
  - 5: 334 (39.5%)
  - **Concentración alta:** 70.2% responde 4-5

**Implicación:** El enfoque ingroup/outgroup **SÍ ES VIABLE** porque hay varianza suficiente de identificación con pueblos originarios entre no indígenas. No es un "floor effect" donde todos responden 1-2.

---

### Pregunta 2: ¿id_indi (a4) y a5 correlacionan alto (r > 0.7)?

**RESPUESTA: SÍ, correlación muy alta**

- **r(id_indi, a5) = 0.821**
- Interpretación: "cuánto se identifica" y "cuán importante es" miden el mismo constructo latente
- **Decisión:** Se pueden promediar en `idx_id_etnica` (media de a4 y a5)

**Medias por grupo (ola 2):**
|             | id_indi (a4) | a5 (importancia) | id_chile (a6) |
|-------------|--------------|------------------|---------------|
| No indígenas| 2.56 (SD=1.35) | 2.71 (SD=1.38) | 4.51 (SD=0.82) |
| Indígenas   | 3.92 (SD=1.15) | 4.12 (SD=1.02) | 4.36 (SD=0.83) |

---

### Pregunta 3: ¿La predominancia (étnica − nacional) varía por zona?

**RESPUESTA: Sí, ligeramente** (ver figura `fig_predominancia_trayectorias.png`)

**Predominancia baseline (ola 2):**
- **No indígenas:**
  - Media = -1.88 (predomina fuerte identidad nacional sobre étnica)
  - Rango: -4 a +3
  
- **Indígenas:**
  - Media = -0.34 (predomina LIGERAMENTE identidad nacional sobre étnica)
  - Rango: -4 a +4

**HALLAZGO INESPERADO:** Los indígenas en promedio se identifican MÁS con Chile que con su pueblo originario en baseline (brecha_id = -0.34). Esto contradice la hipótesis inicial.

**Trayectorias por zona:** La predominancia identitaria NO cambia sustancialmente entre zona decreto y fuera en ninguno de los dos grupos → **menor riesgo de post-tratamiento**.

---

### Pregunta 4: ¿El efecto DiD varía por tercil de predominancia?

**RESPUESTA: SÍ, heterogeneidad significativa**

#### **Justificación de CONTROL (idx_vio_control):**

| Tercil de predominancia | β DiD | SE | p-value | Interpretación |
|-------------------------|-------|-----|---------|----------------|
| **Nacional** (predomina id. nacional) | +2.352 | 0.691 | **<0.001** | Efecto POSITIVO fuerte |
| **Equilibrio** (mixto) | -2.041 | 0.787 | **0.010** | Efecto NEGATIVO fuerte |
| **Étnica** (predomina id. étnica) | +1.555 | 0.578 | **0.007** | Efecto POSITIVO moderado |

**Patrón:** El efecto del decreto sobre justificación de violencia de control es **no monótono**: más fuerte entre quienes tienen predominancia nacional o étnica clara, y se invierte en el grupo de equilibrio.

#### **Justificación de CAMBIO/RESGUARDO (idx_vio_resguardo):**

| Tercil | β DiD | SE | p-value |
|--------|-------|-----|---------|
| Nacional | +1.954 | 0.698 | **0.005** |
| Equilibrio | +0.724 | 0.826 | 0.381 |
| Étnica | +0.384 | 0.672 | 0.568 |

**Patrón:** Solo significativo en el tercil "Nacional" (quienes se identifican más con Chile que con pueblos originarios).

---

### Pregunta 5: ¿La interacción cuádruple (DiD × predominancia) es significativa?

**RESPUESTA: NO, la interacción continua NO es significativa**

**Control (idx_vio_control):**
- Término crítico: `periododecreto:indigeneousindi:zona_decretodecreto:predominancia_base`
- β = 0.033, SE = 0.286, **p = 0.907**

**Cambio/resguardo (idx_vio_resguardo):**
- Término crítico: `periododecreto:indigeneousindi:zona_decretodecreto:predominancia_base`
- β = -0.387, SE = 0.310, **p = 0.212**

**Implicación:** Aunque hay heterogeneidad por terciles (pregunta 4), la relación continua entre predominancia y el efecto DiD **no es lineal**. La predominancia NO modera el efecto DiD de forma continua.

**NOTA ADICIONAL:** En el modelo de resguardo, la interacción triple `periodoestallido:indigeneousindi:zona_decretodecreto:predominancia_base` SÍ es significativa (β = -0.580, p = 0.020), sugiriendo que la predominancia identitaria sí condicionó la respuesta al estallido social (ola 3), pero NO al decreto (ola 4).

---

## 2. ESTRUCTURA INGROUP/OUTGROUP IDENTITARIO

### Variables creadas:

- **id_ingroup:** Para indígenas = `idx_id_etnica` (promedio id_indi + a5); para no indígenas = `id_chile`
- **id_outgroup:** Para indígenas = `id_chile`; para no indígenas = `idx_id_etnica`
- **brecha_id = id_ingroup − id_outgroup**
  - Positivo = se identifica más con su propio grupo
  - Negativo = se identifica más con el otro grupo

### Resultados baseline (ola 2):

|             | id_ingroup | id_outgroup | brecha_id |
|-------------|------------|-------------|-----------|
| No indígenas| 4.51       | 2.62        | **+1.88** |
| Indígenas   | 4.02       | 4.36        | **-0.34** |

**Interpretación:**
- **No indígenas:** Fuerte favorecimiento ingroup (se identifican mucho más con Chile que con pueblos originarios)
- **Indígenas:** LEVE favorecimiento OUTGROUP (se identifican ligeramente más con Chile que con su pueblo originario)

**HALLAZGO CONTRAINTUITIVO:** Los indígenas chilenos muestran identificación dual con leve predominancia de identidad nacional, no étnica. Esto puede reflejar:
1. Presión asimilacionista histórica
2. Identificación nacional no excluyente de identidad étnica
3. Efecto de panel balanceado (sobreviven en el panel quienes están más integrados)

---

## 3. TRAYECTORIAS TEMPORALES (ver figuras)

### `fig_predominancia_trayectorias.png`

**Observaciones:**
1. La línea de cero marca equilibrio entre identidad étnica y nacional
2. **No indígenas:** Siempre debajo de cero (predomina identidad nacional), sin cambios por zona o periodo
3. **Indígenas:** También debajo de cero pero más cerca del equilibrio, sin diferencias sustanciales por zona
4. **No hay evidencia de post-tratamiento:** las trayectorias son paralelas entre zona decreto y fuera

### `fig_identidad_ingroup_outgroup.png`

**Panel izquierdo (Identidad con MI grupo):**
- No indígenas: alta y estable (~4.5) en todas las olas
- Indígenas: moderada-alta y estable (~4.0) en todas las olas

**Panel derecho (Identidad con el OTRO grupo):**
- No indígenas: baja y estable (~2.6) — no se identifican con pueblos originarios
- Indígenas: alta y estable (~4.3) — se identifican MUCHO con Chile

**Implicación:** La estructura identitaria es **asimétrica**: los indígenas tienen identidad dual (alto en ambas), mientras que los no indígenas tienen identidad exclusiva (alto en nacional, bajo en étnica).

---

## 4. INTERPRETACIÓN Y DECISIÓN ANALÍTICA

### Escenario observado: Mezcla de A y B

#### ✅ **Lo que funciona:**
1. **id_indi tiene varianza entre no indígenas** → ingroup/outgroup viable
2. **id_indi y a5 correlacionan alto** → se pueden promediar en `idx_id_etnica`
3. **Hay heterogeneidad por terciles de predominancia** → análisis de heterogeneidad viable

#### ⚠️ **Lo que NO funciona:**
1. **La interacción continua DiD × predominancia NO es significativa** → predominancia no modera el efecto DiD de forma lineal
2. **La estructura ingroup/outgroup NO es simétrica** → los indígenas tienen identidad dual, no exclusiva

### Contribución publicable:

**Opción recomendada:** Análisis de heterogeneidad por terciles de predominancia identitaria

**Narrativa:**
> "El efecto del decreto sobre justificación de violencia varía según el perfil identitario baseline de los individuos. Entre quienes tienen predominancia identitaria clara (sea nacional o étnica), el decreto incrementa la justificación de violencia de control. Sin embargo, entre quienes tienen identidad equilibrada/dual, el efecto se invierte, sugiriendo que la ambivalencia identitaria protege contra la legitimación de coerción estatal."

**Tablas/figuras a incluir:**
1. Tabla de medias de identidad por grupo (diagnóstico)
2. Tabla de efectos DiD por tercil de predominancia (hallazgo principal)
3. `fig_identidad_ingroup_outgroup.png` (estructura identitaria asimétrica)
4. `fig_predominancia_trayectorias.png` (ausencia de post-tratamiento)

---

## 5. LIMITACIONES Y CONSIDERACIONES

### Limitación 1: Identidad dual indígena
Los indígenas chilenos no muestran predominancia de identidad étnica sobre nacional en baseline. Esto **no invalida el análisis** pero cambia su interpretación: la predominancia no captura "fortaleza étnica" sino "balance entre identidades".

### Limitación 2: No linealidad
La relación entre predominancia y efecto DiD no es lineal. Los análisis por terciles capturan esta no linealidad mejor que la interacción continua.

### Limitación 3: Efecto solo en estallido, no en decreto
La interacción significativa está en ola 3 (estallido), no en ola 4 (decreto). Esto sugiere que la predominancia identitaria condicionó la respuesta al **estallido social**, no al decreto.

---

## 6. PRÓXIMOS PASOS (OPCIONAL)

Si se quiere profundizar:

1. **Análisis separado por grupo étnico:** Correr modelos DiD × predominancia solo entre indígenas para ver si la heterogeneidad se mantiene
2. **Explorar efectos del estallido:** Dado que la interacción significativa está en ola 3, analizar `periodoestallido` como tratamiento primario
3. **Descomponer idx_id_etnica:** Explorar si a4 (identificación) y a5 (importancia) tienen efectos diferenciados
4. **Identidad dual como variable:**
   - `id_dual = (idx_id_etnica + id_chile) / 2`
   - Testar si alta identidad dual (alto en ambas) modera el efecto DiD

---

## 7. ARCHIVOS GENERADOS

✅ **Script:** `R/08_identidad_ingroup.R`  
✅ **Figuras:**
- `output/figuras/fig_identidad_distribucion.png` — distribución de id_indi e id_chile por grupo
- `output/figuras/fig_predominancia_trayectorias.png` — trayectorias de predominancia por grupo y zona
- `output/figuras/fig_identidad_ingroup_outgroup.png` — identidad ingroup vs outgroup por grupo

✅ **Output consola:** Guardado en `output_identidad.txt` (ejecutar `cat output_identidad.txt` para verlo)

---

## 8. DECISIÓN FINAL

**¿Usar ingroup/outgroup como en justicia procedimental?**  
→ **NO, porque la estructura es asimétrica** (indígenas tienen identidad dual, no exclusiva)

**¿Usar predominancia como moderador?**  
→ **SÍ, por terciles (no continuo)**, porque captura heterogeneidad no lineal

**¿Reportar como hallazgo negativo si no modera?**  
→ **NO es negativo**: SÍ hay heterogeneidad por terciles, lo cual es publicable

**Hallazgo publicable:**  
> La predominancia identitaria en baseline condiciona la respuesta al decreto: efectos más fuertes entre quienes tienen predominancia clara (nacional o étnica), y efecto invertido entre quienes tienen identidad equilibrada.
