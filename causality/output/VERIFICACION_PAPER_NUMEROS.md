# Verificación de consistencia numérica — paper.qmd

**Fecha:** 2026-05-25  
**Status:** ✅ Números verificados contra RDS del pipeline

---

## N panel

| Fuente | Valor |
|--------|-------|
| `subset_data` obs totales | 4.742 (1.580 × 3 olas) |
| Individuos únicos (`paper$n_panel`) | **1.580** |
| Texto anterior (incorrecto) | 1.592 / 1.578 |

---

## Modelo C — Efectos DiD principales (τ₄)

| VD | β | p | Prompt | ✓ |
|----|---|---|--------|---|
| Control social | **0,762** | 0,006 | 0,76** | ✅ |
| Cambio social | **0,837** | 0,004 | 0,84** | ✅ |

---

## Modelo C — Efectos de período y estallido

| Término | VD | β | p | Prompt | ✓ |
|---------|-----|---|---|--------|---|
| periodoestallido | Cambio | **0,283** | <0,001 | +0,283*** | ✅ |
| periododecreto | Control | **0,301** | <0,001 | +0,301*** | ✅ |
| periododecreto | Cambio | **0,023** | n.s. | (vs baseline ola 2) | ✅ |
| τ₃ estallido | Control | **0,152** | n.s. | 0,152 n.s. | ✅ |
| τ₃ estallido | Cambio | **−0,076** | n.s. | −0,076 n.s. | ✅ |
| ola3 × zona | Cambio | **0,464** | 0,025 | +0,464* | ✅ |
| cerca_conflicto (baseline) | Cambio | **−0,372** | 0,025 | −0,372* | ✅ |

---

## Modelo B — Transición ola 3→4 (T2_decreto)

| VD | β general T2 | p | Prompt | ✓ |
|----|--------------|---|--------|---|
| Cambio social | **−0,246** | 0,004 | β=−0,246** | ✅ |
| Control social | **+0,258** | 0,001 | (sube en ola 4) | ✅ |

**Nota interpretativa:** La caída de cambio social en ola 4 se identifica con Modelo B (ref. ola 3), no con el efecto de período del Modelo C (ref. ola 2 = 0,023 n.s.).

---

## Mecanismo — Paso 1 (DiD sobre mediadores)

| Mediador | β | p | Prompt | ✓ |
|----------|---|---|--------|---|
| just_proc_ingroup | **+0,767** | 0,031 | +0,77* | ✅ |
| just_proc_outgroup | **+0,246** | 0,488 | +0,246 n.s. | ✅ |
| brecha_just_proc | **−0,548** | 0,082 | −0,548 p=.082 | ✅ |

---

## Mecanismo — Paso 3 (atenuación DiD)

| VD | β sin | p sin | β con ingroup | Atenuación | Prompt | ✓ |
|----|-------|-------|---------------|------------|--------|---|
| Control | 0,372 | 0,064 | 0,214 | **+42,4%** | +42% | ✅ |
| Cambio | 0,421 | 0,056 | 0,523 | **−24,3%** | −24% supresión | ✅ |

---

## Baseline justicia procedimental (ola 2)

| Grupo | ingroup | outgroup | brecha (out−in) | Prompt | ✓ |
|-------|---------|----------|-----------------|--------|---|
| Indígenas | 2,74 | 3,13 | **+0,40** | +0,40 | ✅ |
| No indígenas | 3,04 | 2,72 | **−0,32** | −0,32 | ✅ |

---

## Identidad — Predominancia baseline (ola 2)

| Grupo | brecha_id (in−out) | Prompt | ✓ |
|-------|---------------------|--------|---|
| Indígenas | **−0,34** | −0,34 | ✅ |
| No indígenas | **+1,88** | +1,88 | ✅ |
| r(a4, a5) | **0,821** | r=.82 | ✅ |

---

## Heterogeneidad — DiD por tercil (Modelo C)

### Control social
| Tercil | β | p | ✓ |
|--------|---|---|---|
| Nacional | +2,352 | <0,001 | ✅ |
| Equilibrio | −2,041 | 0,010 | ✅ |
| Étnica | +1,555 | 0,007 | ✅ |

### Cambio social
| Tercil | β | p | ✓ |
|--------|---|---|---|
| Nacional | +1,954 | 0,005 | ✅ |
| Equilibrio | +0,724 | n.s. | ✅ |
| Étnica | +0,384 | n.s. | ✅ |

### Interacción cuádruple continua (decreto)
| VD | β | p | ✓ |
|----|---|---|---|
| Control | +0,033 | 0,907 | ✅ |
| Cambio | −0,387 | 0,212 | ✅ |

### Interacción estallido (ola 3, cambio)
| β | p | ✓ |
|---|---|---|
| −0,580 | 0,020 | ✅ |

---

## Controles sustantivos (Modelo C)

| Variable | Control social | Cambio social | Prompt | ✓ |
|----------|----------------|---------------|--------|---|
| id_causa | **−0,138*** | **+0,271*** | espejo | ✅ |
| id_chile | **−0,050*** | **−0,120*** | espejo | ✅ |
| perc_desigualdad | **−0,127*** | n.s. | −0,127*** control | ✅ |
| perc_injusticia | **−0,120*** | n.s. | −0,120*** control | ✅ |

---

## Codificación ordinal A (sensibilidad)

| VD | β DiD ordinal A | p | Prompt | ✓ |
|----|-----------------|---|--------|---|
| Cambio social | **1,869** | 0,009 | 1,87** | ✅ |
| Control social | **1,136** | 0,055 | 1,14+ | ✅ |

---

## Correcciones aplicadas al paper.qmd

1. ✅ Título y años: 2018–2023 (no 2016–2022)
2. ✅ N = 1.580 vía `paper$n_panel` (no 1.592)
3. ✅ Comunas: 53 en 4 provincias (eliminado Los Lagos)
4. ✅ Ola 3 = resabio estallido, no "estado de excepción activo"
5. ✅ Narrativa apertura → contención fallida → demanda dual
6. ✅ Mecanismo: regularización (no deterioro) en discusión
7. ✅ Sección heterogeneidad identitaria (§5.5)
8. ✅ Números dinámicos vía `paper$` en prosa y tablas
9. ✅ `paper_results.R` ampliado con brechas, período, mecanismo, hetero
10. ✅ Apéndice A6 actualizado (ingroup/outgroup) + A8 heterogeneidad

---

## Archivos modificados

- `paper/paper.qmd` — reescritura prosa completa
- `R/paper_results.R` — objetos adicionales para inline
- `R/08_identidad_ingroup.R` — guarda `data/hetero_identidad.rds`
- `output/VERIFICACION_PAPER_NUMEROS.md` — este documento

---

**Reproducir:** ejecutar pipeline 01–08, luego renderizar `paper/paper.qmd`.
