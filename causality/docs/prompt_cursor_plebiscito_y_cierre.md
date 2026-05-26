# Prompt para Cursor — Ajuste final: plebiscito al anexo + cierre del paper

Complementa `prompt_cursor_diagrama_y_estructura.md`. Aplica los últimos ajustes de contenido antes del render final.

## CAMBIO 1 — Plebiscito al anexo

- Eliminar §5.5 del cuerpo (tabla, figura, párrafos).
- En §6.5 (`sec-disc-voto`): una sola frase con OR dinámico + referencia `@sec-apendice-a10`.
- Crear **Apéndice A10** con tabla logística, interpretación (timing, interacción n.s., ORs de actitudes) y `fig_prob_rechazo.png`.
- Renumerar: Modelos A/B/C → tabla complementaria; fórmulas permanecen en A11.

## CAMBIO 2 — Estructura definitiva

Cuerpo: Intro → Marco (2.1–2.5 + fig conceptual) → Datos → Modelo → Resultados (5.1–5.5 robustez) → Discusión (6.1–6.5 nota plebiscito) → Limitaciones → Conclusiones.

Apéndice: A1–A9, **A10 plebiscito**, A11 fórmulas.

## CAMBIO 3 — Conclusiones

Cuatro párrafos: síntesis empírica; contribución teórica (reconfiguración dual, no backlash); implicaciones literatura/política; limitaciones y agenda.

## CAMBIO 4 — Introducción

Párrafo Gerber + Disi Pavlic + tres limitaciones (usar `@gerber2018`, no 2017, salvo alias en bib).

## Reglas

- Sin backlash al describir hallazgos propios.
- Coeficientes vía inline R (`paper$tau4_resg`, `m_rechazo`, etc.).
- Render final y verificar refs `@sec-apendice-a10`, sin `tbl-plebiscito` ni `fig-rechazo` en cuerpo.
