# deneken_lcga_cambio — Trayectorias latentes de violencia por cambio social (ELRI)

Análisis LCGA (Growth Mixture Modeling con `lcmm::hlme`) de la **justificación de violencia por cambio social** en el panel ELRI, siguiendo la lógica del script ELSOC de percepción de seguridad del barrio.

## Variable dependiente

| Nombre interno | Concepto | Ítems ELRI | Escala |
|----------------|----------|------------|--------|
| `denek_vio_camb_soc` | Justificación violencia de cambio social | d4_2 + d4_3 (promedio) | 1 = nada justificado … 5 = totalmente justificado |

Equivalente a `idx_vio_resguardo` en el pipeline `causality/`.

## Ventana temporal

| Ola ELRI | Año | Tiempo LCGA (`denek_tiempo`) |
|----------|-----|------------------------------|
| 1 | 2016 | 0 |
| 2 | 2018 | 1 |
| 3 | dic 2020 – may 2021 | 2 |
| 4 | 2023 | 3 |

Panel balanceado: personas con `denek_vio_camb_soc` válido en las **4 olas** (N ≈ 1.535 × 4 = 6.140 obs).

## Resultado preliminar (modelo `denek_lcga3_sq`, 3 clases)

| Clase | % | Perfil |
|-------|---|--------|
| 1 | 16,9% | Rechazo creciente / declive (alta justificación inicial → caída en 2023) |
| 2 | 17,9% | Incremento moderado (baja en 2016 → pico en 2020–2021 → muy alta en 2023) |
| 3 | 65,2% | Trayectoria intermedia (rechazo persistente, media ≈ 1,7) |

Selección por menor BIC entre modelos convergentes (lineales y cuadráticos, 1–4 clases).

## Estructura

```
lcga_deneken_cambio/
├── R/
│   ├── 00_denek_preparar_panel.R   # Construye df_denek_lcga
│   ├── 01_denek_estimar_lcga.R     # Modelos 1–4 clases (lineal + cuadrático)
│   └── 02_denek_graficar_clases.R  # Trayectorias observadas + export
├── data/data_proc/
├── output/figuras/
├── output/tablas/
└── run_denek_lcga.sh
```

## Ejecución

```bash
cd lcga_deneken_cambio
bash run_denek_lcga.sh
```

**Nota:** `01_denek_estimar_lcga.R` puede demorar varios minutos (100 arranques aleatorios × 6 modelos).

## Dependencias

- `lcmm`, `dplyr`, `ggplot2`, `readr`
- Datos fuente: `../causality/data/panel_completo.rds` (ejecutar `causality/run_all.sh` o al menos `R/01_limpieza.R` antes).

## Salidas principales

- `data/data_proc/denek_lcga_resultados.rds` — modelos + asignación de clases
- `data/data_proc/denek_panel_lcga.rds` — panel long con clase latente
- `output/figuras/denek_trayectorias_vio_camb_lcga.png` — medias observadas por clase
- `output/tablas/denek_lcga_criterios_fit.csv` — AIC, BIC, entropía
