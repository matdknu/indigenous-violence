# Estado de excepción y justificación de la violencia en Chile

**Evidencia cuasi-experimental de un panel longitudinal (2018–2023)**

Matías Deneken & Federico Díaz  
Pontificia Universidad Católica de Chile

## 📄 Paper

**[➡️ Leer el paper completo](https://matdknu.github.io/indigenous-violence/)** (HTML interactivo)

## Abstract

Este artículo examina cómo el estado de excepción constitucional de octubre de 2021 en La Araucanía y el Biobío se asocia con cambios diferenciales en la justificación de la violencia entre personas indígenas y no indígenas según su cercanía territorial al conflicto. Con el panel ELRI (N = 1,578; olas 2018–2023) se implementa una estrategia cuasi-experimental exploratoria de diferencias-en-diferencias (triple interacción período × identidad × zona).

### Hallazgos principales

- **Demanda dual paradójica:** Las personas indígenas en zona de excepción aumentan simultáneamente la justificación de violencia de control social (β = 0,76**, p<.01) y de cambio social (β = 0,84**, p<.01) en 2023
- **Mecanismo ingroup/outgroup:** El decreto mejora la percepción de justicia procedimental ingroup entre indígenas (+0,77*, p=.031), lo que media 42% del efecto en violencia de control (Tyler 1990: regularización del trato)
- **Supresión en resguardo:** Para violencia de cambio social, controlar por justicia procedimental amplifica el efecto DiD (−24%), revelando dos procesos contrapuestos

## Estructura del repositorio

```
indigenous-violence/
├── docs/                    ← GitHub Pages (paper HTML)
│   ├── index.html          ← Paper completo
│   ├── figuras/            ← Todas las figuras
│   └── tablas/             ← Todas las tablas
├── causality/              ← Análisis cuantitativo
│   ├── R/                  ← Scripts del pipeline
│   ├── data/               ← Datos procesados
│   ├── output/             ← Figuras y tablas
│   └── paper/              ← Fuentes Quarto (.qmd)
├── lcga/                   ← Análisis de trayectorias
└── README.md               ← Este archivo
```

## Pipeline analítico

```bash
cd causality
Rscript R/01_limpieza.R        # Limpieza + variables ingroup/outgroup
Rscript R/02_descriptivos.R    # Estadísticos descriptivos + figuras
Rscript R/03_modelos.R         # Modelos DiD principales
Rscript R/04_robustez.R        # Robustez (placebo, IPW, PSM)
Rscript R/05_mecanismo.R       # Mediación justicia procedimental
cd paper && quarto render paper.qmd --to html
```

## Datos

Panel ELRI (Encuesta Longitudinal de Relaciones Interculturales):
- 4 olas (2016, 2018, 2020-2021, 2023)
- Panel balanceado: 1,578 individuos
- Diseño espejo: indígenas / no indígenas
- 53 comunas en zona de excepción (D.S. N°418/2021)

## Citar

```bibtex
@unpublished{deneken2026estado,
  author = {Deneken, Matías and Díaz, Federico},
  title = {Estado de excepción, identidad étnica y justificación de la violencia: 
           evidencia cuasi-experimental de un panel longitudinal en Chile (2018--2023)},
  year = {2026},
  note = {Working paper},
  url = {https://matdknu.github.io/indigenous-violence/}
}
```

## Contacto

- Matías Deneken: [email]
- Federico Díaz: [email]

## Licencia

- Código: MIT License
- Paper: CC BY 4.0
