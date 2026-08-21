# Sociedad artificial de justificación de la violencia

Modelo NetLogo 6.4 que operacionaliza el marco de Deneken, Díaz, Gerber y González (2026): dos grupos, dos territorios, dos objetos actitudinales (violencia vs. demandas) y el arco apertura nacional → decreto territorial → desacople.

## Abrir

1. Instalar [NetLogo 6.4+](https://ccl.northwestern.edu/netlogo/).
2. Abrir `justificacion_violencia.nlogo`.
3. `setup` (S) y luego `go` (G), o **ir a ola 4 (2023)** para saltar a t = 60.

## Experimentos que reproducen el paper

| Switches encendidos | Hipótesis | Qué mirar |
|---|---|---|
| solo `fuente-nacional?` | H1 ciclo | Sube la resistencia en los 4 grupos; DiD ≈ 0 |
| solo `fuente-territorial?` | H2 zona | Efecto zona común; DiD étnico ≈ 0 |
| territorial + `diferencial-etnico?` | H3 | DiD resistencia > DiD control |
| + `regularizacion-jp?` | H4 | Sube JP ingroup solo en indígenas de la zona |
| + `ciclo-demandas?` | H5 | Las demandas se mueven; su DiD se queda en 0 |

Un tick = un mes. `t = 0` es enero de 2018; `t = 21` estallido; `t = 45` D.S. 418; `t = 56` Rechazo; `t = 60` ola 4.

Los sliders de shock están calibrados a los coeficientes FE del paper (targets de simulación, no estimaciones).
