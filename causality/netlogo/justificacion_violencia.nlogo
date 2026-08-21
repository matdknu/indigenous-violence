;; =============================================================================
;; Sociedad artificial de justificación de la violencia
;; -----------------------------------------------------------------------------
;; Modelo basado en:
;;   Deneken, Díaz, Gerber y González (2026).
;;   "Apertura normativa y contención fallida: estallido social, estado de
;;    excepción y justificación de la violencia entre personas indígenas y
;;    no indígenas en Chile (2018–2023)"
;;
;; El modelo operacionaliza cinco hipótesis del paper:
;;   H1  Fuente nacional (ciclo): el estallido abre la justificación de la
;;       protesta a escala nacional, sin diferencial territorial.
;;   H2  Fuente territorial: el decreto endurece la justificación en la zona.
;;   H3  Diferencial étnico: el plus es mayor entre indígenas de la zona,
;;       y más robusto para resistencia que para coerción estatal.
;;   H4  Regularización procedimental: la coerción institucionalizada mejora
;;       la justicia procedimental ingroup (predictibilidad, no benevolencia).
;;   H5  Desacople: las demandas siguen el ciclo político nacional, no la zona.
;;
;; 1 tick = 1 mes. t = 0 es enero de 2018.
;; =============================================================================

breed [ciudadanos ciudadano]

ciudadanos-own [
  indigenous?              ;; pertenencia étnica (fija, como en ELRI ola 1)
  in-zone?                 ;; reside en comuna del D.S. 418
  justif-control           ;; justificación de coerción estatal (d3_1), 1–5
  justif-resistencia       ;; justificación de protesta disruptiva (d4_3), 1–5
  jp-ingroup               ;; justicia procedimental hacia el propio grupo, 1–5
  jp-outgroup              ;; justicia procedimental hacia el otro grupo, 1–5
  apoyo-demandas           ;; apoyo a demandas indígenas (índice), ~z
  id-chile                 ;; identificación con Chile, 1–5
  id-causa                 ;; identificación con la causa indígena, 1–5
  perc-desigualdad         ;; percepción de desigualdad, 1–5
]

patches-own [
  zona-excepcion?          ;; patch en la Macrozona Sur (tratamiento territorial)
  militarizado?            ;; presencia coercitiva institucionalizada (post-decreto)
  clima-protesta           ;; intensidad local del ciclo de protesta
]

globals [
  periodo                  ;; "baseline" | "estallido" | "decreto"
  mes-anio                 ;; etiqueta calendario
  decreto-activo?          ;; D.S. 418 vigente
  cierre-constituyente?    ;; post-Rechazo (sep 2022)

  ;; medias de línea base (ola 2) para contrastes DiD
  base-r-indi-zona
  base-r-indi-fuera
  base-r-noindi-zona
  base-r-noindi-fuera
  base-c-indi-zona
  base-c-indi-fuera
  base-c-noindi-zona
  base-c-noindi-fuera
  base-jp-indi-zona
  base-d-indi-zona
  base-d-indi-fuera
  base-d-noindi-zona
  base-d-noindi-fuera

  baseline-capturada?
]


;; -----------------------------------------------------------------------------
;; SETUP
;; -----------------------------------------------------------------------------

to setup
  clear-all
  setup-territorio
  setup-ciudadanos
  setup-linea-base
  set periodo "baseline"
  set mes-anio "2018-01"
  set decreto-activo? false
  set cierre-constituyente? false
  set baseline-capturada? false
  reset-ticks
  snapshot-baseline
  actualizar-colores
end

to setup-territorio
  ;; La zona de excepción ocupa el cuadrante sureste (~23% de patches),
  ;; análogo a las 53 comunas del D.S. 418 (Cautín, Malleco, Arauco, Biobío).
  ask patches [
    set zona-excepcion? (pxcor >= umbral-zona-x) and (pycor <= umbral-zona-y)
    set militarizado? false
    set clima-protesta 0
    ifelse zona-excepcion?
    [ set pcolor 58 ]   ;; verde-gris (sur)
    [ set pcolor 8 ]    ;; gris claro (resto del país)
  ]
end

to setup-ciudadanos
  create-ciudadanos n-ciudadanos [
    setxy random-xcor random-ycor
    set in-zone? [zona-excepcion?] of patch-here
    ;; Proporción indígena ~53% (ELRI espejo: 845/1580). Independiente de zona
    ;; (en el paper: 22% de indígenas y 23% de no indígenas viven en la zona).
    set indigenous? (random-float 1 < prop-indigena)
    inicializar-actitudes
    set size 0.85
    ifelse indigenous?
    [ set shape "circle" ]
    [ set shape "square" ]
  ]
end

to inicializar-actitudes
  ;; Medias de Tabla 3 (ola 2 / baseline) + ruido. Escala Likert 1–5.
  ifelse indigenous? [
    set justif-control     acotar (random-normal 2.14 1.10) 1 5
    set justif-resistencia acotar (random-normal 1.78 1.05) 1 5
    set id-chile           acotar (random-normal 4.36 0.83) 1 5
    set id-causa           acotar (random-normal 3.72 1.04) 1 5
    set perc-desigualdad   acotar (random-normal 3.13 0.73) 1 5
    ;; Agravio percibido: mejor trato al outgroup que al ingroup (brecha ≈ 0.40)
    set jp-ingroup         acotar (random-normal 2.55 0.85) 1 5
    set jp-outgroup        acotar (random-normal 2.95 0.85) 1 5
    set apoyo-demandas     random-normal  0.25 0.80
  ] [
    set justif-control     acotar (random-normal 2.30 1.15) 1 5
    set justif-resistencia acotar (random-normal 1.54 0.90) 1 5
    set id-chile           acotar (random-normal 4.51 0.82) 1 5
    set id-causa           acotar (random-normal 3.08 1.20) 1 5
    set perc-desigualdad   acotar (random-normal 3.04 0.78) 1 5
    ;; Privilegio percibido: mejor trato al ingroup (brecha ≈ −0.32)
    set jp-ingroup         acotar (random-normal 3.20 0.85) 1 5
    set jp-outgroup        acotar (random-normal 2.88 0.85) 1 5
    set apoyo-demandas     random-normal -0.25 0.80
  ]
  ;; En baseline, indígenas en zona justifican MENOS la resistencia
  ;; (β = −0.425, p = .016). Patrón opuesto al post-tratamiento.
  if indigenous? and in-zone? [
    set justif-resistencia acotar (justif-resistencia - 0.25) 1 5
  ]
end

to setup-linea-base
  set base-r-indi-zona 0
  set base-r-indi-fuera 0
  set base-r-noindi-zona 0
  set base-r-noindi-fuera 0
  set base-c-indi-zona 0
  set base-c-indi-fuera 0
  set base-c-noindi-zona 0
  set base-c-noindi-fuera 0
  set base-jp-indi-zona 0
  set base-d-indi-zona 0
  set base-d-indi-fuera 0
  set base-d-noindi-zona 0
  set base-d-noindi-fuera 0
end


;; -----------------------------------------------------------------------------
;; GO
;; -----------------------------------------------------------------------------

to go
  actualizar-periodo
  actualizar-territorio
  ask ciudadanos [
    aplicar-estructura-identitaria
    aplicar-fuente-nacional
    aplicar-fuente-territorial
    aplicar-regularizacion-jp
    aplicar-ciclo-demandas
    influir-socialmente
    acotar-actitudes
  ]
  actualizar-colores
  tick
end


;; -----------------------------------------------------------------------------
;; CALENDARIO Y SHOCKS MACRO
;; -----------------------------------------------------------------------------

to actualizar-periodo
  ;; t = 0 → enero 2018
  ;; t = 21 → octubre 2019 (estallido)
  ;; t = 36 → enero 2021 (campo ola 3: dic 2020–may 2021)
  ;; t = 45 → octubre 2021 (D.S. 418)
  ;; t = 56 → septiembre 2022 (Rechazo)
  ;; t = 60 → enero 2023 (campo ola 4)
  let anio 2018 + floor (ticks / 12)
  let mes 1 + (ticks mod 12)
  set mes-anio (word anio "-" (ifelse-value (mes < 10) [word "0" mes] [mes]))

  ifelse ticks < 21 [
    set periodo "baseline"
  ] [
    ifelse ticks < 45 [
      set periodo "estallido"
    ] [
      set periodo "decreto"
    ]
  ]
  set decreto-activo? (ticks >= 45)
  set cierre-constituyente? (ticks >= 56)
end

to actualizar-territorio
  ask patches [
    ;; El estallido es nacional: clima de protesta en todo el país.
    ifelse periodo = "estallido" [
      set clima-protesta 0.85 + 0.15 * random-float 1
    ] [
      set clima-protesta 0.15 * random-float 1
    ]
    ;; El decreto militariza solo la zona (coerción institucionalizada).
    set militarizado? (zona-excepcion? and decreto-activo?)
    ifelse militarizado? [
      set pcolor 43   ;; ocre: presencia militar
    ] [
      ifelse zona-excepcion?
      [ set pcolor 58 ]
      [ set pcolor 8 ]
    ]
  ]
end


;; -----------------------------------------------------------------------------
;; MECANISMOS (H1–H5)
;; -----------------------------------------------------------------------------

to aplicar-estructura-identitaria
  ;; Regularidad de base (Tajfel & Turner 1979; Reicher 2004): no es el
  ;; contraste causal, pero enmarca por qué una misma intervención puede
  ;; afectar distinto a cada grupo. Paso pequeño por tick.
  let delta-id (id-causa - id-chile) / 4
  set justif-resistencia justif-resistencia + 0.004 * delta-id * peso-identidad
  set justif-control     justif-control     - 0.003 * delta-id * peso-identidad
end

to aplicar-fuente-nacional
  ;; H1. Apertura normativa del estallido: efecto de período, transversal.
  ;; Firma empírica: coeficiente de período > 0; triple interacción ≈ 0.
  if not fuente-nacional? [ stop ]
  if periodo = "estallido" [
    ;; +0.311 en resistencia a escala nacional (FE, ola 3). Se reparte
    ;; en ~24 meses para producir una trayectoria, no un salto.
    set justif-resistencia justif-resistencia + (shock-estallido-nacional / 24)
    ;; La represión reactiva del estallido es nacional y no abre el control
    ;; estatal (coef. ola 3 de control ≈ −0.11, ns).
    set justif-control justif-control + (shock-estallido-control / 24)
    ;; El proceso constituyente eleva las demandas (ciclo nacional).
    set apoyo-demandas apoyo-demandas + (shock-demandas-auge / 24)
  ]
end

to aplicar-fuente-territorial
  ;; H2 + H3. Coerción institucionalizada (D.S. 418): efecto de zona,
  ;; común a ambos grupos, más un plus étnico asimétrico.
  if not fuente-territorial? [ stop ]
  if not in-zone? [ stop ]
  if not decreto-activo? [ stop ]

  ;; Efecto de zona común (H2). FE: zona×ola4 = 0.903 (resistencia),
  ;; 1.193 (control). Se reparte en ~24 meses post-decreto.
  set justif-resistencia justif-resistencia + (shock-decreto-zona / 24)
  set justif-control     justif-control     + (shock-decreto-zona-control / 24)

  ;; Diferencial étnico (H3): robusto en resistencia (0.413**),
  ;; frágil en coerción estatal (0.313+).
  if diferencial-etnico? and indigenous? [
    set justif-resistencia justif-resistencia + (shock-decreto-triple / 24)
    set justif-control     justif-control     + (shock-decreto-triple-control / 24)
  ]
end

to aplicar-regularizacion-jp
  ;; H4. La coerción reglada (protocolos, predictibilidad) mejora la JP
  ;; ingroup de indígenas en zona (β = 0.602). Contrasta con la represión
  ;; reactiva del estallido, que no regulariza.
  if not regularizacion-jp? [ stop ]
  if decreto-activo? and indigenous? and in-zone? [
    set jp-ingroup jp-ingroup + (shock-jp-regularizacion / 24)
    ;; La brecha se cierra porque sube el ingroup, no porque baje el outgroup
    ;; (efecto outgroup ns en el paper).
  ]
  ;; Durante el estallido, la represión reactiva erosiona levemente la JP
  ;; (patrón de Disi Pavlic et al. 2025 / Curtice 2021), sin diferencial de zona.
  if periodo = "estallido" [
    set jp-ingroup jp-ingroup - 0.004
  ]
end

to aplicar-ciclo-demandas
  ;; H5. Las demandas no tienen firma territorial. Suben con el proceso
  ;; constituyente y caen tras el Rechazo, en paralelo para los cuatro grupos.
  if not ciclo-demandas? [ stop ]
  if cierre-constituyente? [
    set apoyo-demandas apoyo-demandas - (shock-demandas-caida / 18)
  ]
end

to influir-socialmente
  ;; Contagio local con sesgo endogrupal (homofilia identitaria).
  ;; No genera por sí solo el DiD: solo suaviza y clusteriza.
  if tasa-influencia <= 0 [ stop ]
  let vecinos ciudadanos-on neighbors
  if not any? vecinos [ stop ]
  let endo vecinos with [indigenous? = [indigenous?] of myself]
  let exo  vecinos with [indigenous? != [indigenous?] of myself]
  let w peso-endogrupo
  if any? endo and any? exo [
    set justif-resistencia justif-resistencia + tasa-influencia * (
      w * mean [justif-resistencia] of endo + (1 - w) * mean [justif-resistencia] of exo
      - justif-resistencia)
    set justif-control justif-control + tasa-influencia * (
      w * mean [justif-control] of endo + (1 - w) * mean [justif-control] of exo
      - justif-control)
    set apoyo-demandas apoyo-demandas + tasa-influencia * (
      w * mean [apoyo-demandas] of endo + (1 - w) * mean [apoyo-demandas] of exo
      - apoyo-demandas)
  ]
  if any? endo and not any? exo [
    set justif-resistencia justif-resistencia + tasa-influencia * (
      mean [justif-resistencia] of endo - justif-resistencia)
    set justif-control justif-control + tasa-influencia * (
      mean [justif-control] of endo - justif-control)
  ]
end

to acotar-actitudes
  set justif-control     acotar justif-control 1 5
  set justif-resistencia acotar justif-resistencia 1 5
  set jp-ingroup         acotar jp-ingroup 1 5
  set jp-outgroup        acotar jp-outgroup 1 5
  set id-chile           acotar id-chile 1 5
  set id-causa           acotar id-causa 1 5
  set perc-desigualdad   acotar perc-desigualdad 1 5
  set apoyo-demandas     acotar apoyo-demandas -3 3
end


;; -----------------------------------------------------------------------------
;; VISUALIZACIÓN
;; -----------------------------------------------------------------------------

to actualizar-colores
  ask ciudadanos [
    let v 0
    if variable-color = "resistencia"      [ set v justif-resistencia ]
    if variable-color = "control estatal"  [ set v justif-control ]
    if variable-color = "JP ingroup"       [ set v jp-ingroup ]
    if variable-color = "demandas"         [ set v (apoyo-demandas + 3) * (5 / 6) ]
    ;; escala 1 (amarillo) → 5 (rojo oscuro)
    set color scale-color red v 1 5
  ]
end


;; -----------------------------------------------------------------------------
;; MEDICIÓN (análogo a las olas ELRI)
;; -----------------------------------------------------------------------------

to snapshot-baseline
  if not any? ciudadanos [ stop ]
  set base-r-indi-zona    media-r true  true
  set base-r-indi-fuera   media-r true  false
  set base-r-noindi-zona  media-r false true
  set base-r-noindi-fuera media-r false false
  set base-c-indi-zona    media-c true  true
  set base-c-indi-fuera   media-c true  false
  set base-c-noindi-zona  media-c false true
  set base-c-noindi-fuera media-c false false
  set base-jp-indi-zona   media-jp true true
  set base-d-indi-zona    media-d true  true
  set base-d-indi-fuera   media-d true  false
  set base-d-noindi-zona  media-d false true
  set base-d-noindi-fuera media-d false false
  set baseline-capturada? true
end

to-report media-r [indi zona]
  let g ciudadanos with [indigenous? = indi and in-zone? = zona]
  ifelse any? g [ report mean [justif-resistencia] of g ] [ report 0 ]
end

to-report media-c [indi zona]
  let g ciudadanos with [indigenous? = indi and in-zone? = zona]
  ifelse any? g [ report mean [justif-control] of g ] [ report 0 ]
end

to-report media-jp [indi zona]
  let g ciudadanos with [indigenous? = indi and in-zone? = zona]
  ifelse any? g [ report mean [jp-ingroup] of g ] [ report 0 ]
end

to-report media-d [indi zona]
  let g ciudadanos with [indigenous? = indi and in-zone? = zona]
  ifelse any? g [ report mean [apoyo-demandas] of g ] [ report 0 ]
end

to-report delta [actual base]
  report actual - base
end

to-report did-resistencia
  ;; Triple diferencia respecto de la línea base (ola 2):
  ;; (Δ indi-zona − Δ indi-fuera) − (Δ noindi-zona − Δ noindi-fuera)
  if not baseline-capturada? [ report 0 ]
  report (delta media-r-indi-zona  base-r-indi-zona)  - (delta media-r-indi-fuera  base-r-indi-fuera)
       - (delta media-r-noindi-zona base-r-noindi-zona) + (delta media-r-noindi-fuera base-r-noindi-fuera)
end

to-report did-control
  if not baseline-capturada? [ report 0 ]
  report (delta media-c-indi-zona  base-c-indi-zona)  - (delta media-c-indi-fuera  base-c-indi-fuera)
       - (delta media-c-noindi-zona base-c-noindi-zona) + (delta media-c-noindi-fuera base-c-noindi-fuera)
end

to-report did-demandas
  if not baseline-capturada? [ report 0 ]
  report (delta media-d-indi-zona  base-d-indi-zona)  - (delta media-d-indi-fuera  base-d-indi-fuera)
       - (delta media-d-noindi-zona base-d-noindi-zona) + (delta media-d-noindi-fuera base-d-noindi-fuera)
end

to-report efecto-zona-resistencia
  ;; Efecto territorial común: promedio de Δ zona − Δ fuera, ambos grupos.
  if not baseline-capturada? [ report 0 ]
  let d-indi   (delta media-r-indi-zona   base-r-indi-zona)   - (delta media-r-indi-fuera   base-r-indi-fuera)
  let d-noindi (delta media-r-noindi-zona base-r-noindi-zona) - (delta media-r-noindi-fuera base-r-noindi-fuera)
  report (d-indi + d-noindi) / 2
end

to-report media-r-indi-zona
  report media-r true true
end
to-report media-r-indi-fuera
  report media-r true false
end
to-report media-r-noindi-zona
  report media-r false true
end
to-report media-r-noindi-fuera
  report media-r false false
end
to-report media-c-indi-zona
  report media-c true true
end
to-report media-c-indi-fuera
  report media-c true false
end
to-report media-c-noindi-zona
  report media-c false true
end
to-report media-c-noindi-fuera
  report media-c false false
end
to-report media-jp-indi-zona
  report media-jp true true
end
to-report media-d-todas
  ifelse any? ciudadanos [ report mean [apoyo-demandas] of ciudadanos ] [ report 0 ]
end

to-report prop-justifica-resistencia
  ;; Análogo a Figura 2: proporción que responde 3–5.
  ifelse any? ciudadanos [
    report count ciudadanos with [justif-resistencia >= 3] / count ciudadanos
  ] [
    report 0
  ]
end


;; -----------------------------------------------------------------------------
;; UTILIDADES
;; -----------------------------------------------------------------------------

to-report acotar [x lo hi]
  report max (list lo (min (list hi x)))
end

to exportar-olas
  ;; Escribe un CSV con las cuatro celdas (identidad × zona) en t actual.
  let fname (word "ola_" mes-anio ".csv")
  file-open fname
  file-print "grupo,n,resistencia,control,jp_ingroup,demandas"
  foreach [["indi-zona" true true] ["indi-fuera" true false] ["noindi-zona" false true] ["noindi-fuera" false false]] [ fila ->
    let lab item 0 fila
    let indi item 1 fila
    let zona item 2 fila
    let g ciudadanos with [indigenous? = indi and in-zone? = zona]
    if any? g [
      file-print (word lab "," count g ","
        precision mean [justif-resistencia] of g 3 ","
        precision mean [justif-control] of g 3 ","
        precision mean [jp-ingroup] of g 3 ","
        precision mean [apoyo-demandas] of g 3)
    ]
  ]
  file-close
end
@#$#@#$#@
GRAPHICS-WINDOW
255
10
768
524
-1
-1
15.242424242424242
1
10
1
1
1
0
0
0
1
-16
16
-16
16
1
1
1
ticks
30.0

BUTTON
15
15
85
48
setup
setup
NIL
1
T
OBSERVER
NIL
S
NIL
NIL
1

BUTTON
90
15
160
48
go
go
T
1
T
OBSERVER
NIL
G
NIL
NIL
0

BUTTON
165
15
235
48
1 mes
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

MONITOR
15
58
120
103
período
periodo
17
1
11

MONITOR
125
58
235
103
calendario
mes-anio
17
1
11

SLIDER
15
115
235
148
n-ciudadanos
n-ciudadanos
100
1200
600.0
50
1
NIL
HORIZONTAL

SLIDER
15
150
235
183
prop-indigena
prop-indigena
0.2
0.8
0.53
0.01
1
NIL
HORIZONTAL

SLIDER
15
185
235
218
umbral-zona-x
umbral-zona-x
-5
12
4.0
1
1
NIL
HORIZONTAL

SLIDER
15
220
235
253
umbral-zona-y
umbral-zona-y
-12
8
2.0
1
1
NIL
HORIZONTAL

TEXTBOX
15
265
240
283
Fuentes de cambio (H1–H5)
13
0.0
1

SWITCH
15
285
235
318
fuente-nacional?
fuente-nacional?
0
1
-1000

SWITCH
15
320
235
353
fuente-territorial?
fuente-territorial?
0
1
-1000

SWITCH
15
355
235
388
diferencial-etnico?
diferencial-etnico?
0
1
-1000

SWITCH
15
390
235
423
regularizacion-jp?
regularizacion-jp?
0
1
-1000

SWITCH
15
425
235
458
ciclo-demandas?
ciclo-demandas?
0
1
-1000

TEXTBOX
15
470
240
488
Calibración (magnitud de shocks)
12
0.0
1

SLIDER
15
490
235
523
shock-estallido-nacional
shock-estallido-nacional
0
1
0.31
0.01
1
NIL
HORIZONTAL

SLIDER
15
525
235
558
shock-estallido-control
shock-estallido-control
-0.4
0.4
-0.05
0.01
1
NIL
HORIZONTAL

SLIDER
15
560
235
593
shock-decreto-zona
shock-decreto-zona
0
2
0.9
0.05
1
NIL
HORIZONTAL

SLIDER
15
595
235
628
shock-decreto-zona-control
shock-decreto-zona-control
0
2
1.19
0.05
1
NIL
HORIZONTAL

SLIDER
15
630
235
663
shock-decreto-triple
shock-decreto-triple
0
1
0.41
0.01
1
NIL
HORIZONTAL

SLIDER
15
665
235
698
shock-decreto-triple-control
shock-decreto-triple-control
0
1
0.31
0.01
1
NIL
HORIZONTAL

SLIDER
15
700
235
733
shock-jp-regularizacion
shock-jp-regularizacion
0
1.2
0.6
0.05
1
NIL
HORIZONTAL

SLIDER
15
735
235
768
shock-demandas-auge
shock-demandas-auge
0
1
0.35
0.05
1
NIL
HORIZONTAL

SLIDER
15
770
235
803
shock-demandas-caida
shock-demandas-caida
0
1
0.4
0.05
1
NIL
HORIZONTAL

TEXTBOX
15
810
240
828
Influencia social
12
0.0
1

SLIDER
15
830
235
863
tasa-influencia
tasa-influencia
0
0.3
0.08
0.01
1
NIL
HORIZONTAL

SLIDER
15
865
235
898
peso-endogrupo
peso-endogrupo
0.5
1
0.75
0.05
1
NIL
HORIZONTAL

SLIDER
15
900
235
933
peso-identidad
peso-identidad
0
2
1.0
0.1
1
NIL
HORIZONTAL

CHOOSER
780
10
980
55
variable-color
variable-color
"resistencia" "control estatal" "JP ingroup" "demandas"
0

MONITOR
780
65
900
110
DiD resistencia
precision did-resistencia 3
17
1
11

MONITOR
905
65
1025
110
DiD control
precision did-control 3
17
1
11

MONITOR
1030
65
1150
110
DiD demandas
precision did-demandas 3
17
1
11

MONITOR
780
115
900
160
efecto zona (R)
precision efecto-zona-resistencia 3
17
1
11

MONITOR
905
115
1025
160
R indi-zona
precision media-r-indi-zona 2
17
1
11

MONITOR
1030
115
1150
160
R noindi-fuera
precision media-r-noindi-fuera 2
17
1
11

PLOT
780
170
1150
360
Justificación de la resistencia (d4_3)
meses desde 2018
media 1–5
0.0
72.0
1.0
4.0
true
true
"" ""
PENS
"indi-zona" 1.0 0 -2674135 true "" "plot media-r-indi-zona"
"indi-fuera" 1.0 0 -955883 true "" "plot media-r-indi-fuera"
"noindi-zona" 1.0 0 -13345367 true "" "plot media-r-noindi-zona"
"noindi-fuera" 1.0 0 -7500403 true "" "plot media-r-noindi-fuera"

PLOT
780
365
1150
555
Justificación del control estatal (d3_1)
meses desde 2018
media 1–5
0.0
72.0
1.0
4.5
true
true
"" ""
PENS
"indi-zona" 1.0 0 -2674135 true "" "plot media-c-indi-zona"
"indi-fuera" 1.0 0 -955883 true "" "plot media-c-indi-fuera"
"noindi-zona" 1.0 0 -13345367 true "" "plot media-c-noindi-zona"
"noindi-fuera" 1.0 0 -7500403 true "" "plot media-c-noindi-fuera"

PLOT
780
560
1150
750
Justicia procedimental ingroup y demandas
meses desde 2018
media
0.0
72.0
1.0
5.0
true
true
"" ""
PENS
"JP indi-zona" 1.0 0 -2674135 true "" "plot media-jp-indi-zona"
"demandas (todas)" 1.0 0 -10899396 true "" "plot (media-d-todas + 3)"

TEXTBOX
780
755
1150
820
Círculos = indígenas; cuadrados = no indígenas.\nColor = intensidad de la variable elegida (amarillo → rojo).\nPatches ocre = zona militarizada (post D.S. 418).\nLíneas verticales implícitas: t=21 estallido, t=45 decreto, t=56 Rechazo.
11
0.0
1

BUTTON
780
825
940
858
exportar ola actual
exportar-olas
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
945
825
1150
858
ir a ola 4 (2023)
while [ticks < 60] [ go ]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

@#$#@#$#@
## QUÉ ES ESTE MODELO

Sociedad artificial que traduce el marco causal del paper de Deneken, Díaz, Gerber y González (2026) a un sistema de agentes. No sustituye al DiD del panel ELRI: es un laboratorio para ver *cómo* pueden emerger las cinco firmas empíricas si los mecanismos propuestos operan.

Dos grupos (indígenas / no indígenas), dos territorios (zona del D.S. 418 / resto del país) y dos objetos actitudinales que el paper separa: justificación de la violencia versus apoyo a las demandas indígenas.

## EL ARCO DE TRES MOMENTOS

El tiempo es mensual. `t = 0` es enero de 2018.

1. **Baseline (2018, t < 21).** Actitudes inicializadas con las medias de la Tabla 3. Los indígenas en zona justifican *menos* la resistencia que los de fuera (patrón pretratamiento del paper).
2. **Estallido (oct 2019–sep 2021, 21 ≤ t < 45).** Apertura normativa nacional (H1). Sube la justificación de la protesta en los cuatro grupos. La triple interacción permanece cerca de cero.
3. **Decreto (desde oct 2021, t ≥ 45).** Coerción institucionalizada en la zona (H2), plus étnico en resistencia (H3), mejora de la JP ingroup por regularización (H4). Desde sep 2022 (`t = 56`) las demandas caen con el Rechazo, en paralelo para todos (H5).

## CÓMO USARLO

1. Abrir el archivo en NetLogo 6.4 o superior.
2. `setup` (o tecla S).
3. `go` (o tecla G). Dejar correr hasta t ≈ 60 (enero 2023, ola 4).
4. Observar los monitores **DiD resistencia**, **DiD control** y **DiD demandas**.

Experimento mínimo para reproducir el paper:

- Todos los switches ON → arco completo.
- Solo `fuente-nacional?` ON → H1: sube la resistencia en todos, DiD ≈ 0.
- Solo `fuente-territorial?` ON → H2: efecto de zona, sin plus étnico.
- Territorial + `diferencial-etnico?` → H3: DiD resistencia > DiD control.
- + `regularizacion-jp?` → H4: sube JP ingroup solo en indígenas de la zona.
- `ciclo-demandas?` ON y territorial ON → H5: las demandas se mueven, el DiD de demandas se queda en cero.

El botón **ir a ola 4 (2023)** avanza hasta t = 60. **exportar ola actual** escribe un CSV con las cuatro celdas.

## AGENTES Y ESPACIO

- **Círculos** = indígenas; **cuadrados** = no indígenas.
- El cuadrante sureste (patches con `pxcor ≥ umbral-zona-x` y `pycor ≤ umbral-zona-y`) es la Macrozona Sur. Ajustar los umbrales cambia el tamaño del tratamiento (en ELRI, ~23% de la muestra vive en la zona).
- Tras el decreto, esos patches se vuelven ocre (militarización).
- El color de los agentes escala la variable del chooser (amarillo = baja, rojo = alta).

## PARÁMETROS POR DEFECTO

Calibrados a los coeficientes FE del paper (no son estimaciones: son *targets* de simulación):

| Parámetro | Default | Target empírico |
|---|---|---|
| `shock-estallido-nacional` | 0.31 | β ola 3 resistencia = 0.311 |
| `shock-decreto-zona` | 0.90 | β zona×ola 4 resistencia = 0.903 |
| `shock-decreto-zona-control` | 1.19 | β zona×ola 4 control = 1.193 |
| `shock-decreto-triple` | 0.41 | τ₄ resistencia = 0.413 |
| `shock-decreto-triple-control` | 0.31 | τ₄ control = 0.313 |
| `shock-jp-regularizacion` | 0.60 | β JP ingroup = 0.602 |

La influencia social (`tasa-influencia`, `peso-endogrupo`) no produce el DiD por sí sola: solo suaviza y clusteriza. Si se sube demasiado, diluye los shocks.

## QUÉ NO HACE EL MODELO

- No identifica causalmente el D.S. 418 en aislamiento: el período post-45 agrega decreto y cierre constituyente, igual que el paper (Sección 8.1).
- La "mediación" JP → violencia no está cableada como path causal. H4 solo mueve la JP ingroup (Paso 1 del paper).
- No hay georreferenciación real ni comunas. El tratamiento es un bloque espacial.
- Los agentes no se mueren, no migran y no cambian de identidad (como el panel balanceado con identidad fija).

## REFERENCIA

Deneken, M., Díaz, F., Gerber, M. M., y González, R. (2026). *Apertura normativa y contención fallida*. Working paper.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

circle
false
0
Circle -7500403 true true 0 0 300

square
false
0
Rectangle -7500403 true true 30 30 270 270
@#$#@#$#@
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
