# Ganadores y perdedores: las bases de la insatisfacción democrática en Chile, 2006-2026

Código de replicación del trabajo grupal del curso ICP5006 Medición y Análisis Dimensional de Datos Políticos (Magíster en Ciencia Política, Instituto de Ciencia Política, Pontificia Universidad Católica de Chile), dictado por la profesora Soledad Araya Orrego en el segundo semestre de 2026.

**Autores:** Lucas Bottinelli Baltra, Matías Llorens Rubio y Benjamín Zúñiga Silva.

## Pregunta e hipótesis

¿En qué medida la posición electoral, socioeconómica, territorial y generacional de las personas explica su satisfacción con el funcionamiento de la democracia en Chile, y cómo evolucionaron esas brechas entre 2006 y 2026?

El trabajo extiende el marco de ganadores y perdedores (Anderson y Guillory, 1997) más allá de la arena electoral. La expectativa general es que la insatisfacción se concentre en quienes ocupan posiciones perdedoras y que esa concentración haya crecido en el tiempo.

| Hipótesis | Expectativa |
|---|---|
| H1. Condición electoral | Quienes votaron por el presidente en ejercicio están más satisfechos que quienes votaron por otro candidato. |
| H2. Posición socioeconómica | Las personas con menos educación e ingreso están menos satisfechas. |
| H3. Edad | Las personas mayores están más satisfechas que las jóvenes. |
| H4. Acceso a la ciudadanía | Quienes viven en zonas rurales y fuera de la Región Metropolitana están menos satisfechos. |

## Datos

Barómetro de las Américas (LAPOP/AmericasBarometer), Chile. Diez olas, todas muestras nacionales independientes de personas adultas en edad de votar (cortes transversales repetidos, sin panel).

| Ola | Trabajo de campo | Gobierno | Modo | N válido en PN4 |
|---|---|---|---|---|
| 2006 | jul-ago 2006 | Bachelet I | Presencial | 1.464 |
| 2008 | dic 2007-ene 2008 | Bachelet I | Presencial | 1.446 |
| 2010 | abr-jun 2010 | Piñera I | Presencial | 1.865 |
| 2012 | mar-abr 2012 | Piñera I | Presencial | 734 |
| 2014 | abr-jun 2014 | Bachelet II | Presencial | 1.418 |
| 2017 | mar-may 2017 | Bachelet II | Presencial | 1.554 |
| 2019 | ene-mar 2019 | Piñera II | Presencial | 1.552 |
| 2021 | feb-may 2021 | Piñera II | Telefónica | 1.423 |
| 2023 | jun-ago 2023 | Boric | Presencial | 1.605 |
| 2026 | mar-jun 2026 | Kast | Presencial | 976 |

En 2012 PN4 se aplicó a la mitad de la muestra. La ola 2026 proviene del archivo regional y se filtra para Chile (`pais == 13`).

## Cómo reproducir el análisis

1. Abrir `LAPOP_FINAL.Rproj` en RStudio.
2. Instalar los paquetes, una sola vez:

   ```r
   install.packages(c("haven", "dplyr", "tidyr", "ggplot2", "survey",
                      "patchwork", "modelsummary", "tinytable"))
   ```

3. Correr los scripts en orden:

   ```r
   source("scripts/00_descarga.R")        # solo la primera vez
   source("scripts/01_datos.R")
   source("scripts/02_tablas.R")
   source("scripts/03_graficos.R")
   source("scripts/04_modelos.R")
   source("scripts/05_prob_predichas.R")
   source("scripts/06_test_de_wald.R")
   ```


## Estructura

```
LAPOP_FINAL/
├── LAPOP_FINAL.Rproj
├── README.md
├── .gitignore
├── data/
│   ├── raw/                    bases originales de LAPOP (no versionadas)
│   └── lapop.rds               base armonizada (no versionada, la crea 01)
├── scripts/
│   ├── 00_descarga.R           descarga las diez olas
│   ├── 01_datos.R              armoniza las variables y une las olas
│   ├── 02_tablas.R             tablas descriptivas por ola y por grupo
│   ├── 03_graficos.R           figuras descriptivas
│   ├── 04_modelos.R            modelos logit con diseño muestral
│   ├── 05_prob_predichas.R     probabilidades predichas y razones de odds por período
│   └── 06_test_de_wald.R       test de igualdad de efectos entre períodos
└── output/
    ├── tables/
    └── figures/
```

## Correspondencia con el informe

| Informe | Archivo | Script |
|---|---|---|
| Figura 1. Olas y gobierno vigente | Hecha a mano, no se genera en este repositorio | |
| Figura 2. Satisfacción nacional, 2006-2026 | `output/figures/f1_tendencia_nacional.png` | `03` |
| Figura 3. Grupos seleccionados y promedio nacional | `output/figures/f2c_grupos_un_panel.png` | `03` |
| Figura 4. Diferencia de cada grupo con el promedio nacional | `output/figures/f3_mapa_calor.png` | `03` |
| Figura 5. Razones de odds por período | `output/figures/f5_coeficientes_periodo.png` | `05` |
| Tabla 2. % satisfecho y casos válidos por ola | `output/tables/t1_olas.rds` y fila "Total" de `t2_satisfaccion_grupos.rds` | `02` |
| Tabla 3. Modelos del período completo | `output/tables/t3_modelos.html` (columnas "2006-2026" y "2006-2023 con voto") | `04` |
| Tabla 4. Probabilidades predichas | `output/tables/t5_probabilidades.rds` | `05` |
| Tabla 5. Modelos por período y test de Wald | `output/tables/t3_modelos.html` (columnas por período) y `output/tables/t4_wald.rds` | `04`, `06` |


Salidas adicionales que no aparecen en el informe:

| Archivo | Contenido |
|---|---|
| `output/tables/t2_satisfaccion_grupos.rds` | % satisfecho por grupo y ola (ponderado) |
| `output/figures/f2_grupos.png` y `f2_*.png` | Satisfacción por grupo con intervalos como barras, en conjunto y por variable |
| `output/figures/f2b_grupos.png` y `f2b_*.png` | Lo mismo, con intervalos como bandas |
| `output/figures/f4_probabilidades.png` | Versión gráfica de la Tabla 4 |

