library(dplyr)
library(survey)
library(modelsummary)

options(survey.lonely.psu = "adjust")

lapop <- readRDS("data/lapop.rds") %>%
  filter(ola != 2021, !is.na(satisfecho)) %>%
  mutate(upm_ola = paste(ola, upm),
         estrato_ola = paste(ola, estrato))

diseno <- svydesign(ids = ~upm_ola, strata = ~estrato_ola, weights = ~peso,
                    data = lapop, nest = TRUE)

m_total <- svyglm(satisfecho ~ educ + ingreso + edad + zona + region + genero + factor(ola),
                  design = diseno, family = quasibinomial())

m_2006_2014 <- svyglm(satisfecho ~ educ + ingreso + edad + zona + region + genero + factor(ola),
                      design = subset(diseno, ola <= 2014), family = quasibinomial())

m_2017_2019 <- svyglm(satisfecho ~ educ + ingreso + edad + zona + region + genero + factor(ola),
                      design = subset(diseno, ola == 2017 | ola == 2019), family = quasibinomial())

m_2023_2026 <- svyglm(satisfecho ~ educ + ingreso + edad + zona + region + genero + factor(ola),
                      design = subset(diseno, ola >= 2023), family = quasibinomial())

m_voto <- svyglm(satisfecho ~ voto + educ + ingreso + edad + zona + region + genero + factor(ola),
                 design = subset(diseno, ola <= 2023), family = quasibinomial())

modelos <- list("2006-2026" = m_total,
                "2006-2014" = m_2006_2014,
                "2017-2019" = m_2017_2019,
                "2023-2026" = m_2023_2026,
                "2006-2023 con voto" = m_voto)

nombres <- c("votoGanador" = "Voto: ganador",
             "votoNo votó, blanco o nulo" = "Voto: no votó, blanco o nulo",
             "educMedia" = "Educación: media",
             "educSuperior" = "Educación: superior",
             "ingresoMedio" = "Ingreso: medio",
             "ingresoAlto" = "Ingreso: alto",
             "ingresoNo declara" = "Ingreso: no declara",
             "edad30-44" = "Edad: 30-44",
             "edad45-59" = "Edad: 45-59",
             "edad60 o más" = "Edad: 60 o más",
             "zonaRural" = "Zona: rural",
             "regionRegión Metropolitana" = "Región Metropolitana",
             "generoMujer" = "Mujer")

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
saveRDS(modelos, "output/tables/modelos.rds")

modelsummary(modelos,
             exponentiate = TRUE,
             statistic = "conf.int",
             stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
             coef_map = nombres,
             gof_map = "nobs",
             title = "Razones de odds de estar satisfecho con la democracia",
             notes = c("Regresión logística con efectos fijos de ola y diseño muestral. Intervalos de confianza de 95% entre corchetes.",
                       "Categorías de referencia: perdedor, básica o menos, ingreso bajo, 18-29, urbana, otras regiones, hombre.",
                       "Fuente: Barómetro de las Américas (LAPOP), Chile 2006-2026. Sin la ola telefónica de 2021."),
             output = "output/tables/t3_modelos.html")
