#Lee las diez olas de LAPOP Chile (2006-2026), deja las mismas variables con los mismos nombres en todas y las junta en una sola base

library(haven)
library(dplyr)



l2006 <- read_dta("data/raw/lapop_2006.dta") %>%
  zap_labels() %>%
  mutate(ingreso = ifelse(q10 > 20, NA, q10),
         ingreso = cut(ingreso, quantile(ingreso, c(0, 1/3, 2/3, 1), na.rm = TRUE),
                       include.lowest = TRUE, labels = c("Bajo", "Medio", "Alto"))) %>%
  transmute(ola = 2006, pn4, peso = wt, upm, estrato = estratopri,
            voto = chivb3, ganador = 1, vb1, vb2,
            educ = ed, ingreso, edad = q2, zona = ur, rm = chiregion == 13, genero = q1)

l2008 <- read_dta("data/raw/lapop_2008.dta") %>%
  zap_labels() %>%
  mutate(ingreso = ifelse(q10 > 20, NA, q10),
         ingreso = cut(ingreso, quantile(ingreso, c(0, 1/3, 2/3, 1), na.rm = TRUE),
                       include.lowest = TRUE, labels = c("Bajo", "Medio", "Alto"))) %>%
  transmute(ola = 2008, pn4, peso = 1, upm, estrato = estratopri,   # 2008 no trae pesos
            voto = vb3, ganador = 1301, vb1, vb2,
            educ = ed, ingreso, edad = q2, zona = ur, rm = chiregion == 13, genero = q1)

l2010 <- read_dta("data/raw/lapop_2010.dta") %>%
  zap_labels() %>%
  mutate(ingreso = ifelse(q10 > 20, NA, q10),
         ingreso = cut(ingreso, quantile(ingreso, c(0, 1/3, 2/3, 1), na.rm = TRUE),
                       include.lowest = TRUE, labels = c("Bajo", "Medio", "Alto"))) %>%
  transmute(ola = 2010, pn4, peso = wt, upm, estrato = estratopri,
            voto = vb3, ganador = 1302, vb1, vb2,
            educ = ed, ingreso, edad = q2, zona = ur, rm = chiregion == 1313, genero = q1)

l2012 <- read_dta("data/raw/lapop_2012.dta") %>%
  zap_labels() %>%
  mutate(ingreso = ifelse(q10new > 20, NA, q10new),
         ingreso = cut(ingreso, quantile(ingreso, c(0, 1/3, 2/3, 1), na.rm = TRUE),
                       include.lowest = TRUE, labels = c("Bajo", "Medio", "Alto"))) %>%
  transmute(ola = 2012, pn4, peso = wt, upm, estrato = estratopri,
            voto = vb3, ganador = 1302, vb1, vb2,
            educ = ed, ingreso, edad = q2, zona = ur, rm = prov == 1313, genero = q1)

l2014 <- read_dta("data/raw/lapop_2014.dta") %>%
  zap_labels() %>%
  mutate(ingreso = ifelse(q10new > 20, NA, q10new),
         ingreso = cut(ingreso, quantile(ingreso, c(0, 1/3, 2/3, 1), na.rm = TRUE),
                       include.lowest = TRUE, labels = c("Bajo", "Medio", "Alto"))) %>%
  transmute(ola = 2014, pn4, peso = wt, upm, estrato = estratopri,
            voto = vb3n, ganador = 1305, vb1, vb2,
            educ = ed, ingreso, edad = q2, zona = ur, rm = prov == 1313, genero = q1)

l2017 <- read_dta("data/raw/lapop_2017.dta") %>%
  zap_labels() %>%
  mutate(ingreso = ifelse(q10new > 20, NA, q10new),
         ingreso = cut(ingreso, quantile(ingreso, c(0, 1/3, 2/3, 1), na.rm = TRUE),
                       include.lowest = TRUE, labels = c("Bajo", "Medio", "Alto"))) %>%
  transmute(ola = 2017, pn4, peso = wt, upm, estrato = estratopri,
            voto = vb3n, ganador = 1305, vb1 = NA, vb2,
            educ = ed, ingreso, edad = q2, zona = ur, rm = prov == 1313, genero = q1)

l2019 <- read_dta("data/raw/lapop_2019.dta") %>%
  zap_labels() %>%
  mutate(ingreso = ifelse(q10new > 20, NA, q10new),
         ingreso = cut(ingreso, quantile(ingreso, c(0, 1/3, 2/3, 1), na.rm = TRUE),
                       include.lowest = TRUE, labels = c("Bajo", "Medio", "Alto"))) %>%
  transmute(ola = 2019, pn4, peso = wt, upm, estrato = estratopri,
            voto = vb3n, ganador = 1301, vb1 = NA, vb2,
            educ = ed, ingreso, edad = q2, zona = ur, rm = prov == 1313, genero = q1)

l2021 <- read_dta("data/raw/lapop_2021.dta") %>%
  zap_labels() %>%
  transmute(ola = 2021, pn4, peso = wt, upm, estrato = estratopri,
            voto = NA, ganador = NA, vb1 = NA, vb2 = NA,
            educ = edr, ingreso = NA, edad = q2, zona = NA, rm = prov1t == 1300013, genero = q1tb)

l2023 <- read_dta("data/raw/lapop_2023.dta") %>%
  zap_labels() %>%
  mutate(ingreso = cut(q10inc, quantile(q10inc, c(0, 1/3, 2/3, 1), na.rm = TRUE),
                       include.lowest = TRUE, labels = c("Bajo", "Medio", "Alto"))) %>%
  transmute(ola = 2023, pn4, peso = wt, upm, estrato = estratopri,
            voto = vb3n, ganador = 1301, vb1 = NA, vb2,
            educ = edre, ingreso, edad = q2, zona = ur, rm = chiregion == 1313, genero = q1tc_r)


l2026 <- read_dta("data/raw/lapop_2026.dta") %>%
  zap_labels() %>%
  filter(pais == 13) %>%
  mutate(ingreso = cut(q10inc, quantile(q10inc, c(0, 1/3, 2/3, 1), na.rm = TRUE),
                       include.lowest = TRUE, labels = c("Bajo", "Medio", "Alto"))) %>%
  transmute(ola = 2026, pn4, peso = wt, upm, estrato = strata,
            voto = NA, ganador = NA, vb1 = NA, vb2 = NA,
            educ = edre, ingreso, edad = q2, zona = ur, rm = region == 1304, genero = q1tc_r)

lapop <- bind_rows(l2006, l2008, l2010, l2012, l2014, l2017, l2019, l2021, l2023, l2026)

lapop <- lapop %>%
  mutate(
    satisfecho = case_when(pn4 == 1 | pn4 == 2 ~ 1,
                           pn4 == 3 | pn4 == 4 ~ 0),
    
    voto = case_when(vb2 == 2 ~ "No votó, blanco o nulo",
                     is.na(vb2) & vb1 == 2 ~ "No votó, blanco o nulo",   # no inscritos (2010)
                     voto == 0 | voto == 97 ~ "No votó, blanco o nulo",
                     voto == ganador ~ "Ganador",
                     voto != 88 & voto != 98 & voto != 99 ~ "Perdedor"),
    
    educ = case_when(ola <= 2019 & educ <= 8  ~ "Básica o menos",
                     ola <= 2019 & educ <= 12 ~ "Media",
                     ola <= 2019 & educ <= 30 ~ "Superior",
                     ola == 2021 & educ <= 1  ~ "Básica o menos",
                     ola == 2021 & educ == 2  ~ "Media",
                     ola == 2021 & educ == 3  ~ "Superior",
                     ola >= 2023 & educ <= 2  ~ "Básica o menos",
                     ola >= 2023 & educ <= 4  ~ "Media",
                     ola >= 2023 & educ <= 6  ~ "Superior"),
    
    ingreso = case_when(ola == 2021 ~ NA,
                        is.na(ingreso) ~ "No declara",
                        TRUE ~ as.character(ingreso)),
    
    edad = case_when(edad >= 18 & edad <= 29 ~ "18-29",
                     edad >= 30 & edad <= 44 ~ "30-44",
                     edad >= 45 & edad <= 59 ~ "45-59",
                     edad >= 60 & edad <= 100 ~ "60 o más"),
    
    zona   = case_when(zona == 1 ~ "Urbana", zona == 2 ~ "Rural"),
    region = case_when(rm == TRUE ~ "Región Metropolitana", rm == FALSE ~ "Otras regiones"),
    genero = case_when(genero == 1 ~ "Hombre", genero == 2 ~ "Mujer")
  )

lapop <- lapop %>%
  mutate(voto    = factor(voto, levels = c("Perdedor", "Ganador", "No votó, blanco o nulo")),
         educ    = factor(educ, levels = c("Básica o menos", "Media", "Superior")),
         ingreso = factor(ingreso, levels = c("Bajo", "Medio", "Alto", "No declara")),
         edad    = factor(edad, levels = c("18-29", "30-44", "45-59", "60 o más")),
         zona    = factor(zona, levels = c("Urbana", "Rural")),
         region  = factor(region, levels = c("Otras regiones", "Región Metropolitana")),
         genero  = factor(genero, levels = c("Hombre", "Mujer"))) %>%
  select(ola, satisfecho, peso, upm, estrato, voto, educ, ingreso, edad, zona, region, genero)

saveRDS(lapop, "data/lapop.rds")