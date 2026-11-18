# T1 son olas de la encuesta; T2 es % satisfecho por grupo y ola.

library(dplyr)
library(tidyr)

lapop <- readRDS("data/lapop.rds")

#T1

t1 <- lapop %>%
  group_by(ola) %>%
  summarise(casos = n(),
            casos_validos = sum(!is.na(satisfecho)))

t1$campo <- c("jul-ago 2006", "dic 2007-ene 2008", "abr-jun 2010", "mar-abr 2012",
              "abr-jun 2014", "mar-may 2017", "ene-mar 2019", "feb-may 2021",
              "jun-ago 2023", "mar-jun 2026")
t1$gobierno <- c("Bachelet I", "Bachelet I", "Piñera I", "Piñera I", "Bachelet II",
                 "Bachelet II", "Piñera II", "Piñera II", "Boric", "Kast")
t1$modo <- c(rep("Presencial", 7), "Telefónica", "Presencial", "Presencial")


t1 <- select(t1, ola, campo, gobierno, modo, casos, casos_validos)


t_total <- lapop %>%
  group_by(ola) %>%
  summarise(pct = weighted.mean(satisfecho, peso, na.rm = TRUE), .groups = "drop") %>%
  mutate(variable = "Total", categoria = "Chile")

t_voto <- lapop %>%
  filter(!is.na(voto)) %>%
  group_by(ola, categoria = voto) %>%
  summarise(pct = weighted.mean(satisfecho, peso, na.rm = TRUE), .groups = "drop") %>%
  mutate(variable = "Voto")

t_educ <- lapop %>%
  filter(!is.na(educ)) %>%
  group_by(ola, categoria = educ) %>%
  summarise(pct = weighted.mean(satisfecho, peso, na.rm = TRUE), .groups = "drop") %>%
  mutate(variable = "Educación")

t_ingreso <- lapop %>%
  filter(!is.na(ingreso)) %>%
  group_by(ola, categoria = ingreso) %>%
  summarise(pct = weighted.mean(satisfecho, peso, na.rm = TRUE), .groups = "drop") %>%
  mutate(variable = "Ingreso")

t_edad <- lapop %>%
  filter(!is.na(edad)) %>%
  group_by(ola, categoria = edad) %>%
  summarise(pct = weighted.mean(satisfecho, peso, na.rm = TRUE), .groups = "drop") %>%
  mutate(variable = "Edad")

t_zona <- lapop %>%
  filter(!is.na(zona)) %>%
  group_by(ola, categoria = zona) %>%
  summarise(pct = weighted.mean(satisfecho, peso, na.rm = TRUE), .groups = "drop") %>%
  mutate(variable = "Zona")

t_region <- lapop %>%
  filter(!is.na(region)) %>%
  group_by(ola, categoria = region) %>%
  summarise(pct = weighted.mean(satisfecho, peso, na.rm = TRUE), .groups = "drop") %>%
  mutate(variable = "Región")


t_genero <- lapop %>%
  filter(!is.na(genero)) %>%
  group_by(ola, categoria = genero) %>%
  summarise(pct = weighted.mean(satisfecho, peso, na.rm = TRUE), .groups = "drop") %>%
  mutate(variable = "Género")

#T2

t2 <- bind_rows(t_total, t_voto, t_educ, t_ingreso, t_edad, t_zona, t_region, t_genero) %>%
  mutate(categoria = as.character(categoria),
         pct = round(100 * pct, 1)) %>%
  pivot_wider(names_from = ola, values_from = pct) %>%
  select(variable, categoria, everything())



dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
saveRDS(t1, "output/tables/t1_olas.rds")
saveRDS(t2, "output/tables/t2_satisfaccion_grupos.rds")