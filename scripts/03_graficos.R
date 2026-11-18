library(dplyr)
library(ggplot2)
library(survey)
library(patchwork)

options(survey.lonely.psu = "adjust")

lapop <- readRDS("data/lapop.rds") %>%
  filter(!is.na(satisfecho)) %>%
  mutate(upm_ola = paste(ola, upm),
         estrato_ola = paste(ola, estrato))

diseno <- svydesign(ids = ~upm_ola, strata = ~estrato_ola, weights = ~peso,
                    data = lapop, nest = TRUE)

diseno2 <- subset(diseno, ola != 2021)

olas <- c(2006, 2008, 2010, 2012, 2014, 2017, 2019, 2023, 2026)
fuente <- "Fuente: Barómetro de las Américas (LAPOP), Chile 2006-2026."
pregunta <- "Pregunta: En general, ¿usted diría que está muy satisfecho(a), satisfecho(a), insatisfecho(a) o muy insatisfecho(a)\ncon la forma en que la democracia funciona en Chile? Se muestra el % que responde satisfecho(a) o muy satisfecho(a).\nFuente: Barómetro de las Américas (LAPOP), Chile 2006-2026."

dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

tema <- theme_minimal() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))

nac_todo <- svyby(~satisfecho, ~ola, diseno, svymean) %>%
  mutate(pct = 100 * satisfecho,
         li = 100 * (satisfecho - 1.96 * se),
         ls = 100 * (satisfecho + 1.96 * se),
         modo = ifelse(ola == 2021, "Telefónica", "Presencial"))

nac_todo$fecha <- c(2006.6, 2007.95, 2010.4, 2012.25, 2014.4,
                    2017.35, 2019.15, 2021.3, 2023.55, 2026.35)

gobiernos <- data.frame(
  nombre = c("Bachelet I", "Piñera I", "Bachelet II", "Piñera II", "Boric", "Kast"),
  inicio = c(2006.2, 2010.2, 2014.2, 2018.2, 2022.2, 2026.2),
  fin = c(2010.2, 2014.2, 2018.2, 2022.2, 2026.2, 2027),
  sector = c("Centroizquierda", "Centroderecha", "Centroizquierda", "Centroderecha", "Izquierda", "Derecha")
)

eventos <- data.frame(
  nombre = c("Terremoto 27F", "Movilización estudiantil", "Casos Penta y Caval",
             "Estallido social", "Pandemia", "Plebiscito constitucional 2022",
             "Plebiscito constitucional 2023"),
  fecha = c(2010.15, 2011.45, 2015.1, 2019.8, 2020.2, 2022.7, 2023.96)
)

f1 <- ggplot() +
  geom_rect(data = gobiernos, aes(xmin = inicio, xmax = fin, ymin = -Inf, ymax = Inf, fill = sector),
            alpha = 0.25) +
  geom_text(data = gobiernos, aes(x = (inicio + fin) / 2, y = 78, label = nombre), size = 3) +
  geom_vline(data = eventos, aes(xintercept = fecha), linetype = "dotted", color = "grey40") +
  geom_text(data = eventos, aes(x = fecha, y = 15, label = nombre),
            angle = 90, hjust = 0, vjust = -0.4, size = 2.8, color = "grey30") +
  geom_line(data = nac_todo, aes(x = fecha, y = pct)) +
  geom_errorbar(data = nac_todo, aes(x = fecha, ymin = li, ymax = ls), width = 0.25) +
  geom_point(data = nac_todo, aes(x = fecha, y = pct, shape = modo), size = 2.5) +
  scale_shape_manual(values = c(16, 1)) +
  scale_fill_manual(values = c("Izquierda" = "#e07a5f", "Centroizquierda" = "#f2cc8f",
                               "Centroderecha" = "#a8c5da", "Derecha" = "#3d5a80"),
                    breaks = c("Izquierda", "Centroizquierda", "Centroderecha", "Derecha")) +
  scale_x_continuous(breaks = seq(2006, 2026, 2)) +
  coord_cartesian(ylim = c(15, 80), xlim = c(2006, 2027)) +
  labs(title = "Satisfacción con la democracia en Chile, 2006-2026",
       x = NULL, y = "% satisfecho o muy satisfecho", fill = "Gobierno", shape = "Encuesta",
       caption = pregunta) +
  theme_minimal() +
  theme(legend.position = "bottom", plot.caption = element_text(hjust = 0))

ggsave("output/figures/f1_tendencia_nacional.png", f1, width = 10, height = 6.5, bg = "white")

nac <- svyby(~satisfecho, ~ola, diseno2, svymean) %>%
  mutate(pct = 100 * satisfecho)

g_voto <- svyby(~satisfecho, ~ola + voto, subset(diseno2, !is.na(voto)), svymean) %>%
  mutate(pct = 100 * satisfecho, li = 100 * (satisfecho - 1.96 * se), ls = 100 * (satisfecho + 1.96 * se))

g_educ <- svyby(~satisfecho, ~ola + educ, subset(diseno2, !is.na(educ)), svymean) %>%
  mutate(pct = 100 * satisfecho, li = 100 * (satisfecho - 1.96 * se), ls = 100 * (satisfecho + 1.96 * se))

g_ingreso <- svyby(~satisfecho, ~ola + ingreso, subset(diseno2, !is.na(ingreso)), svymean) %>%
  mutate(pct = 100 * satisfecho, li = 100 * (satisfecho - 1.96 * se), ls = 100 * (satisfecho + 1.96 * se))

g_edad <- svyby(~satisfecho, ~ola + edad, subset(diseno2, !is.na(edad)), svymean) %>%
  mutate(pct = 100 * satisfecho, li = 100 * (satisfecho - 1.96 * se), ls = 100 * (satisfecho + 1.96 * se))

g_zona <- svyby(~satisfecho, ~ola + zona, subset(diseno2, !is.na(zona)), svymean) %>%
  mutate(pct = 100 * satisfecho, li = 100 * (satisfecho - 1.96 * se), ls = 100 * (satisfecho + 1.96 * se))

g_region <- svyby(~satisfecho, ~ola + region, subset(diseno2, !is.na(region)), svymean) %>%
  mutate(pct = 100 * satisfecho, li = 100 * (satisfecho - 1.96 * se), ls = 100 * (satisfecho + 1.96 * se))

g_genero <- svyby(~satisfecho, ~ola + genero, subset(diseno2, !is.na(genero)), svymean) %>%
  mutate(pct = 100 * satisfecho, li = 100 * (satisfecho - 1.96 * se), ls = 100 * (satisfecho + 1.96 * se))

pd <- position_dodge(width = 0.8)

f2_voto <- ggplot(g_voto, aes(x = ola, y = pct, color = voto)) +
  geom_line(data = nac, aes(x = ola, y = pct), color = "grey40", linetype = "dashed", inherit.aes = FALSE) +
  geom_line(position = pd) +
  geom_errorbar(aes(ymin = li, ymax = ls), width = 0, position = pd) +
  geom_point(position = pd) +
  scale_color_discrete(labels = paste0(levels(lapop$voto), " (", round(100 * prop.table(table(lapop$voto))), "%)")) +
  scale_x_continuous(breaks = olas) +
  labs(title = "Voto en la última elección presidencial", x = NULL, y = "% satisfecho", color = NULL) +
  tema

f2_educ <- ggplot(g_educ, aes(x = ola, y = pct, color = educ)) +
  geom_line(data = nac, aes(x = ola, y = pct), color = "grey40", linetype = "dashed", inherit.aes = FALSE) +
  geom_line(position = pd) +
  geom_errorbar(aes(ymin = li, ymax = ls), width = 0, position = pd) +
  geom_point(position = pd) +
  scale_color_discrete(labels = paste0(levels(lapop$educ), " (", round(100 * prop.table(table(lapop$educ))), "%)")) +
  scale_x_continuous(breaks = olas) +
  labs(title = "Educación", x = NULL, y = "% satisfecho", color = NULL) +
  tema

f2_ingreso <- ggplot(g_ingreso, aes(x = ola, y = pct, color = ingreso)) +
  geom_line(data = nac, aes(x = ola, y = pct), color = "grey40", linetype = "dashed", inherit.aes = FALSE) +
  geom_line(position = pd) +
  geom_errorbar(aes(ymin = li, ymax = ls), width = 0, position = pd) +
  geom_point(position = pd) +
  scale_color_discrete(labels = paste0(levels(lapop$ingreso), " (", round(100 * prop.table(table(lapop$ingreso))), "%)")) +
  scale_x_continuous(breaks = olas) +
  labs(title = "Ingreso del hogar", x = NULL, y = "% satisfecho", color = NULL) +
  tema

f2_edad <- ggplot(g_edad, aes(x = ola, y = pct, color = edad)) +
  geom_line(data = nac, aes(x = ola, y = pct), color = "grey40", linetype = "dashed", inherit.aes = FALSE) +
  geom_line(position = pd) +
  geom_errorbar(aes(ymin = li, ymax = ls), width = 0, position = pd) +
  geom_point(position = pd) +
  scale_color_discrete(labels = paste0(levels(lapop$edad), " (", round(100 * prop.table(table(lapop$edad))), "%)")) +
  scale_x_continuous(breaks = olas) +
  labs(title = "Edad", x = NULL, y = "% satisfecho", color = NULL) +
  tema

f2_zona <- ggplot(g_zona, aes(x = ola, y = pct, color = zona)) +
  geom_line(data = nac, aes(x = ola, y = pct), color = "grey40", linetype = "dashed", inherit.aes = FALSE) +
  geom_line(position = pd) +
  geom_errorbar(aes(ymin = li, ymax = ls), width = 0, position = pd) +
  geom_point(position = pd) +
  scale_color_discrete(labels = paste0(levels(lapop$zona), " (", round(100 * prop.table(table(lapop$zona))), "%)")) +
  scale_x_continuous(breaks = olas) +
  labs(title = "Zona", x = NULL, y = "% satisfecho", color = NULL) +
  tema

f2_region <- ggplot(g_region, aes(x = ola, y = pct, color = region)) +
  geom_line(data = nac, aes(x = ola, y = pct), color = "grey40", linetype = "dashed", inherit.aes = FALSE) +
  geom_line(position = pd) +
  geom_errorbar(aes(ymin = li, ymax = ls), width = 0, position = pd) +
  geom_point(position = pd) +
  scale_color_discrete(labels = paste0(levels(lapop$region), " (", round(100 * prop.table(table(lapop$region))), "%)")) +
  scale_x_continuous(breaks = olas) +
  labs(title = "Región", x = NULL, y = "% satisfecho", color = NULL) +
  tema

f2_genero <- ggplot(g_genero, aes(x = ola, y = pct, color = genero)) +
  geom_line(data = nac, aes(x = ola, y = pct), color = "grey40", linetype = "dashed", inherit.aes = FALSE) +
  geom_line(position = pd) +
  geom_errorbar(aes(ymin = li, ymax = ls), width = 0, position = pd) +
  geom_point(position = pd) +
  scale_color_discrete(labels = paste0(levels(lapop$genero), " (", round(100 * prop.table(table(lapop$genero))), "%)")) +
  scale_x_continuous(breaks = olas) +
  labs(title = "Género", x = NULL, y = "% satisfecho", color = NULL) +
  tema

f2 <- f2_voto + f2_educ + f2_ingreso + f2_edad + f2_zona + f2_region + f2_genero +
  plot_layout(ncol = 2) +
  plot_annotation(title = "Satisfacción con la democracia por grupo, 2006-2026",
                  subtitle = "Línea punteada gris: promedio nacional. Barras: intervalos de confianza al 95%. Entre paréntesis, % de la muestra. Sin la ola telefónica de 2021.",
                  caption = fuente)

ggsave("output/figures/f2_grupos.png", f2, width = 12, height = 15, bg = "white")

ggsave("output/figures/f2_voto.png", f2_voto, width = 8, height = 5, bg = "white")
ggsave("output/figures/f2_educacion.png", f2_educ, width = 8, height = 5, bg = "white")
ggsave("output/figures/f2_ingreso.png", f2_ingreso, width = 8, height = 5, bg = "white")
ggsave("output/figures/f2_edad.png", f2_edad, width = 8, height = 5, bg = "white")
ggsave("output/figures/f2_zona.png", f2_zona, width = 8, height = 5, bg = "white")
ggsave("output/figures/f2_region.png", f2_region, width = 8, height = 5, bg = "white")
ggsave("output/figures/f2_genero.png", f2_genero, width = 8, height = 5, bg = "white")

f2b_voto <- ggplot(g_voto, aes(x = ola, y = pct, color = voto)) +
  geom_ribbon(aes(ymin = li, ymax = ls, fill = voto), alpha = 0.15, color = NA, show.legend = FALSE) +
  geom_line(data = nac, aes(x = ola, y = pct), color = "grey40", linetype = "dashed", inherit.aes = FALSE) +
  geom_line() +
  geom_point() +
  scale_color_discrete(labels = paste0(levels(lapop$voto), " (", round(100 * prop.table(table(lapop$voto))), "%)")) +
  scale_x_continuous(breaks = olas) +
  labs(title = "Voto en la última elección presidencial", x = NULL, y = "% satisfecho", color = NULL) +
  tema

f2b_educ <- ggplot(g_educ, aes(x = ola, y = pct, color = educ)) +
  geom_ribbon(aes(ymin = li, ymax = ls, fill = educ), alpha = 0.15, color = NA, show.legend = FALSE) +
  geom_line(data = nac, aes(x = ola, y = pct), color = "grey40", linetype = "dashed", inherit.aes = FALSE) +
  geom_line() +
  geom_point() +
  scale_color_discrete(labels = paste0(levels(lapop$educ), " (", round(100 * prop.table(table(lapop$educ))), "%)")) +
  scale_x_continuous(breaks = olas) +
  labs(title = "Educación", x = NULL, y = "% satisfecho", color = NULL) +
  tema

f2b_ingreso <- ggplot(g_ingreso, aes(x = ola, y = pct, color = ingreso)) +
  geom_ribbon(aes(ymin = li, ymax = ls, fill = ingreso), alpha = 0.15, color = NA, show.legend = FALSE) +
  geom_line(data = nac, aes(x = ola, y = pct), color = "grey40", linetype = "dashed", inherit.aes = FALSE) +
  geom_line() +
  geom_point() +
  scale_color_discrete(labels = paste0(levels(lapop$ingreso), " (", round(100 * prop.table(table(lapop$ingreso))), "%)")) +
  scale_x_continuous(breaks = olas) +
  labs(title = "Ingreso del hogar", x = NULL, y = "% satisfecho", color = NULL) +
  tema

f2b_edad <- ggplot(g_edad, aes(x = ola, y = pct, color = edad)) +
  geom_ribbon(aes(ymin = li, ymax = ls, fill = edad), alpha = 0.15, color = NA, show.legend = FALSE) +
  geom_line(data = nac, aes(x = ola, y = pct), color = "grey40", linetype = "dashed", inherit.aes = FALSE) +
  geom_line() +
  geom_point() +
  scale_color_discrete(labels = paste0(levels(lapop$edad), " (", round(100 * prop.table(table(lapop$edad))), "%)")) +
  scale_x_continuous(breaks = olas) +
  labs(title = "Edad", x = NULL, y = "% satisfecho", color = NULL) +
  tema

f2b_zona <- ggplot(g_zona, aes(x = ola, y = pct, color = zona)) +
  geom_ribbon(aes(ymin = li, ymax = ls, fill = zona), alpha = 0.15, color = NA, show.legend = FALSE) +
  geom_line(data = nac, aes(x = ola, y = pct), color = "grey40", linetype = "dashed", inherit.aes = FALSE) +
  geom_line() +
  geom_point() +
  scale_color_discrete(labels = paste0(levels(lapop$zona), " (", round(100 * prop.table(table(lapop$zona))), "%)")) +
  scale_x_continuous(breaks = olas) +
  labs(title = "Zona", x = NULL, y = "% satisfecho", color = NULL) +
  tema

f2b_region <- ggplot(g_region, aes(x = ola, y = pct, color = region)) +
  geom_ribbon(aes(ymin = li, ymax = ls, fill = region), alpha = 0.15, color = NA, show.legend = FALSE) +
  geom_line(data = nac, aes(x = ola, y = pct), color = "grey40", linetype = "dashed", inherit.aes = FALSE) +
  geom_line() +
  geom_point() +
  scale_color_discrete(labels = paste0(levels(lapop$region), " (", round(100 * prop.table(table(lapop$region))), "%)")) +
  scale_x_continuous(breaks = olas) +
  labs(title = "Región", x = NULL, y = "% satisfecho", color = NULL) +
  tema

f2b_genero <- ggplot(g_genero, aes(x = ola, y = pct, color = genero)) +
  geom_ribbon(aes(ymin = li, ymax = ls, fill = genero), alpha = 0.15, color = NA, show.legend = FALSE) +
  geom_line(data = nac, aes(x = ola, y = pct), color = "grey40", linetype = "dashed", inherit.aes = FALSE) +
  geom_line() +
  geom_point() +
  scale_color_discrete(labels = paste0(levels(lapop$genero), " (", round(100 * prop.table(table(lapop$genero))), "%)")) +
  scale_x_continuous(breaks = olas) +
  labs(title = "Género", x = NULL, y = "% satisfecho", color = NULL) +
  tema

f2b <- f2b_voto + f2b_educ + f2b_ingreso + f2b_edad + f2b_zona + f2b_region + f2b_genero +
  plot_layout(ncol = 2) +
  plot_annotation(title = "Satisfacción con la democracia por grupo, 2006-2026",
                  subtitle = "Línea punteada gris: promedio nacional. Bandas: intervalos de confianza al 95%. Entre paréntesis, % de la muestra. Sin la ola telefónica de 2021.",
                  caption = fuente)

ggsave("output/figures/f2b_grupos.png", f2b, width = 12, height = 15, bg = "white")

ggsave("output/figures/f2b_voto.png", f2b_voto, width = 8, height = 5, bg = "white")
ggsave("output/figures/f2b_educacion.png", f2b_educ, width = 8, height = 5, bg = "white")
ggsave("output/figures/f2b_ingreso.png", f2b_ingreso, width = 8, height = 5, bg = "white")
ggsave("output/figures/f2b_edad.png", f2b_edad, width = 8, height = 5, bg = "white")
ggsave("output/figures/f2b_zona.png", f2b_zona, width = 8, height = 5, bg = "white")
ggsave("output/figures/f2b_region.png", f2b_region, width = 8, height = 5, bg = "white")
ggsave("output/figures/f2b_genero.png", f2b_genero, width = 8, height = 5, bg = "white")

unico <- bind_rows(
  g_voto %>% filter(voto == "Ganador") %>%
    mutate(grupo = paste0("Ganadores (", round(100 * mean(lapop$voto == "Ganador", na.rm = TRUE)), "%)")),
  g_educ %>% filter(educ == "Superior") %>%
    mutate(grupo = paste0("Educación superior (", round(100 * mean(lapop$educ == "Superior", na.rm = TRUE)), "%)")),
  g_ingreso %>% filter(ingreso == "Alto") %>%
    mutate(grupo = paste0("Ingreso alto (", round(100 * mean(lapop$ingreso == "Alto", na.rm = TRUE)), "%)")),
  g_edad %>% filter(edad == "18-29") %>%
    mutate(grupo = paste0("18 a 29 años (", round(100 * mean(lapop$edad == "18-29", na.rm = TRUE)), "%)")),
  g_zona %>% filter(zona == "Rural") %>%
    mutate(grupo = paste0("Rural (", round(100 * mean(lapop$zona == "Rural", na.rm = TRUE)), "%)")),
  g_region %>% filter(region == "Región Metropolitana") %>%
    mutate(grupo = paste0("Región Metropolitana (", round(100 * mean(lapop$region == "Región Metropolitana", na.rm = TRUE)), "%)")),
  g_genero %>% filter(genero == "Mujer") %>%
    mutate(grupo = paste0("Mujeres (", round(100 * mean(lapop$genero == "Mujer", na.rm = TRUE)), "%)"))
)

f2c <- ggplot(unico, aes(x = ola, y = pct, color = grupo)) +
  geom_line() +
  geom_point() +
  geom_line(data = nac, aes(x = ola, y = pct), color = "black", linewidth = 1.2, inherit.aes = FALSE) +
  scale_x_continuous(breaks = olas) +
  labs(title = "Satisfacción con la democracia: grupos seleccionados y promedio nacional",
       subtitle = "Línea negra: promedio nacional. Entre paréntesis, % de la muestra.",
       x = NULL, y = "% satisfecho o muy satisfecho", color = NULL, caption = fuente) +
  theme_minimal()

ggsave("output/figures/f2c_grupos_un_panel.png", f2c, width = 10, height = 6, bg = "white")

calor <- bind_rows(
  g_voto %>% rename(categoria = voto) %>% mutate(variable = "Voto"),
  g_educ %>% rename(categoria = educ) %>% mutate(variable = "Educación"),
  g_ingreso %>% rename(categoria = ingreso) %>% mutate(variable = "Ingreso"),
  g_edad %>% rename(categoria = edad) %>% mutate(variable = "Edad"),
  g_zona %>% rename(categoria = zona) %>% mutate(variable = "Zona"),
  g_region %>% rename(categoria = region) %>% mutate(variable = "Región"),
  g_genero %>% rename(categoria = genero) %>% mutate(variable = "Género")
) %>%
  left_join(select(nac, ola, nacional = pct), by = "ola") %>%
  mutate(diferencia = pct - nacional,
         fila = paste0(variable, ": ", categoria))

calor$fila <- factor(calor$fila, levels = rev(unique(calor$fila)))

f3 <- ggplot(calor, aes(x = factor(ola), y = fila, fill = diferencia)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(diferencia)), size = 3) +
  scale_fill_gradient2(low = "firebrick", mid = "white", high = "steelblue", midpoint = 0) +
  labs(title = "Diferencia de cada grupo con el promedio nacional de su ola",
       subtitle = "Puntos porcentuales. Azul: más satisfecho que el promedio; rojo: menos satisfecho.",
       x = NULL, y = NULL, fill = "Diferencia", caption = fuente) +
  theme_minimal() +
  theme(panel.grid = element_blank())

ggsave("output/figures/f3_mapa_calor.png", f3, width = 9, height = 8, bg = "white")