library(dplyr)
library(ggplot2)
library(survey)

set.seed(2026)

modelos <- readRDS("output/tables/modelos.rds")

dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

# Probabilidades predichas
predecir <- function(m, var) {
  sims <- rbind(coef(m), MASS::mvrnorm(1000, coef(m), vcov(m)))
  w <- m$prior.weights
  bind_rows(lapply(m$xlevels[[var]], function(niv) {
    datos <- m$model
    datos[[var]] <- factor(niv, levels = m$xlevels[[var]])
    X <- model.matrix(delete.response(terms(m)), datos, xlev = m$xlevels)
    p <- 100 * colSums(w * plogis(X %*% t(sims))) / sum(w)
    data.frame(variable = var, categoria = niv, prob = p[1],
               li = quantile(p[-1], 0.025), ls = quantile(p[-1], 0.975), row.names = NULL)
  }))
}

pred <- bind_rows(predecir(modelos[["2006-2023 con voto"]], "voto"),
                  lapply(c("educ", "ingreso", "edad", "zona", "region", "genero"),
                         function(v) predecir(modelos[["2006-2026"]], v))) %>%
  mutate(across(c(prob, li, ls), ~ round(.x, 1)))

saveRDS(pred, "output/tables/t5_probabilidades.rds")

etiquetas <- c(voto = "Voto", educ = "Educación", ingreso = "Ingreso", edad = "Edad",
               zona = "Zona", region = "Región", genero = "Género")

f4 <- pred %>%
  mutate(variable = factor(etiquetas[variable], levels = etiquetas),
         categoria = factor(categoria, levels = rev(unique(categoria)))) %>%
  ggplot(aes(x = prob, y = categoria)) +
  geom_errorbar(aes(xmin = li, xmax = ls), width = 0.2, colour = "grey40", orientation = "y") +
  geom_point(size = 2, colour = "#1F3A5F") +
  facet_grid(variable ~ ., scales = "free_y", space = "free_y") +
  labs(x = "Probabilidad predicha de estar satisfecho (%)", y = NULL,
       caption = "Fuente: Barómetro de las Américas (LAPOP), Chile 2006-2026. Voto: modelo 2006-2023. IC 95%.") +
  theme_minimal() +
  theme(strip.text.y = element_text(angle = 0, hjust = 0))

ggsave("output/figures/f4_probabilidades.png", f4, width = 7, height = 7.5, dpi = 300)

# Razones de odds por período
nombres <- c("educMedia" = "Educación: media",
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

coefs <- bind_rows(lapply(c("2006-2014", "2017-2019", "2023-2026"), function(p) {
  m <- modelos[[p]]
  data.frame(periodo = p, termino = names(coef(m)), b = coef(m),
             se = sqrt(diag(vcov(m))), row.names = NULL)
})) %>%
  filter(termino %in% names(nombres)) %>%
  mutate(termino = factor(nombres[termino], levels = rev(nombres)),
         or = exp(b), li = exp(b - 1.96 * se), ls = exp(b + 1.96 * se))

f5 <- ggplot(coefs, aes(x = or, y = termino, colour = periodo)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_errorbar(aes(xmin = li, xmax = ls), width = 0, orientation = "y",
                position = position_dodge(width = 0.6)) +
  geom_point(size = 2, position = position_dodge(width = 0.6)) +
  scale_x_log10() +
  scale_colour_manual(values = c("#9DB4CC", "#4F7CAC", "#1F3A5F")) +
  labs(x = "Razón de odds (escala logarítmica)", y = NULL, colour = NULL,
       caption = "Fuente: Elaboración propia en base a Barómetro de las Américas (LAPOP), Chile 2006-2026. IC 95%.") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("output/figures/f5_coeficientes_periodo.png", f5, width = 7, height = 6, dpi = 300)