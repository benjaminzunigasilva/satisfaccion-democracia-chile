library(survey)

modelos <- readRDS("output/tables/modelos.rds")
periodos <- modelos[c("2006-2014", "2017-2019", "2023-2026")]

wald <- function(patron) {
  k <- grep(patron, names(coef(periodos[[1]])), value = TRUE)
  b <- lapply(periodos, function(m) coef(m)[k])
  v <- lapply(periodos, function(m) vcov(m)[k, k, drop = FALSE])
  d <- c(b[[1]] - b[[3]], b[[2]] - b[[3]])
  s <- rbind(cbind(v[[1]] + v[[3]], v[[3]]),
             cbind(v[[3]], v[[2]] + v[[3]]))
  chi2 <- as.numeric(t(d) %*% solve(s) %*% d)
  gl <- length(d)
  data.frame(chi2 = round(chi2, 2), gl = gl, p = round(pchisq(chi2, gl, lower.tail = FALSE), 3))
}

t_wald <- rbind("Educación" = wald("^educ"),
                "Ingreso" = wald("^ingreso"),
                "Edad" = wald("^edad"),
                "Zona" = wald("^zona"),
                "Región Metropolitana" = wald("^region"),
                "Género" = wald("^genero"))

saveRDS(t_wald, "output/tables/t4_wald.rds")
t_wald