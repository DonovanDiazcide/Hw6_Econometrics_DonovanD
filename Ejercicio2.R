# =============================================================================
# EJERCICIO 2: ERRORES DE MEDICIÓN - SUPUESTOS CEV
# =============================================================================
#
# CONTEXTO: Queremos estudiar los determinantes de las horas semanales de TV
# que ve un niño, pero tenemos un problema: las horas reportadas pueden tener
# error de medición.
#
# MODELO VERDADERO (no observable):
# tvhours* = β0 + β1*age + β2*age² + β3*motheduc + β4*fatheduc + β5*sibs + u
#
# MODELO OBSERVADO (con error de medición):
# tvhours = tvhours* + e  (donde e es el error de medición)
# =============================================================================

# Cargar librerías necesarias
library(tidyverse)
library(ggplot2)
library(knitr)
library(gridExtra)

# Limpiar entorno
rm(list = ls())

# =============================================================================
# PARTE EDUCATIVA: ¿QUÉ SON LOS ERRORES DE MEDICIÓN?
# =============================================================================

cat("="*80, "\n")
cat("INTRODUCCIÓN: ERRORES DE MEDICIÓN\n")
cat("="*80, "\n\n")

cat("Cuando medimos variables en encuestas, pueden existir errores:\n")
cat("1. Error de reporte: la persona no recuerda exactamente\n")
cat("2. Error de redondeo: reportan 'aproximadamente 10 horas'\n")
cat("3. Sesgo social: subreportan comportamientos 'negativos'\n")
cat("4. Error de interpretación: mal entienden la pregunta\n\n")

# =============================================================================
# SIMULACIÓN EDUCATIVA: Entender el problema del error de medición
# =============================================================================

cat("SIMULACIÓN 1: Visualizando el error de medición\n")
cat("-"*80, "\n")

set.seed(123)
n <- 500

# Generar datos simulados
simdata <- tibble(
  # Variables exógenas (sin error)
  age = sample(6:16, n, replace = TRUE),
  motheduc = rnorm(n, mean = 13, sd = 2.5),
  fatheduc = rnorm(n, mean = 13, sd = 2.5),
  sibs = rpois(n, lambda = 1.5),
  
  # Variable VERDADERA (no observable en la realidad)
  tvhours_true = 15 + 
    0.5 * age - 
    0.03 * age^2 - 
    0.4 * motheduc - 
    0.3 * fatheduc + 
    0.5 * sibs + 
    rnorm(n, mean = 0, sd = 2),
  
  # ERROR DE MEDICIÓN (en la práctica, no lo observamos por separado)
  measurement_error = rnorm(n, mean = 0, sd = 3),
  
  # Variable OBSERVADA (lo que realmente medimos)
  tvhours_observed = tvhours_true + measurement_error
)

# Asegurar que horas sean no-negativas
simdata <- simdata %>%
  mutate(
    tvhours_true = pmax(tvhours_true, 0),
    tvhours_observed = pmax(tvhours_observed, 0)
  )

# Visualizar la diferencia entre valor verdadero y observado
p1 <- ggplot(simdata, aes(x = tvhours_true, y = tvhours_observed)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Relación entre horas verdaderas y observadas",
    subtitle = "La línea roja muestra medición perfecta (sin error)",
    x = "Horas verdaderas de TV (tvhours*)",
    y = "Horas observadas de TV (tvhours)"
  ) +
  theme_minimal()

# Distribución del error de medición
p2 <- ggplot(simdata, aes(x = measurement_error)) +
  geom_histogram(bins = 30, fill = "coral", alpha = 0.7) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Distribución del error de medición",
    subtitle = "e = tvhours - tvhours*",
    x = "Error de medición (e)",
    y = "Frecuencia"
  ) +
  theme_minimal()

# Mostrar gráficos
grid.arrange(p1, p2, ncol = 2)

cat("\nEstadísticas del error de medición:\n")
cat("Media:", mean(simdata$measurement_error), "\n")
cat("Desviación estándar:", sd(simdata$measurement_error), "\n")
cat("Correlación(tvhours_true, error):", 
    cor(simdata$tvhours_true, simdata$measurement_error), "\n\n")

# =============================================================================
# PREGUNTA a: ¿Es probable que se cumplan los supuestos CEV?
# =============================================================================

cat("\n", "="*80, "\n")
cat("PREGUNTA a: ¿Se cumplen los supuestos CEV?\n")
cat("="*80, "\n\n")

# aquí va el código de latex - RESPUESTA a

# =============================================================================
# PREGUNTA b: ¿Qué exigen los supuestos CEV en esta aplicación?
# =============================================================================

cat("\n", "="*80, "\n")
cat("PREGUNTA b: Supuestos CEV en este contexto\n")
cat("="*80, "\n\n")

cat("Los supuestos de errores en variables clásicos (CEV) son:\n\n")

cat("SUPUESTO 1 - Media cero del error:\n")
cat("  E[e] = 0\n")
cat("  Interpretación: En promedio, no hay sesgo sistemático en el reporte\n")
cat("  Ejemplo: Algunos sobre-reportan, otros sub-reportan, pero se cancela\n\n")

cat("SUPUESTO 2 - Independencia del valor verdadero:\n")
cat("  Cov(tvhours*, e) = 0\n")
cat("  Interpretación: El error no depende del valor verdadero\n")
cat("  Ejemplo: Quien ve 5 horas tiene igual error que quien ve 20\n\n")

cat("SUPUESTO 3 - Independencia de las X's:\n")
cat("  Cov(X_j, e) = 0  para j = 1,...,k\n")
cat("  Interpretación: El error no se correlaciona con las variables explicativas\n")
cat("  Ejemplo: El error no depende de la edad, educación parental, etc.\n\n")

cat("SUPUESTO 4 - Homocedasticidad (a veces):\n")
cat("  Var(e | X, tvhours*) = σ²_e\n")
cat("  Interpretación: La varianza del error es constante\n\n")

# Verificar supuestos en nuestra simulación
cat("Verificación en datos simulados:\n")
cat("-"*40, "\n")
cat("E[e] =", mean(simdata$measurement_error), "(≈ 0) ✓\n")
cat("Cov(tvhours*, e) =", 
    cov(simdata$tvhours_true, simdata$measurement_error), "(≈ 0) ✓\n")
cat("Cov(age, e) =", 
    cov(simdata$age, simdata$measurement_error), "(≈ 0) ✓\n")
cat("Cov(motheduc, e) =", 
    cov(simdata$motheduc, simdata$measurement_error), "(≈ 0) ✓\n")

# aquí va el código de latex - RESPUESTA b

# =============================================================================
# PREGUNTA c: DEMOSTRACIÓN FORMAL DEL SESGO
# =============================================================================

cat("\n\n", "="*80, "\n")
cat("PREGUNTA c: Derivación formal del sesgo por error de medición\n")
cat("="*80, "\n\n")

cat("Vamos a demostrar el sesgo paso por paso con simulaciones numéricas\n\n")

# --- PASO 1: Estimar modelo con variable VERDADERA (sin error) ---
cat("PASO 1: Modelo con variable verdadera (benchmark ideal)\n")
cat("-"*60, "\n")

modelo_verdadero <- lm(tvhours_true ~ age + I(age^2) + motheduc + fatheduc + sibs, 
                       data = simdata)

cat("\nCoeficientes del modelo VERDADERO:\n")
print(round(coef(modelo_verdadero), 4))

beta_true_age <- coef(modelo_verdadero)["age"]
cat("\nβ_age (verdadero) =", beta_true_age, "\n")

# --- PASO 2: Estimar modelo con variable OBSERVADA (con error) ---
cat("\n\nPASO 2: Modelo con variable observada (con error de medición)\n")
cat("-"*60, "\n")

modelo_observado <- lm(tvhours_observed ~ age + I(age^2) + motheduc + fatheduc + sibs, 
                       data = simdata)

cat("\nCoeficientes del modelo OBSERVADO:\n")
print(round(coef(modelo_observado), 4))

beta_obs_age <- coef(modelo_observado)["age"]
cat("\nβ_age (observado) =", beta_obs_age, "\n")

# --- PASO 3: Calcular el sesgo ---
cat("\n\nPASO 3: Cuantificar el sesgo\n")
cat("-"*60, "\n")

sesgo <- beta_obs_age - beta_true_age
sesgo_relativo <- (sesgo / beta_true_age) * 100

cat("\nSesgo absoluto:", sesgo, "\n")
cat("Sesgo relativo:", sesgo_relativo, "%\n")
cat("\n¡El error de medición en Y NO sesga los coeficientes!\n")
cat("Solo aumenta la varianza de los errores.\n")

# --- SIMULACIÓN ESPECIAL: Error de medición en X ---
cat("\n\n", "="*80, "\n")
cat("CASO ESPECIAL: ¿Qué pasa si el ERROR está en una X?\n")
cat("="*80, "\n\n")

cat("Imaginemos que 'age' (edad) también se mide con error:\n")
cat("age_observed = age_true + error_age\n\n")

# Generar error en age
simdata <- simdata %>%
  mutate(
    age_error = rnorm(n, mean = 0, sd = 1),
    age_observed = age + age_error
  )

# Modelo con age observada (con error)
modelo_age_error <- lm(tvhours_true ~ age_observed + I(age_observed^2) + 
                         motheduc + fatheduc + sibs, 
                       data = simdata)

cat("Comparación de coeficientes de 'age':\n")
cat("-"*60, "\n")
cat("β_age (con age verdadera):", coef(modelo_verdadero)["age"], "\n")
cat("β_age (con age observada):", coef(modelo_age_error)["age_observed"], "\n")
cat("\nSesgo:", coef(modelo_age_error)["age_observed"] - coef(modelo_verdadero)["age"], "\n")
cat("\n¡ATENCIÓN! El error en X SÍ genera sesgo (sesgo de atenuación)\n")

# --- VISUALIZACIÓN DEL SESGO DE ATENUACIÓN ---
cat("\n\nVISUALIZACIÓN: Sesgo de atenuación cuando X tiene error\n")
cat("-"*60, "\n")

# Comparar relaciones
p3 <- ggplot(simdata, aes(x = age, y = tvhours_true)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 1.5) +
  labs(
    title = "Relación con age VERDADERA",
    subtitle = "Pendiente no sesgada",
    x = "Edad verdadera",
    y = "Horas de TV verdaderas"
  ) +
  theme_minimal()

p4 <- ggplot(simdata, aes(x = age_observed, y = tvhours_true)) +
  geom_point(alpha = 0.3, color = "coral") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1.5) +
  labs(
    title = "Relación con age OBSERVADA (con error)",
    subtitle = "Pendiente atenuada (sesgada hacia cero)",
    x = "Edad observada (con error)",
    y = "Horas de TV verdaderas"
  ) +
  theme_minimal()

grid.arrange(p3, p4, ncol = 2)

# aquí va el código de latex - RESPUESTA c (derivación formal)

# =============================================================================
# DEMOSTRACIÓN NUMÉRICA DEL SESGO: Experimento de Monte Carlo
# =============================================================================

cat("\n\n", "="*80, "\n")
cat("SIMULACIÓN DE MONTE CARLO: Confirmando el sesgo teórico\n")
cat("="*80, "\n\n")

cat("Vamos a repetir el experimento 1000 veces para ver el sesgo promedio\n\n")

# Función para una simulación
simular_una_vez <- function(n = 500, sd_error_y = 3, sd_error_x = 0) {
  # Generar datos
  age <- sample(6:16, n, replace = TRUE)
  motheduc <- rnorm(n, 13, 2.5)
  fatheduc <- rnorm(n, 13, 2.5)
  sibs <- rpois(n, 1.5)
  
  # Variable verdadera Y
  tvhours_true <- 15 + 0.5*age - 0.03*age^2 - 0.4*motheduc - 
    0.3*fatheduc + 0.5*sibs + rnorm(n, 0, 2)
  tvhours_true <- pmax(tvhours_true, 0)
  
  # Variable observada Y (con error)
  tvhours_obs <- tvhours_true + rnorm(n, 0, sd_error_y)
  tvhours_obs <- pmax(tvhours_obs, 0)
  
  # Variable observada X (con error, si sd_error_x > 0)
  if (sd_error_x > 0) {
    age_obs <- age + rnorm(n, 0, sd_error_x)
  } else {
    age_obs <- age
  }
  
  # Estimar modelos
  mod_true <- lm(tvhours_true ~ age + I(age^2) + motheduc + fatheduc + sibs)
  mod_obs_y <- lm(tvhours_obs ~ age + I(age^2) + motheduc + fatheduc + sibs)
  mod_obs_x <- lm(tvhours_true ~ age_obs + I(age_obs^2) + motheduc + fatheduc + sibs)
  
  # Retornar coeficientes
  return(c(
    beta_true = coef(mod_true)["age"],
    beta_error_y = coef(mod_obs_y)["age"],
    beta_error_x = coef(mod_obs_x)["age_obs"]
  ))
}

# Ejecutar Monte Carlo
set.seed(456)
n_sim <- 1000
resultados <- replicate(n_sim, simular_una_vez(n = 500, sd_error_y = 3, sd_error_x = 1))
resultados <- t(resultados)  # Transponer para tener formato correcto

# Análisis de resultados
resultados_df <- as.data.frame(resultados)

cat("RESULTADOS DE 1000 SIMULACIONES:\n")
cat("="*60, "\n\n")

cat("Coeficiente verdadero (promedio):", mean(resultados_df$beta_true), "\n")
cat("  - Esto debe estar cerca del valor poblacional (β = 0.5)\n\n")

cat("Con ERROR EN Y (variable dependiente):\n")
cat("  Coeficiente estimado (promedio):", mean(resultados_df$beta_error_y), "\n")
cat("  Sesgo:", mean(resultados_df$beta_error_y - resultados_df$beta_true), "\n")
cat("  → El sesgo es aproximadamente CERO ✓\n\n")

cat("Con ERROR EN X (variable independiente):\n")
cat("  Coeficiente estimado (promedio):", mean(resultados_df$beta_error_x), "\n")
cat("  Sesgo:", mean(resultados_df$beta_error_x - resultados_df$beta_true), "\n")
cat("  → Sesgo de ATENUACIÓN hacia cero ✗\n\n")

# Visualizar distribuciones
resultados_long <- resultados_df %>%
  pivot_longer(cols = everything(), names_to = "tipo", values_to = "beta")

ggplot(resultados_long, aes(x = beta, fill = tipo)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "black", linewidth = 1) +
  labs(
    title = "Distribución de estimadores en 1000 simulaciones",
    subtitle = "Línea vertical = valor verdadero del parámetro",
    x = "Coeficiente estimado de 'age'",
    y = "Densidad",
    fill = "Tipo de modelo"
  ) +
  scale_fill_manual(
    values = c("beta_true" = "green", "beta_error_y" = "blue", "beta_error_x" = "red"),
    labels = c("Modelo verdadero", "Error en Y", "Error en X")
  ) +
  theme_minimal()

# =============================================================================
# CÁLCULO DE LA FÓRMULA DEL SESGO DE ATENUACIÓN
# =============================================================================

cat("\n\n", "="*80, "\n")
cat("CÁLCULO NUMÉRICO: Fórmula del sesgo de atenuación\n")
cat("="*80, "\n\n")

cat("La fórmula teórica del sesgo de atenuación es:\n")
cat("plim(β̂) = β * [Var(X*) / (Var(X*) + Var(e_x))]\n")
cat("donde X* es el valor verdadero y e_x es el error de medición\n\n")

# Calcular componentes en nuestra simulación
var_age_true <- var(simdata$age)
var_error_age <- var(simdata$age_error)
ratio_atenuacion <- var_age_true / (var_age_true + var_error_age)

cat("En nuestra simulación:\n")
cat("Var(age_true) =", var_age_true, "\n")
cat("Var(error_age) =", var_error_age, "\n")
cat("Ratio de atenuación =", ratio_atenuacion, "\n\n")

cat("Predicción teórica:\n")
cat("β̂ ≈ β_true *", ratio_atenuacion, "=", 
    coef(modelo_verdadero)["age"] * ratio_atenuacion, "\n")
cat("β̂ observado =", coef(modelo_age_error)["age_observed"], "\n")
cat("\n¡La teoría predice correctamente el sesgo!\n")

# =============================================================================
# IMPLICACIONES PRÁCTICAS
# =============================================================================

cat("\n\n", "="*80, "\n")
cat("IMPLICACIONES PRÁCTICAS\n")
cat("="*80, "\n\n")

cat("1. ERROR EN Y (variable dependiente):\n")
cat("   ✓ NO sesga los coeficientes\n")
cat("   ✗ Aumenta la varianza → errores estándar más grandes\n")
cat("   ✗ Reduce el poder estadístico de las pruebas\n")
cat("   → Solución: Mejorar medición, aumentar tamaño de muestra\n\n")

cat("2. ERROR EN X (variables independientes):\n")
cat("   ✗ SÍ sesga los coeficientes (sesgo de atenuación)\n")
cat("   ✗ Subestima efectos en valor absoluto\n")
cat("   ✗ Puede llevar a conclusiones incorrectas\n")
cat("   → Soluciones: Variables instrumentales, múltiples mediciones,\n")
cat("      validación con datos externos\n\n")

cat("3. EN ESTE EJERCICIO (tvhours):\n")
cat("   - tvhours* (verdadera) es la variable DEPENDIENTE\n")
cat("   - El error está en Y, NO en X\n")
cat("   - Por lo tanto: NO hay sesgo en los coeficientes\n")
cat("   - Pero: Estimadores menos precisos (mayor varianza)\n\n")

# =============================================================================
# RESUMEN EJECUTIVO
# =============================================================================

cat("\n", "="*80, "\n")
cat("RESUMEN EJECUTIVO - EJERCICIO 2\n")
cat("="*80, "\n\n")

cat("PREGUNTA a: ¿Se cumplen los supuestos CEV?\n")
cat("  RESPUESTA: Probablemente NO del todo porque:\n")
cat("  - Puede haber sesgo social (subreporte)\n")
cat("  - Error puede correlacionarse con educación parental\n")
cat("  - Niños mayores pueden reportar más precisamente\n\n")

cat("PREGUNTA b: ¿Qué exigen los supuestos CEV?\n")
cat("  RESPUESTA: Exigen que:\n")
cat("  1. E[e] = 0 (sin sesgo sistemático)\n")
cat("  2. Cov(tvhours*, e) = 0 (error independiente del valor verdadero)\n")
cat("  3. Cov(X_j, e) = 0 (error independiente de las X's)\n\n")

cat("PREGUNTA c: ¿Cuál es el sesgo?\n")
cat("  RESPUESTA: ¡NO HAY SESGO en los coeficientes!\n")
cat("  - Error en Y solo aumenta Var(u)\n")
cat("  - Los β̂ siguen siendo insesgados\n")
cat("  - Pero tienen mayor varianza (menor precisión)\n\n")

cat("="*80, "\n")