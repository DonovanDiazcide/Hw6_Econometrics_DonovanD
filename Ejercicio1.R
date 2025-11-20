# =============================================================================
# EJERCICIO 1: ESPECIFICACIÓN Y ANÁLISIS DE REGRESIÓN - MICHIGAN SCHOOLS
# =============================================================================
# 
# CONTEXTO: Queremos estimar el efecto del gasto por estudiante (expend) 
# sobre el desempeño en matemáticas (math10) en escuelas de Michigan
#
# MODELO TEÓRICO:
# math10 = β0 + β1*log(expend) + β2*log(enroll) + β3*poverty + u
# =============================================================================

# Cargar librerías necesarias
library(tidyverse)
library(wooldridge)  # Contiene los datos que necesitamos
library(broom)       # Para presentar resultados de forma ordenada
library(modelsummary)# Para tablas profesionales
library(car)         # Para pruebas de especificación

# Cargar datos
data("meap93")  # Datos de Michigan Educational Assessment Program 1993
head(meap93)    # Ver primeras observaciones
summary(meap93)
nrow(meap93)

meap93 %>% count()



# =============================================================================
# PREGUNTA i: ¿Por qué lnchprg es una proxy razonable para poverty?
# =============================================================================

# CONCEPTO CLAVE: Variable Proxy
# Una variable proxy es aquella que usamos cuando no podemos observar directamente
# la variable de interés. Para ser una buena proxy debe estar:
# 1. Correlacionada con la variable no observada (poverty)
# 2. Capturar la misma información relevante para nuestro modelo

# Exploremos la relación entre lnchprg y poverty (si tuviéramos ambas)
summary(meap93$lnchprg)  # Porcentaje elegible para comidas subsidiadas

# Crear visualización para entender el concepto
ggplot(meap93, aes(x = lnchprg)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  labs(
    title = "Distribución de estudiantes elegibles para programa de comidas",
    subtitle = "lnchprg como proxy de pobreza",
    x = "Porcentaje de estudiantes en programa de comidas subsidiadas",
    y = "Frecuencia"
  ) +
  theme_minimal()

# aquí va el código de latex - RESPUESTA i

# =============================================================================
# PREGUNTA ii: INTERPRETACIÓN DE RESULTADOS DE REGRESIÓN
# =============================================================================

# Vamos a replicar los dos modelos de la tabla:
# Modelo (1): SIN la variable proxy lnchprg
# Modelo (2): CON la variable proxy lnchprg

# --- MODELO 1: Sin variable proxy ---
modelo1 <- lm(math10 ~ log(expend) + log(enroll), data = meap93)

# --- MODELO 2: Con variable proxy ---
modelo2 <- lm(math10 ~ log(expend) + log(enroll) + lnchprg, data = meap93)

# Presentar resultados en tabla profesional
modelsummary(
  list("Modelo (1)" = modelo1, "Modelo (2)" = modelo2),
  stars = TRUE,
  gof_map = c("nobs", "r.squared"),
  title = "Efecto del gasto en desempeño matemático - Michigan Schools"
)

# Extraer información clave para análisis
summary(modelo1)
summary(modelo2)

# =============================================================================
# PREGUNTA ii.a: ¿Por qué el efecto de expend es menor en columna (2)?
#                ¿Sigue siendo estadísticamente significativo?
# =============================================================================

# Comparar coeficientes
cat("\n--- COMPARACIÓN DE COEFICIENTES DE log(expend) ---\n")
cat("Modelo 1 (sin lnchprg):", coef(modelo1)["log(expend)"], "\n")
cat("Modelo 2 (con lnchprg):", coef(modelo2)["log(expend)"], "\n")
cat("Reducción:", coef(modelo1)["log(expend)"] - coef(modelo2)["log(expend)"], "\n")

# Prueba t para significancia estadística en Modelo 2
t_stat <- summary(modelo2)$coefficients["log(expend)", "t value"]
p_value <- summary(modelo2)$coefficients["log(expend)", "Pr(>|t|)"]

cat("\n--- PRUEBA DE SIGNIFICANCIA EN MODELO 2 ---\n")
cat("Estadístico t:", t_stat, "\n")
cat("P-value:", p_value, "\n")
cat("¿Significativo al 5%?:", ifelse(p_value < 0.05, "SÍ", "NO"), "\n")

# aquí va el código de latex - RESPUESTA ii.a

# =============================================================================
# PREGUNTA ii.b: ¿Por qué cambia el signo de log(enroll)?
# =============================================================================

# Comparar coeficientes de enrollment
cat("\n--- COMPARACIÓN DE COEFICIENTES DE log(enroll) ---\n")
cat("Modelo 1:", coef(modelo1)["log(enroll)"], "\n")
cat("Modelo 2:", coef(modelo2)["log(enroll)"], "\n")

# Analizar correlación entre variables (problema de sesgo por variable omitida)
cor_matrix <- cor(meap93[, c("lexpend", "lenroll", "lnchprg")], 
                  use = "complete.obs")
print(cor_matrix)

# Visualizar relaciones
meap93 %>%
  mutate(
    lexpend = log(expend),
    lenroll = log(enroll)
  ) %>%
  select(lexpend, lenroll, lnchprg) %>%
  pairs(main = "Matriz de correlaciones entre variables")

# aquí va el código de latex - RESPUESTA ii.b

# =============================================================================
# PREGUNTA ii.c: Interpretación del coeficiente de log(enroll) en Modelo 2
# =============================================================================

# El coeficiente es negativo (-1.26): ¿qué significa?
cat("\n--- INTERPRETACIÓN DE log(enroll) EN MODELO 2 ---\n")
cat("Coeficiente:", coef(modelo2)["log(enroll)"], "\n")
cat("Un aumento de 1% en matrícula disminuye math10 en:", 
    coef(modelo2)["log(enroll)"]/100, "puntos\n")

# aquí va el código de latex - RESPUESTA ii.c

# =============================================================================
# PREGUNTA ii.d: Interpretación del coeficiente de lnchprg
# =============================================================================

cat("\n--- INTERPRETACIÓN DE lnchprg EN MODELO 2 ---\n")
cat("Coeficiente:", coef(modelo2)["lnchprg"], "\n")
cat("Un aumento de 1 punto porcentual en lnchprg disminuye math10 en:", 
    abs(coef(modelo2)["lnchprg"]), "puntos\n")

# aquí va el código de latex - RESPUESTA ii.d

# =============================================================================
# PREGUNTA ii.e: Incremento sustancial en R²
# =============================================================================

r2_modelo1 <- summary(modelo1)$r.squared
r2_modelo2 <- summary(modelo2)$r.squared

cat("\n--- COMPARACIÓN DE R² ---\n")
cat("R² Modelo 1:", r2_modelo1, "\n")
cat("R² Modelo 2:", r2_modelo2, "\n")
cat("Incremento absoluto:", r2_modelo2 - r2_modelo1, "\n")
cat("Incremento relativo:", ((r2_modelo2 - r2_modelo1)/r2_modelo1)*100, "%\n")

# aquí va el código de latex - RESPUESTA ii.e

# =============================================================================
# PREGUNTA iii: PRUEBA DE ESPECIFICACIÓN - Forma Funcional
# =============================================================================

# CONTEXTO: Queremos comparar dos especificaciones:
# Especificación A (actual): log(expend) y log(enroll)
# Especificación B (alternativa): expend, expend², enroll, enroll²

# --- Especificación B: Niveles y cuadrados ---
modelo_niveles <- lm(math10 ~ expend + I(expend^2) + enroll + I(enroll^2), 
                     data = meap93)

# MÉTODO 1: PRUEBA DE DAVIDSON-MACKINNON (J-Test)
# Paso 1: Obtener valores predichos de cada modelo
meap93$yhat_log <- fitted(modelo2)      # Predicciones modelo logarítmico
meap93$yhat_nivel <- fitted(modelo_niveles)  # Predicciones modelo niveles

# Paso 2: Agregar predicciones del otro modelo como regresor
# Prueba H0: Especificación logarítmica es correcta
test_log <- lm(math10 ~ log(expend) + log(enroll) + lnchprg + yhat_nivel, 
               data = meap93)

# Prueba H0: Especificación de niveles es correcta  
test_nivel <- lm(math10 ~ expend + I(expend^2) + enroll + I(enroll^2) + 
                   lnchprg + yhat_log, data = meap93)

cat("\n--- PRUEBA DE DAVIDSON-MACKINNON (J-TEST) ---\n")
cat("\nModelo con logs + predicciones de niveles:\n")
summary(test_log)$coefficients["yhat_nivel", ]

cat("\nModelo con niveles + predicciones de logs:\n")
summary(test_nivel)$coefficients["yhat_log", ]

# MÉTODO 2: CRITERIOS DE INFORMACIÓN (AIC, BIC)
cat("\n--- CRITERIOS DE INFORMACIÓN ---\n")
cat("Modelo Logarítmico:\n")
cat("  AIC:", AIC(modelo2), "\n")
cat("  BIC:", BIC(modelo2), "\n")
cat("\nModelo Niveles:\n")
cat("  AIC:", AIC(modelo_niveles), "\n")
cat("  BIC:", BIC(modelo_niveles), "\n")
cat("\n¿Cuál es mejor? El que tenga MENOR AIC/BIC\n")

# MÉTODO 3: TEST DE RESET (Ramsey)
cat("\n--- TEST DE RESET (Ramsey) ---\n")
reset_log <- resettest(modelo2, power = 2:3, type = "fitted")
print(reset_log)

reset_nivel <- resettest(modelo_niveles, power = 2:3, type = "fitted")
print(reset_nivel)

# MÉTODO 4: Validación cruzada simple
set.seed(123)
# Dividir datos en entrenamiento (70%) y prueba (30%)
n <- nrow(meap93)
train_idx <- sample(1:n, size = 0.7*n)

# Entrenar modelos
modelo2_train <- lm(math10 ~ log(expend) + log(enroll) + lnchprg, 
                    data = meap93[train_idx, ])
modelo_niveles_train <- lm(math10 ~ expend + I(expend^2) + enroll + I(enroll^2), 
                           data = meap93[train_idx, ])

# Predecir en datos de prueba
pred_log <- predict(modelo2_train, newdata = meap93[-train_idx, ])
pred_nivel <- predict(modelo_niveles_train, newdata = meap93[-train_idx, ])

# Calcular RMSE (Root Mean Squared Error)
rmse_log <- sqrt(mean((meap93$math10[-train_idx] - pred_log)^2, na.rm = TRUE))
rmse_nivel <- sqrt(mean((meap93$math10[-train_idx] - pred_nivel)^2, na.rm = TRUE))

cat("\n--- VALIDACIÓN CRUZADA ---\n")
cat("RMSE Modelo Logarítmico:", rmse_log, "\n")
cat("RMSE Modelo Niveles:", rmse_nivel, "\n")
cat("Mejor modelo (menor RMSE):", 
    ifelse(rmse_log < rmse_nivel, "Logarítmico", "Niveles"), "\n")

# aquí va el código de latex - RESPUESTA iii

# =============================================================================
# RESUMEN Y CONCLUSIONES
# =============================================================================

cat("\n" , "="*80, "\n")
cat("RESUMEN DE HALLAZGOS:\n")
cat("="*80, "\n")
cat("1. lnchprg es una buena proxy: correlacionada con pobreza\n")
cat("2. Incluir lnchprg reduce sesgo de variable omitida\n")
cat("3. R² aumenta sustancialmente: mejor ajuste del modelo\n")
cat("4. Especificación logarítmica parece preferible\n")
cat("="*80, "\n")