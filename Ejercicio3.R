# =============================================================================
# EJERCICIO 3: COMPARACIÓN EXPERIMENTAL VS OBSERVACIONAL
# =============================================================================
#
# CONTEXTO: Queremos evaluar el efecto de programas de capacitación laboral
# (job training) sobre los ingresos reales (real earnings).
#
# Tenemos DOS conjuntos de datos:
# - JTRAIN2: Datos EXPERIMENTALES (asignación aleatoria al tratamiento)
# - JTRAIN3: Datos OBSERVACIONALES (auto-selección en el tratamiento)
#
# PREGUNTA DE INVESTIGACIÓN: ¿Aumenta la capacitación laboral los ingresos?
# =============================================================================

# Cargar librerías necesarias
library(tidyverse)
library(wooldridge)   # Contiene los datasets
library(modelsummary) # Para tablas profesionales
library(ggplot2)
library(gridExtra)
library(knitr)
library(broom)
library(lmtest)
library(sandwich)

# Limpiar entorno
rm(list = ls())

# =============================================================================
# INTRODUCCIÓN CONCEPTUAL: Experimentos vs Observación
# =============================================================================

cat("="*80, "\n")
cat("DISEÑO EXPERIMENTAL VS OBSERVACIONAL\n")
cat("="*80, "\n\n")

cat("DATOS EXPERIMENTALES (JTRAIN2):\n")
cat("- Investigadores ASIGNAN aleatoriamente el tratamiento (training)\n")
cat("- Similar a un ensayo clínico controlado\n")
cat("- Garantiza que grupos tratados y control sean comparables\n")
cat("- Alta validez interna (inferencia causal confiable)\n\n")

cat("DATOS OBSERVACIONALES (JTRAIN3):\n")
cat("- Individuos ELIGEN si participar o no (auto-selección)\n")
cat("- Refleja el mundo real\n")
cat("- Grupos tratados y control pueden diferir sistemáticamente\n")
cat("- Sesgo de selección potencial\n")
cat("- Mayor validez externa pero menor interna\n\n")

# =============================================================================
# CARGAR Y EXPLORAR LOS DATOS
# =============================================================================

cat("="*80, "\n")
cat("CARGANDO LOS DATOS\n")
cat("="*80, "\n\n")

# Cargar datasets
data("jtrain2")  # Datos experimentales
data("jtrain3")  # Datos observacionales

cat("Dimensiones de JTRAIN2 (experimental):", nrow(jtrain2), "x", ncol(jtrain2), "\n")
cat("Dimensiones de JTRAIN3 (observacional):", nrow(jtrain3), "x", ncol(jtrain3), "\n\n")

# Explorar variables
cat("Variables en JTRAIN2:\n")
print(names(jtrain2))
cat("\n")

cat("Variables clave:\n")
cat("- train: 1 si recibió capacitación, 0 si no\n")
cat("- re74: Ingresos reales en 1974 (pre-tratamiento)\n")
cat("- re75: Ingresos reales en 1975 (pre-tratamiento)\n")
cat("- re78: Ingresos reales en 1978 (post-tratamiento) - OUTCOME\n")
cat("- educ: Años de educación\n")
cat("- age: Edad\n")
cat("- black: 1 si es afroamericano\n")
cat("- hisp: 1 si es hispano\n\n")

# =============================================================================
# PREGUNTA a: Fracción que recibió capacitación en cada dataset
# =============================================================================

cat("\n", "="*80, "\n")
cat("PREGUNTA a: Fracción de participantes en cada dataset\n")
cat("="*80, "\n\n")

# Calcular proporciones
prop_jtrain2 <- mean(jtrain2$train)
prop_jtrain3 <- mean(jtrain3$train)

cat("JTRAIN2 (Experimental):\n")
cat("  Participantes (train=1):", sum(jtrain2$train), "\n")
cat("  No participantes (train=0):", sum(1-jtrain2$train), "\n")
cat("  Fracción tratada:", round(prop_jtrain2, 4), 
    "(", round(prop_jtrain2*100, 2), "%)\n\n")

cat("JTRAIN3 (Observacional):\n")
cat("  Participantes (train=1):", sum(jtrain3$train), "\n")
cat("  No participantes (train=0):", sum(1-jtrain3$train), "\n")
cat("  Fracción tratada:", round(prop_jtrain3, 4), 
    "(", round(prop_jtrain3*100, 2), "%)\n\n")

# Visualización
df_prop <- data.frame(
  Dataset = c("JTRAIN2\n(Experimental)", "JTRAIN3\n(Observacional)"),
  Proporcion = c(prop_jtrain2, prop_jtrain3),
  Tipo = c("Experimental", "Observacional")
)

p1 <- ggplot(df_prop, aes(x = Dataset, y = Proporcion, fill = Tipo)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(round(Proporcion*100, 1), "%")), 
            vjust = -0.5, size = 5) +
  scale_y_continuous(limits = c(0, 0.5), labels = scales::percent) +
  scale_fill_manual(values = c("Experimental" = "steelblue", 
                               "Observacional" = "coral")) +
  labs(
    title = "Proporción de participantes en capacitación",
    subtitle = "¿Por qué hay tanta diferencia?",
    y = "Proporción tratada",
    x = ""
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14))

print(p1)

cat("\n¿POR QUÉ HAY UNA DIFERENCIA TAN GRANDE?\n")
cat("-"*60, "\n\n")

# aquí va el código de latex - RESPUESTA a

# =============================================================================
# PREGUNTA b: Regresión simple con datos experimentales (JTRAIN2)
# =============================================================================

cat("\n", "="*80, "\n")
cat("PREGUNTA b: Regresión simple - JTRAIN2 (Experimental)\n")
cat("="*80, "\n\n")

cat("MODELO: re78 = β₀ + β₁*train + u\n\n")

# Estimar modelo simple
modelo_b <- lm(re78 ~ train, data = jtrain2)

# Resumen
summary(modelo_b)

# Extraer información clave
coef_train_b <- coef(modelo_b)["train"]
se_train_b <- summary(modelo_b)$coefficients["train", "Std. Error"]
t_stat_b <- summary(modelo_b)$coefficients["train", "t value"]
p_value_b <- summary(modelo_b)$coefficients["train", "Pr(>|t|)"]

cat("\n" ,"="*60, "\n")
cat("INTERPRETACIÓN:\n")
cat("="*60, "\n")
cat("Coeficiente de train (β̂₁):", round(coef_train_b, 2), "\n")
cat("Error estándar:", round(se_train_b, 2), "\n")
cat("Estadístico t:", round(t_stat_b, 3), "\n")
cat("P-value:", round(p_value_b, 4), "\n\n")

if(p_value_b < 0.05) {
  cat("✓ El efecto es estadísticamente significativo al 5%\n")
} else {
  cat("✗ El efecto NO es estadísticamente significativo al 5%\n")
}

cat("\nEFECTO CAUSAL: Participar en capacitación laboral aumenta\n")
cat("los ingresos en $", round(coef_train_b, 2), " en promedio.\n\n")

# Visualización de medias por grupo
medias_b <- jtrain2 %>%
  group_by(train) %>%
  summarise(
    media_re78 = mean(re78),
    se = sd(re78)/sqrt(n()),
    n = n()
  ) %>%
  mutate(grupo = ifelse(train == 1, "Tratados\n(train=1)", "Control\n(train=0)"))

p2 <- ggplot(medias_b, aes(x = grupo, y = media_re78, fill = grupo)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(aes(ymin = media_re78 - 1.96*se, 
                    ymax = media_re78 + 1.96*se), 
                width = 0.2) +
  geom_text(aes(label = paste0("$", round(media_re78, 0))), 
            vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("steelblue", "coral")) +
  labs(
    title = "Ingresos promedio en 1978 - JTRAIN2 (Experimental)",
    subtitle = "Barras de error = IC 95%",
    y = "Ingresos reales en 1978 ($)",
    x = ""
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(p2)

# aquí va el código de latex - RESPUESTA b

# =============================================================================
# PREGUNTA c: Agregar controles - JTRAIN2 (Experimental)
# =============================================================================

cat("\n\n", "="*80, "\n")
cat("PREGUNTA c: Regresión con controles - JTRAIN2 (Experimental)\n")
cat("="*80, "\n\n")

cat("MODELO: re78 = β₀ + β₁*train + β₂*re74 + β₃*re75 + β₄*educ + \n")
cat("               β₅*age + β₆*black + β₇*hisp + u\n\n")

# Estimar modelo con controles
modelo_c <- lm(re78 ~ train + re74 + re75 + educ + age + black + hisp, 
               data = jtrain2)

# Resumen
summary(modelo_c)

# Extraer coeficiente de train
coef_train_c <- coef(modelo_c)["train"]
se_train_c <- summary(modelo_c)$coefficients["train", "Std. Error"]
t_stat_c <- summary(modelo_c)$coefficients["train", "t value"]
p_value_c <- summary(modelo_c)$coefficients["train", "Pr(>|t|)"]

cat("\n", "="*60, "\n")
cat("COMPARACIÓN DE COEFICIENTES:\n")
cat("="*60, "\n")
cat("Sin controles (modelo b):", round(coef_train_b, 2), "\n")
cat("Con controles (modelo c):", round(coef_train_c, 2), "\n")
cat("Diferencia:", round(coef_train_c - coef_train_b, 2), "\n")
cat("Cambio relativo:", round((coef_train_c - coef_train_b)/coef_train_b * 100, 2), "%\n\n")

# Tabla comparativa
modelsummary(
  list("Sin controles" = modelo_b, "Con controles" = modelo_c),
  stars = TRUE,
  coef_rename = c(
    "train" = "Capacitación (train)",
    "re74" = "Ingresos 1974",
    "re75" = "Ingresos 1975",
    "educ" = "Educación",
    "age" = "Edad",
    "black" = "Afroamericano",
    "hisp" = "Hispano"
  ),
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  title = "Efecto de capacitación - JTRAIN2 (Experimental)"
)

cat("\n¿CAMBIA MUCHO EL EFECTO? ¿POR QUÉ?\n")
cat("-"*60, "\n")

# aquí va el código de latex - RESPUESTA c

# =============================================================================
# PREGUNTA d: Regresiones con JTRAIN3 (Observacional)
# =============================================================================

cat("\n\n", "="*80, "\n")
cat("PREGUNTA d: Regresiones - JTRAIN3 (Observacional)\n")
cat("="*80, "\n\n")

# Modelo simple (sin controles)
modelo_d_simple <- lm(re78 ~ train, data = jtrain3)

# Modelo con controles
modelo_d_controles <- lm(re78 ~ train + re74 + re75 + educ + age + black + hisp, 
                         data = jtrain3)

# Extraer coeficientes
coef_train_d_simple <- coef(modelo_d_simple)["train"]
se_train_d_simple <- summary(modelo_d_simple)$coefficients["train", "Std. Error"]
t_stat_d_simple <- summary(modelo_d_simple)$coefficients["train", "t value"]

coef_train_d_controles <- coef(modelo_d_controles)["train"]
se_train_d_controles <- summary(modelo_d_controles)$coefficients["train", "Std. Error"]
t_stat_d_controles <- summary(modelo_d_controles)$coefficients["train", "t value"]

cat("RESULTADOS JTRAIN3 (Observacional):\n")
cat("="*60, "\n\n")

cat("Sin controles:\n")
cat("  β̂_train =", round(coef_train_d_simple, 2), "\n")
cat("  SE =", round(se_train_d_simple, 2), "\n")
cat("  t-stat =", round(t_stat_d_simple, 3), "\n\n")

cat("Con controles:\n")
cat("  β̂_train =", round(coef_train_d_controles, 2), "\n")
cat("  SE =", round(se_train_d_controles, 2), "\n")
cat("  t-stat =", round(t_stat_d_controles, 3), "\n\n")

# Tabla comparativa JTRAIN3
modelsummary(
  list("Sin controles" = modelo_d_simple, "Con controles" = modelo_d_controles),
  stars = TRUE,
  coef_rename = c(
    "train" = "Capacitación (train)",
    "re74" = "Ingresos 1974",
    "re75" = "Ingresos 1975",
    "educ" = "Educación",
    "age" = "Edad",
    "black" = "Afroamericano",
    "hisp" = "Hispano"
  ),
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  title = "Efecto de capacitación - JTRAIN3 (Observacional)"
)

cat("\n", "="*60, "\n")
cat("COMPARACIÓN: ¿Qué pasa al agregar controles en JTRAIN3?\n")
cat("="*60, "\n")
cat("Cambio absoluto:", round(coef_train_d_controles - coef_train_d_simple, 2), "\n")
cat("Cambio relativo:", round((coef_train_d_controles - coef_train_d_simple)/coef_train_d_simple * 100, 2), "%\n\n")

# aquí va el código de latex - RESPUESTA d

# =============================================================================
# CUADRO COMPARATIVO: Experimental vs Observacional
# =============================================================================

cat("\n\n", "="*80, "\n")
cat("CUADRO RESUMEN: EXPERIMENTAL VS OBSERVACIONAL\n")
cat("="*80, "\n\n")

# Crear dataframe comparativo
comparacion <- data.frame(
  Especificación = c("Sin controles", "Con controles"),
  JTRAIN2_coef = c(coef_train_b, coef_train_c),
  JTRAIN2_tstat = c(t_stat_b, t_stat_c),
  JTRAIN3_coef = c(coef_train_d_simple, coef_train_d_controles),
  JTRAIN3_tstat = c(t_stat_d_simple, t_stat_d_controles)
)

print(kable(comparacion, digits = 2, format = "simple",
            col.names = c("Especificación", "Coef.", "t-stat", "Coef.", "t-stat")))

# Visualización comparativa
df_plot <- data.frame(
  Dataset = rep(c("JTRAIN2\n(Experimental)", "JTRAIN3\n(Observacional)"), each = 2),
  Modelo = rep(c("Sin controles", "Con controles"), 2),
  Coeficiente = c(coef_train_b, coef_train_c, 
                  coef_train_d_simple, coef_train_d_controles),
  SE = c(se_train_b, se_train_c, 
         se_train_d_simple, se_train_d_controles)
)

p3 <- ggplot(df_plot, aes(x = Modelo, y = Coeficiente, fill = Dataset)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = Coeficiente - 1.96*SE, 
                    ymax = Coeficiente + 1.96*SE),
                position = position_dodge(width = 0.8),
                width = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_fill_manual(values = c("steelblue", "coral")) +
  labs(
    title = "Efecto estimado de capacitación: Experimental vs Observacional",
    subtitle = "Barras de error = IC 95%",
    y = "Efecto sobre ingresos 1978 ($)",
    x = "Especificación del modelo",
    fill = "Tipo de datos"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p3)

# =============================================================================
# PREGUNTA e: Comparar distribuciones de avgre
# =============================================================================

cat("\n\n", "="*80, "\n")
cat("PREGUNTA e: Comparar ingresos pre-tratamiento (avgre)\n")
cat("="*80, "\n\n")

cat("Definimos: avgre = (re74 + re75) / 2\n\n")

# Crear variable avgre en ambos datasets
jtrain2 <- jtrain2 %>%
  mutate(avgre = (re74 + re75) / 2)

jtrain3 <- jtrain3 %>%
  mutate(avgre = (re74 + re75) / 2)

# Estadísticas descriptivas
cat("ESTADÍSTICAS DESCRIPTIVAS - JTRAIN2 (Experimental):\n")
cat("-"*60, "\n")
stats_j2 <- jtrain2 %>%
  summarise(
    Media = mean(avgre),
    SD = sd(avgre),
    Mínimo = min(avgre),
    Máximo = max(avgre),
    Q1 = quantile(avgre, 0.25),
    Mediana = median(avgre),
    Q3 = quantile(avgre, 0.75)
  )
print(kable(stats_j2, digits = 2, format = "simple"))

cat("\nESTADÍSTICAS DESCRIPTIVAS - JTRAIN3 (Observacional):\n")
cat("-"*60, "\n")
stats_j3 <- jtrain3 %>%
  summarise(
    Media = mean(avgre),
    SD = sd(avgre),
    Mínimo = min(avgre),
    Máximo = max(avgre),
    Q1 = quantile(avgre, 0.25),
    Mediana = median(avgre),
    Q3 = quantile(avgre, 0.75)
  )
print(kable(stats_j3, digits = 2, format = "simple"))

# Combinar para visualización
df_avgre <- bind_rows(
  jtrain2 %>% mutate(Dataset = "JTRAIN2 (Experimental)"),
  jtrain3 %>% mutate(Dataset = "JTRAIN3 (Observacional)")
)

# Gráficos comparativos
p4 <- ggplot(df_avgre, aes(x = avgre, fill = Dataset)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 40) +
  scale_fill_manual(values = c("steelblue", "coral")) +
  labs(
    title = "Distribución de ingresos promedio pre-tratamiento",
    subtitle = "avgre = (ingresos1974 + ingresos1975) / 2",
    x = "Ingresos promedio 1974-1975 ($)",
    y = "Frecuencia"
  ) +
  theme_minimal()

p5 <- ggplot(df_avgre, aes(x = Dataset, y = avgre, fill = Dataset)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("steelblue", "coral")) +
  labs(
    title = "Comparación de distribuciones (boxplot)",
    y = "avgre ($)",
    x = ""
  ) +
  theme_minimal() +
  theme(legend.position = "none")

grid.arrange(p4, p5, ncol = 2)

cat("\n¿SON REPRESENTATIVOS DE LAS MISMAS POBLACIONES?\n")
cat("-"*60, "\n")

# aquí va el código de latex - RESPUESTA e

# =============================================================================
# PREGUNTA f: Restringir a avgre < 10,000
# =============================================================================

cat("\n\n", "="*80, "\n")
cat("PREGUNTA f: Análisis restringido a avgre < $10,000\n")
cat("="*80, "\n\n")

# Calcular proporción en JTRAIN2
prop_bajo_j2 <- mean(jtrain2$avgre < 10000)
cat("Proporción con avgre < $10,000 en JTRAIN2:", 
    round(prop_bajo_j2, 4), "(", round(prop_bajo_j2*100, 2), "%)\n\n")

# Filtrar datos
jtrain2_sub <- jtrain2 %>% filter(avgre < 10000)
jtrain3_sub <- jtrain3 %>% filter(avgre < 10000)

cat("Observaciones restantes:\n")
cat("  JTRAIN2:", nrow(jtrain2_sub), "de", nrow(jtrain2), "\n")
cat("  JTRAIN3:", nrow(jtrain3_sub), "de", nrow(jtrain3), "\n\n")

# Regresión con controles - JTRAIN2 submuestra
modelo_f_j2 <- lm(re78 ~ train + re74 + re75 + educ + age + black + hisp, 
                  data = jtrain2_sub)

coef_f_j2 <- coef(modelo_f_j2)["train"]
se_f_j2 <- summary(modelo_f_j2)$coefficients["train", "Std. Error"]
t_f_j2 <- summary(modelo_f_j2)$coefficients["train", "t value"]

cat("JTRAIN2 (avgre < $10,000) - Experimental:\n")
cat("  β̂_train =", round(coef_f_j2, 2), "\n")
cat("  SE =", round(se_f_j2, 2), "\n")
cat("  t-stat =", round(t_f_j2, 3), "\n\n")

# Regresión con controles - JTRAIN3 submuestra
modelo_f_j3 <- lm(re78 ~ train + re74 + re75 + educ + age + black + hisp, 
                  data = jtrain3_sub)

coef_f_j3 <- coef(modelo_f_j3)["train"]
se_f_j3 <- summary(modelo_f_j3)$coefficients["train", "Std. Error"]
t_f_j3 <- summary(modelo_f_j3)$coefficients["train", "t value"]

cat("JTRAIN3 (avgre < $10,000) - Observacional:\n")
cat("  β̂_train =", round(coef_f_j3, 2), "\n")
cat("  SE =", round(se_f_j3, 2), "\n")
cat("  t-stat =", round(t_f_j3, 3), "\n\n")

# Tabla comparativa
modelsummary(
  list("JTRAIN2 (Exp)" = modelo_f_j2, "JTRAIN3 (Obs)" = modelo_f_j3),
  stars = TRUE,
  coef_omit = "Intercept",
  gof_map = c("nobs", "r.squared"),
  title = "Efecto de capacitación - Submuestra avgre < $10,000"
)

cat("\nCOMPARACIÓN DE EFECTOS (submuestra bajos ingresos):\n")
cat("="*60, "\n")
cat("JTRAIN2 (Experimental):", round(coef_f_j2, 2), "\n")
cat("JTRAIN3 (Observacional):", round(coef_f_j3, 2), "\n")
cat("Diferencia:", round(coef_f_j2 - coef_f_j3, 2), "\n\n")

# aquí va el código de latex - RESPUESTA f

# =============================================================================
# PREGUNTA g: Desempleados en 1974 y 1975
# =============================================================================

cat("\n\n", "="*80, "\n")
cat("PREGUNTA g: Análisis para desempleados en 1974 y 1975\n")
cat("="*80, "\n\n")

cat("Filtrar: re74 = 0 AND re75 = 0 (desempleados ambos años)\n\n")

# Filtrar desempleados
jtrain2_unemp <- jtrain2 %>% filter(re74 == 0 & re75 == 0)
jtrain3_unemp <- jtrain3 %>% filter(re74 == 0 & re75 == 0)

cat("Observaciones de desempleados:\n")
cat("  JTRAIN2:", nrow(jtrain2_unemp), "de", nrow(jtrain2), 
    "(", round(nrow(jtrain2_unemp)/nrow(jtrain2)*100, 1), "%)\n")
cat("  JTRAIN3:", nrow(jtrain3_unemp), "de", nrow(jtrain3), 
    "(", round(nrow(jtrain3_unemp)/nrow(jtrain3)*100, 1), "%)\n\n")

# Regresión simple - JTRAIN2 desempleados
modelo_g_j2 <- lm(re78 ~ train, data = jtrain2_unemp)

coef_g_j2 <- coef(modelo_g_j2)["train"]
se_g_j2 <- summary(modelo_g_j2)$coefficients["train", "Std. Error"]
t_g_j2 <- summary(modelo_g_j2)$coefficients["train", "t value"]

cat("JTRAIN2 (desempleados) - Experimental:\n")
cat("  β̂_train =", round(coef_g_j2, 2), "\n")
cat("  SE =", round(se_g_j2, 2), "\n")
cat("  t-stat =", round(t_g_j2, 3), "\n\n")

# Regresión simple - JTRAIN3 desempleados
modelo_g_j3 <- lm(re78 ~ train, data = jtrain3_unemp)

coef_g_j3 <- coef(modelo_g_j3)["train"]
se_g_j3 <- summary(modelo_g_j3)$coefficients["train", "Std. Error"]
t_g_j3 <- summary(modelo_g_j3)$coefficients["train", "t value"]

cat("JTRAIN3 (desempleados) - Observacional:\n")
cat("  β̂_train =", round(coef_g_j3, 2), "\n")
cat("  SE =", round(se_g_j3, 2), "\n")
cat("  t-stat =", round(t_g_j3, 3), "\n\n")

# Tabla comparativa
modelsummary(
  list("JTRAIN2 (Exp)" = modelo_g_j2, "JTRAIN3 (Obs)" = modelo_g_j3),
  stars = TRUE,
  gof_map = c("nobs", "r.squared"),
  title = "Efecto de capacitación - Desempleados en 1974-1975"
)

cat("\nCOMPARACIÓN DE EFECTOS (desempleados):\n")
cat("="*60, "\n")
cat("JTRAIN2 (Experimental):", round(coef_g_j2, 2), "\n")
cat("JTRAIN3 (Observacional):", round(coef_g_j3, 2), "\n")
cat("Diferencia:", round(coef_g_j2 - coef_g_j3, 2), "\n\n")

# aquí va el código de latex - RESPUESTA g

# =============================================================================
# PREGUNTA h: Discusión sobre poblaciones comparables
# =============================================================================

cat("\n\n", "="*80, "\n")
cat("PREGUNTA h: Importancia de poblaciones comparables\n")
cat("="*80, "\n\n")

# Crear tabla resumen de todos los estimadores
tabla_resumen <- data.frame(
  Especificacion = c(
    "Muestra completa (sin controles)",
    "Muestra completa (con controles)",
    "avgre < $10,000",
    "Desempleados 74-75"
  ),
  JTRAIN2 = c(
    round(coef_train_b, 2),
    round(coef_train_c, 2),
    round(coef_f_j2, 2),
    round(coef_g_j2, 2)
  ),
  JTRAIN3 = c(
    round(coef_train_d_simple, 2),
    round(coef_train_d_controles, 2),
    round(coef_f_j3, 2),
    round(coef_g_j3, 2)
  )
)

tabla_resumen <- tabla_resumen %>%
  mutate(Diferencia = JTRAIN2 - JTRAIN3)

print(kable(tabla_resumen, format = "simple",
            col.names = c("Especificación", "JTRAIN2\n(Experimental)", 
                          "JTRAIN3\n(Observacional)", "Diferencia")))

# Visualización final
df_final <- data.frame(
  Especificacion = rep(tabla_resumen$Especificacion, 2),
  Dataset = rep(c("JTRAIN2 (Exp)", "JTRAIN3 (Obs)"), each = 4),
  Efecto = c(tabla_resumen$JTRAIN2, tabla_resumen$JTRAIN3)
)

p6 <- ggplot(df_final, aes(x = Especificacion, y = Efecto, fill = Dataset)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("steelblue", "coral")) +
  coord_flip() +
  labs(
    title = "Convergencia de estimadores con poblaciones comparables",
    subtitle = "¿Qué pasa cuando comparamos 'manzanas con manzanas'?",
    x = "",
    y = "Efecto estimado ($)",
    fill = ""
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p6)

# aquí va el código de latex - RESPUESTA h

# =============================================================================
# CONCLUSIONES Y LECCIONES
# =============================================================================

cat("\n\n", "="*80, "\n")
cat("CONCLUSIONES PRINCIPALES\n")
cat("="*80, "\n\n")

cat("1. DISEÑO EXPERIMENTAL (JTRAIN2):\n")
cat("   - Estimadores ROBUSTOS a la inclusión de controles\n")
cat("   - Aleatorización garantiza balance en observables y no observables\n")
cat("   - Alta validez interna\n\n")

cat("2. DISEÑO OBSERVACIONAL (JTRAIN3):\n")
cat("   - Estimadores MUY SENSIBLES a la inclusión de controles\n")
cat("   - Sesgo de selección severo\n")
cat("   - Poblaciones no comparables en la muestra completa\n\n")

cat("3. IMPORTANCIA DE POBLACIONES COMPARABLES:\n")
cat("   - Cuando restringimos a poblaciones similares, las estimaciones convergen\n")
cat("   - avgre < $10,000: diferencia se reduce\n")
cat("   - Desempleados: estimaciones muy similares\n")
cat("   - Validez externa vs validez interna trade-off\n\n")

cat("4. LECCIONES:\n")
cat("   ✓ Experimentos: 'Gold standard' para inferencia causal\n")
cat("   ✓ Observación: Requiere supuestos fuertes y población comparable\n")
cat("   ✓ Controles ayudan pero no eliminan completamente el sesgo\n")
cat("   ✓ Matching/restricción puede mejorar comparabilidad\n\n")

cat("="*80, "\n")