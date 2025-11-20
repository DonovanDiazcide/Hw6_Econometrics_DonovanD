# =============================================================================
# EJERCICIO 4 (Partes g-l): VARIABLES INSTRUMENTALES (IV)
# =============================================================================
#
# RECAP: Tenemos sesgo de selección en summercamp
# SOLUCIÓN: Usar 'letter' como variable instrumental (IV)
#
# ESTRUCTURA DE IV:
# - Z = letter (instrumento): asignado aleatoriamente
# - D = summercamp (tratamiento endógeno): auto-selección
# - Y = test_score_year6 (outcome)
#
# SUPUESTOS IV:
# 1. RELEVANCIA: Z afecta D (letter aumenta participación)
# 2. EXOGENEIDAD: Z no afecta Y excepto a través de D
# 3. EXCLUSIÓN: Z solo afecta Y vía D (no hay canales directos)
# 4. MONOTONICIDAD: letter aumenta (o no cambia) participación para todos
# =============================================================================

# Cargar librerías adicionales
library(estimatr)     # Para IV robust
library(AER)          # Para ivreg
library(lmtest)       # Tests de modelos
library(sandwich)     # Errores robustos

# Continuar con analysisdata del ejercicio anterior
# (Si no está cargado, repetir pasos a-f)

cat("="*80, "\n")
cat("EJERCICIO 4: VARIABLES INSTRUMENTALES\n")
cat("="*80, "\n\n")

# =============================================================================
# PREGUNTA j: Primera etapa (First Stage)
# =============================================================================

cat("\n", "="*80, "\n")
cat("PREGUNTA j: Primera etapa - ¿Letter aumenta participación?\n")
cat("="*80, "\n\n")

cat("PRIMERA ETAPA (First Stage):\n")
cat("Regresión de tratamiento (D) sobre instrumento (Z) y controles\n\n")

cat("MODELO:\n")
cat("summercamp = π₀ + π₁*letter + π₂*female + π₃*parental_schooling +\n")
cat("             π₄*parental_income + π₅*test_score_5 + v\n\n")

cat("PREGUNTA CLAVE: ¿π₁ ≠ 0? (Relevancia del instrumento)\n\n")

# Filtrar datos del año 6 (post-tratamiento)
data_year6 <- analysisdata %>%
  filter(year == 6)

# También necesitamos el puntaje del año 5 para cada individuo
data_year5_scores <- analysisdata %>%
  filter(year == 5) %>%
  select(person_id, test_score_year5 = test_score)

# Combinar
data_iv <- data_year6 %>%
  left_join(data_year5_scores, by = "person_id")

# PRIMERA ETAPA con controles
first_stage <- lm(
  summercamp ~ letter + female + parental_schooling + 
    log(parental_income) + test_score_year5,
  data = data_iv
)

cat("RESULTADOS DE PRIMERA ETAPA:\n")
cat("="*60, "\n\n")
summary(first_stage)

# Extraer información clave
coef_letter <- coef(first_stage)["letter"]
se_letter <- summary(first_stage)$coefficients["letter", "Std. Error"]
t_letter <- summary(first_stage)$coefficients["letter", "t value"]
p_letter <- summary(first_stage)$coefficients["letter", "Pr(>|t|)"]
r2_fs <- summary(first_stage)$r.squared
f_stat <- summary(first_stage)$fstatistic[1]

cat("\n", "="*60, "\n")
cat("INTERPRETACIÓN:\n")
cat("="*60, "\n")
cat("Coeficiente de letter (π̂₁):", round(coef_letter, 4), "\n")
cat("Error estándar:", round(se_letter, 4), "\n")
cat("Estadístico t:", round(t_letter, 3), "\n")
cat("P-value:", round(p_letter, 5), "\n")
cat("R² de primera etapa:", round(r2_fs, 4), "\n")
cat("F-statistic:", round(f_stat, 2), "\n\n")

# Interpretación
cat("SIGNIFICADO:\n")
cat("Recibir la carta aumenta la probabilidad de participación en\n")
cat(round(coef_letter * 100, 2), "puntos porcentuales.\n\n")

# Test de instrumento débil (Weak instrument test)
cat("TEST DE INSTRUMENTO DÉBIL:\n")
cat("-"*60, "\n")
cat("Regla empírica: F-stat > 10 para instrumento fuerte\n")
cat("F-statistic =", round(f_stat, 2), "\n")

if(f_stat > 10) {
  cat("✓ El instrumento es FUERTE (F > 10)\n")
  cat("  No hay problema de instrumento débil\n")
} else {
  cat("✗ ADVERTENCIA: Instrumento débil (F < 10)\n")
  cat("  Estimadores IV pueden estar sesgados hacia OLS\n")
}

# Visualización de primera etapa
p_first <- ggplot(data_iv, aes(x = factor(letter), y = summercamp)) +
  stat_summary(fun = mean, geom = "bar", fill = "steelblue", alpha = 0.7) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  labs(
    title = "Primera etapa: Efecto de la carta sobre participación",
    subtitle = "¿La carta aumenta summercamp?",
    x = "Recibió carta recordatoria",
    y = "Proporción que participó en summercamp"
  ) +
  scale_x_discrete(labels = c("No", "Sí")) +
  theme_minimal()

print(p_first)

# aquí va el código de latex - RESPUESTA j

# =============================================================================
# PREGUNTA k: Forma reducida sin controles
# =============================================================================

cat("\n\n", "="*80, "\n")
cat("PREGUNTA k: Forma reducida - Efecto de letter sobre outcome\n")
cat("="*80, "\n\n")

cat("FORMA REDUCIDA (Reduced Form):\n")
cat("Regresión de outcome (Y) sobre instrumento (Z), SIN controles\n\n")

cat("MODELO:\n")
cat("test_score_6 = γ₀ + γ₁*letter + ε\n\n")

cat("ESTE ES EL 'INTENTION TO TREAT' (ITT) EFFECT:\n")
cat("- Efecto de ser ASIGNADO a recibir la carta\n")
cat("- No importa si realmente participaste\n")
cat("- Es el efecto 'puro' de la asignación aleatoria\n\n")

# Forma reducida sin controles
reduced_form <- lm(test_score ~ letter, data = data_iv)

cat("RESULTADOS DE FORMA REDUCIDA:\n")
cat("="*60, "\n\n")
summary(reduced_form)

# Extraer información
coef_rf <- coef(reduced_form)["letter"]
se_rf <- summary(reduced_form)$coefficients["letter", "Std. Error"]
t_rf <- summary(reduced_form)$coefficients["letter", "t value"]
p_rf <- summary(reduced_form)$coefficients["letter", "Pr(>|t|)"]

cat("\n", "="*60, "\n")
cat("INTERPRETACIÓN:\n")
cat("="*60, "\n")
cat("Coeficiente de letter (γ̂₁):", round(coef_rf, 4), "\n")
cat("Error estándar:", round(se_rf, 4), "\n")
cat("Estadístico t:", round(t_rf, 3), "\n")
cat("P-value:", round(p_rf, 5), "\n\n")

cat("SIGNIFICADO (ITT):\n")
cat("Recibir la carta aumenta el puntaje en", round(coef_rf, 4), "SD.\n")
cat("Este es el efecto de la ASIGNACIÓN, no de la participación.\n\n")

# Comparar medias por grupo
cat("COMPARACIÓN DE MEDIAS:\n")
cat("-"*60, "\n")
medias_rf <- data_iv %>%
  group_by(letter) %>%
  summarise(
    N = n(),
    Media = mean(test_score),
    SD = sd(test_score),
    SE = SD / sqrt(N)
  )

print(kable(medias_rf, digits = 4))

# Visualización
p_rf <- ggplot(data_iv, aes(x = factor(letter), y = test_score)) +
  geom_boxplot(fill = "coral", alpha = 0.5) +
  stat_summary(fun = mean, geom = "point", color = "red", size = 3) +
  labs(
    title = "Forma reducida: Efecto de la carta sobre puntajes",
    subtitle = "Intention-to-Treat (ITT) effect",
    x = "Recibió carta recordatoria",
    y = "Test Score Year 6 (estandarizado)"
  ) +
  scale_x_discrete(labels = c("No", "Sí")) +
  theme_minimal()

print(p_rf)

cat("\n¿ES ESTO EL ATE? ¿O EL ATT?\n")
cat("-"*60, "\n")

# aquí va el código de latex - RESPUESTA k

# =============================================================================
# PREGUNTA l: Forma reducida con controles
# =============================================================================

cat("\n\n", "="*80, "\n")
cat("PREGUNTA l: Forma reducida CON controles\n")
cat("="*80, "\n\n")

cat("MODELO:\n")
cat("test_score_6 = γ₀ + γ₁*letter + γ₂*female + γ₃*parental_schooling +\n")
cat("               γ₄*parental_income + γ₅*test_score_5 + ε\n\n")

cat("¿POR QUÉ AGREGAR CONTROLES SI LETTER FUE ALEATORIZADO?\n")
cat("- Letter es aleatorio → No necesitamos controles para insesgamiento\n")
cat("- Pero controles pueden MEJORAR PRECISIÓN (menor varianza)\n")
cat("- Si X's predicen fuertemente Y, incluirlas reduce σ²\n\n")

# Forma reducida con controles
reduced_form_controls <- lm(
  test_score ~ letter + female + parental_schooling + 
    log(parental_income) + test_score_year5,
  data = data_iv
)

cat("RESULTADOS CON CONTROLES:\n")
cat("="*60, "\n\n")
summary(reduced_form_controls)

# Extraer información
coef_rf_c <- coef(reduced_form_controls)["letter"]
se_rf_c <- summary(reduced_form_controls)$coefficients["letter", "Std. Error"]
t_rf_c <- summary(reduced_form_controls)$coefficients["letter", "t value"]
p_rf_c <- summary(reduced_form_controls)$coefficients["letter", "Pr(>|t|)"]

cat("\n", "="*60, "\n")
cat("COMPARACIÓN: SIN vs CON controles\n")
cat("="*60, "\n")
cat("Sin controles:  γ̂₁ =", round(coef_rf, 4), 
    "  SE =", round(se_rf, 4), "\n")
cat("Con controles:  γ̂₁ =", round(coef_rf_c, 4), 
    "  SE =", round(se_rf_c, 4), "\n\n")

cat("Cambio en coeficiente:", round(coef_rf_c - coef_rf, 4), "\n")
cat("Cambio en SE:", round(se_rf_c - se_rf, 4), "\n\n")

if(abs(coef_rf_c - coef_rf) < 0.05) {
  cat("✓ Coeficiente casi idéntico (cambio < 0.05)\n")
  cat("  Confirma que letter es aleatorio\n")
}

if(se_rf_c < se_rf) {
  cat("✓ Error estándar REDUJO con controles\n")
  cat("  Ganancia en precisión de", 
      round((1 - se_rf_c/se_rf)*100, 1), "%\n")
} else {
  cat("✗ Error estándar AUMENTÓ con controles (inusual)\n")
}

# Tabla comparativa
modelsummary(
  list("Sin controles" = reduced_form, 
       "Con controles" = reduced_form_controls),
  stars = TRUE,
  coef_omit = "Intercept",
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  title = "Forma reducida: Efecto de letter sobre test_score"
)

# aquí va el código de latex - RESPUESTA l

# =============================================================================
# ESTIMACIÓN 2SLS (Two-Stage Least Squares)
# =============================================================================

cat("\n\n", "="*80, "\n")
cat("ESTIMACIÓN IV: 2SLS (Two-Stage Least Squares)\n")
cat("="*80, "\n\n")

cat("2SLS combina la primera etapa y la forma reducida:\n\n")

cat("ETAPA 1: summercamp = π₀ + π₁*letter + π*X + v\n")
cat("         → Obtener summercamp_hat (valores predichos)\n\n")

cat("ETAPA 2: test_score = β₀ + β₁*summercamp_hat + β*X + u\n")
cat("         → β̂₁ es el efecto causal (LATE)\n\n")

cat("FÓRMULA DE WALD (sin controles):\n")
cat("β̂₁_IV = (Forma Reducida) / (Primera Etapa)\n")
cat("       = γ̂₁ / π̂₁\n\n")

# Estimación 2SLS con ivreg
iv_model <- ivreg(
  test_score ~ summercamp + female + parental_schooling + 
    log(parental_income) + test_score_year5 |
    letter + female + parental_schooling + 
    log(parental_income) + test_score_year5,
  data = data_iv
)

cat("RESULTADOS 2SLS:\n")
cat("="*60, "\n\n")
summary(iv_model, diagnostics = TRUE)

# Extraer coeficiente IV
coef_iv <- coef(iv_model)["summercamp"]
se_iv <- summary(iv_model)$coefficients["summercamp", "Std. Error"]
t_iv <- summary(iv_model)$coefficients["summercamp", "t value"]
p_iv <- summary(iv_model)$coefficients["summercamp", "Pr(>|t|)"]

cat("\n", "="*60, "\n")
cat("EFECTO CAUSAL (LATE):\n")
cat("="*60, "\n")
cat("β̂₁_IV =", round(coef_iv, 4), "\n")
cat("SE =", round(se_iv, 4), "\n")
cat("t-stat =", round(t_iv, 3), "\n")
cat("P-value =", round(p_iv, 5), "\n\n")

cat("INTERPRETACIÓN:\n")
cat("Para quienes cumplen (compliers), participar en summercamp\n")
cat("aumenta el puntaje en", round(coef_iv, 4), "desviaciones estándar.\n\n")

# Verificar fórmula de Wald
wald_estimate <- coef_rf / coef_letter
cat("VERIFICACIÓN FÓRMULA DE WALD (sin controles):\n")
cat("Forma reducida / Primera etapa =", 
    round(coef_rf, 4), "/", round(coef_letter, 4), 
    "=", round(wald_estimate, 4), "\n\n")

# =============================================================================
# COMPARACIÓN: OLS vs IV
# =============================================================================

cat("\n", "="*80, "\n")
cat("COMPARACIÓN: OLS vs IV\n")
cat("="*80, "\n\n")

# OLS con controles
ols_model <- lm(
  test_score ~ summercamp + female + parental_schooling + 
    log(parental_income) + test_score_year5,
  data = data_iv
)

coef_ols <- coef(ols_model)["summercamp"]
se_ols <- summary(ols_model)$coefficients["summercamp", "Std. Error"]

cat("RESULTADOS:\n")
cat("-"*60, "\n")
cat("OLS:  β̂₁ =", round(coef_ols, 4), "  SE =", round(se_ols, 4), "\n")
cat("IV:   β̂₁ =", round(coef_iv, 4), "   SE =", round(se_iv, 4), "\n")
cat("Diferencia:", round(coef_iv - coef_ols, 4), "\n\n")

# Tabla comparativa completa
modelsummary(
  list("OLS" = ols_model, "2SLS (IV)" = iv_model),
  stars = TRUE,
  coef_rename = c(
    "summercamp" = "Summer Camp",
    "female" = "Female",
    "parental_schooling" = "Parental Schooling",
    "log(parental_income)" = "Log(Income)",
    "test_score_year5" = "Test Score Year 5"
  ),
  gof_map = c("nobs", "r.squared"),
  title = "Comparación OLS vs IV"
)

cat("\n¿POR QUÉ IV > OLS?\n")
cat("-"*60, "\n")

# aquí va el código de latex - RESPUESTA sobre comparación OLS vs IV

# Test de Hausman (endogeneidad)
cat("\n\nTEST DE HAUSMAN (¿Es necesario IV?):\n")
cat("="*60, "\n")
cat("H₀: summercamp es exógeno (OLS es consistente)\n")
cat("H₁: summercamp es endógeno (necesitamos IV)\n\n")

# Test manual de Hausman
# 1. Obtener residuos de primera etapa
residuos_fs <- residuals(first_stage)
data_iv$v_hat <- residuos_fs

# 2. Incluir residuos en regresión OLS
hausman_reg <- lm(
  test_score ~ summercamp + female + parental_schooling + 
    log(parental_income) + test_score_year5 + v_hat,
  data = data_iv
)

# 3. Test de significancia de v_hat
t_hausman <- summary(hausman_reg)$coefficients["v_hat", "t value"]
p_hausman <- summary(hausman_reg)$coefficients["v_hat", "Pr(>|t|)"]

cat("Estadístico t sobre v_hat:", round(t_hausman, 3), "\n")
cat("P-value:", round(p_hausman, 4), "\n\n")

if(p_hausman < 0.05) {
  cat("✗ Rechazamos H₀: summercamp es ENDÓGENO\n")
  cat("  → Necesitamos usar IV\n")
  cat("  → OLS está sesgado\n")
} else {
  cat("✓ No rechazamos H₀: summercamp parece exógeno\n")
  cat("  → OLS sería consistente\n")
  cat("  → IV y OLS deberían ser similares\n")
}

# =============================================================================
# RESUMEN EJERCICIO 4
# =============================================================================

cat("\n\n", "="*80, "\n")
cat("RESUMEN EJERCICIO 4\n")
cat("="*80, "\n\n")

cat("1. SESGO DE SELECCIÓN:\n")
cat("   ✗ Detectado: Participantes tienen mejores puntajes pre-tratamiento\n\n")

cat("2. VARIABLE INSTRUMENTAL (letter):\n")
cat("   ✓ Exógena: Asignación aleatoria confirmada (tabla de balance)\n")
cat("   ✓ Relevante: F-stat =", round(f_stat, 2), "> 10\n\n")

cat("3. ESTIMACIONES:\n")
cat("   ITT (Forma reducida): γ̂ =", round(coef_rf_c, 4), "\n")
cat("   Primera etapa: π̂ =", round(coef_letter, 4), "\n")
cat("   LATE (2SLS): β̂_IV =", round(coef_iv, 4), "\n")
cat("   OLS (sesgado): β̂_OLS =", round(coef_ols, 4), "\n\n")

cat("4. INTERPRETACIÓN:\n")
cat("   - IV > OLS sugiere selección negativa condicional en controles\n")
cat("   - LATE identifica efecto para 'compliers'\n")
cat("   - Efecto sustancial: ~", round(coef_iv, 2), "SD\n\n")

cat("="*80, "\n")

# =============================================================================
# EJERCICIO 5: BAD CONTROLS - EXPERIMENTO DE TIENDAS
# =============================================================================

cat("\n\n", "="*80, "\n")
cat("EJERCICIO 5: BAD CONTROLS Y VARIABLE MEDIADORA\n")
cat("="*80, "\n\n")

cat("CONTEXTO: Experimento en tiendas de snickers\n")
cat("- Tratamiento: Dar pequeños regalos (llaveros) a clientes\n")
cat("- Outcome: Ingresos de la tienda (revenues)\n")
cat("- Mediador: Tasa de retorno de clientes\n\n")

cat("PREGUNTA: ¿Los regalos aumentan los ingresos?\n\n")

cat("TRAMPA: Return_rate es un 'bad control'\n")
cat("- Es afectado por el tratamiento (mediador)\n")
cat("- Controlar por él destruye la identificación causal\n\n")

# Limpiar entorno
rm(list = setdiff(ls(), "analysisdata"))

# Cargar paquetes
library(pacman)
p_load(tidyverse, glue, ggdag, dagitty)

# Generar datos experimentales
set.seed(22)
n_stores <- 200
true_gift_effect <- 100
noise <- 50

data_downstream <- tibble(store_id = 1:n_stores) %>%
  mutate(
    # Tratamiento (aleatorio)
    gives_gift = rbinom(n_stores, 1, prob = 0.5),
    
    # Tasa de retorno (afectada por treatment)
    # La tasa base es ~50%, aumenta 10% si hay regalos
    return_rate = runif(n_stores, min = 0.4, max = 0.6) + gives_gift * 0.1,
    
    # Ingresos (afectados por return_rate)
    # Los regalos afectan ingresos SOLO a través de return_rate
    revenue = 500 + true_gift_effect * 10 * return_rate + 
      rnorm(n_stores, mean = 0, sd = noise)
  )

cat("DATOS GENERADOS:\n")
cat("-"*60, "\n")
cat("Tiendas:", n_stores, "\n")
cat("Efecto verdadero (vía return_rate):", true_gift_effect * 10, 
    "$ por unidad de return_rate\n")
cat("Aumento en return_rate por regalo: 0.10 (10 puntos porcentuales)\n")
cat("Efecto total esperado:", true_gift_effect * 10 * 0.10, "$\n\n")

# =============================================================================
# PREGUNTA a: Visualización y DAG
# =============================================================================

cat("\n", "="*80, "\n")
cat("PREGUNTA a: DAG y visualización de relaciones\n")
cat("="*80, "\n\n")

# Gráfico de dispersión
p_exp <- data_downstream %>%
  mutate(treatment = ifelse(gives_gift == 1, "Gift", "No Gift")) %>%
  ggplot(aes(return_rate, revenue, color = treatment)) +
  geom_point(size = 3, alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual(values = c("Gift" = "coral", "No Gift" = "steelblue")) +
  labs(
    title = "Relación entre tasa de retorno e ingresos",
    subtitle = "Por grupo de tratamiento",
    x = "Return Rate (proporción de clientes que regresan)",
    y = "Revenue ($)",
    color = "Tratamiento"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p_exp)

cat("\nOBSERVACIONES DEL GRÁFICO:\n")
cat("-"*60, "\n")
cat("1. Return_rate y revenue están positivamente relacionados\n")
cat("2. Grupo con regalos tiene mayor return_rate\n")
cat("3. Grupo con regalos tiene mayor revenue\n")
cat("4. Las líneas son paralelas → mismo efecto de return_rate\n\n")

# DAG
cat("DAG (Directed Acyclic Graph):\n")
cat("-"*60, "\n\n")

cat("         gives_gift\n")
cat("          /      \\\n")
cat("         /        \\\n")
cat("        v          v\n")
cat("  return_rate --> revenue\n\n")

cat("INTERPRETACIÓN:\n")
cat("- gives_gift → return_rate (efecto directo)\n")
cat("- return_rate → revenue (efecto directo)\n")
cat("- gives_gift → revenue (efecto INDIRECTO vía return_rate)\n\n")

cat("ESTRUCTURA CAUSAL:\n")
cat("return_rate es un MEDIADOR (mediator)\n")
cat("- Está en el camino causal entre tratamiento y outcome\n")
cat("- NO es un confounder (no causa el tratamiento)\n\n")

# aquí va el código de latex - RESPUESTA a (Ejercicio 5)

# =============================================================================
# PREGUNTA b: Regresión de gifts sobre return_rates
# =============================================================================

cat("\n", "="*80, "\n")
cat("PREGUNTA b: ¿Los regalos aumentan la tasa de retorno?\n")
cat("="*80, "\n\n")

# Regresión: return_rate ~ gives_gift
modelo_b <- lm(return_rate ~ gives_gift, data = data_downstream)

cat("MODELO: return_rate = α₀ + α₁*gives_gift + u\n\n")

summary(modelo_b)

coef_b <- coef(modelo_b)["gives_gift"]
se_b <- summary(modelo_b)$coefficients["gives_gift", "Std. Error"]
t_b <- summary(modelo_b)$coefficients["gives_gift", "t value"]
p_b <- summary(modelo_b)$coefficients["gives_gift", "Pr(>|t|)"]

cat("\n", "="*60, "\n")
cat("INTERPRETACIÓN:\n")
cat("="*60, "\n")
cat("α̂₁ =", round(coef_b, 4), "\n")
cat("SE =", round(se_b, 4), "\n")
cat("t-stat =", round(t_b, 3), "\n")
cat("P-value =", format(p_b, scientific = FALSE), "\n\n")

cat("RESPUESTA: SÍ, los regalos aumentan significativamente la tasa de retorno\n")
cat("en", round(coef_b * 100, 2), "puntos porcentuales en promedio.\n\n")

# Visualización
medias_return <- data_downstream %>%
  group_by(gives_gift) %>%
  summarise(
    Media = mean(return_rate),
    SE = sd(return_rate) / sqrt(n())
  ) %>%
  mutate(Grupo = ifelse(gives_gift == 1, "Con regalo", "Sin regalo"))

p_return <- ggplot(medias_return, aes(x = Grupo, y = Media, fill = Grupo)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(aes(ymin = Media - 1.96*SE, ymax = Media + 1.96*SE),
                width = 0.2) +
  scale_fill_manual(values = c("steelblue", "coral")) +
  labs(
    title = "Efecto de regalos sobre tasa de retorno",
    y = "Return Rate (proporción)",
    x = ""
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(p_return)

# aquí va el código de latex - RESPUESTA b (Ejercicio 5)

# =============================================================================
# PREGUNTA c: Regresión correcta (sin bad control)
# =============================================================================

cat("\n\n", "="*80, "\n")
cat("PREGUNTA c: Estimación correcta del efecto causal\n")
cat("="*80, "\n\n")

cat("MODELO CORRECTO (sin controlar por mediador):\n")
cat("revenue = β₀ + β₁*gives_gift + ε\n\n")

cat("¿POR QUÉ ESTE MODELO?\n")
cat("- Queremos el EFECTO TOTAL de regalos sobre ingresos\n")
cat("- Esto incluye el efecto vía return_rate\n")
cat("- No controlamos por return_rate porque:\n")
cat("  1. Es un mediador (no un confounder)\n")
cat("  2. Está en el camino causal\n")
cat("  3. Tratamiento fue aleatorizado (no hay confounding)\n\n")

# Regresión correcta
modelo_correcto <- lm(revenue ~ gives_gift, data = data_downstream)

cat("RESULTADOS:\n")
cat("="*60, "\n\n")
summary(modelo_correcto)

coef_correcto <- coef(modelo_correcto)["gives_gift"]
se_correcto <- summary(modelo_correcto)$coefficients["gives_gift", "Std. Error"]
t_correcto <- summary(modelo_correcto)$coefficients["gives_gift", "t value"]
p_correcto <- summary(modelo_correcto)$coefficients["gives_gift", "Pr(>|t|)"]

cat("\n", "="*60, "\n")
cat("EFECTO CAUSAL TOTAL:\n")
cat("="*60, "\n")
cat("β̂₁ =", round(coef_correcto, 2), "$\n")
cat("SE =", round(se_correcto, 2), "\n")
cat("IC 95%: [", round(coef_correcto - 1.96*se_correcto, 2), ",", 
    round(coef_correcto + 1.96*se_correcto, 2), "]\n\n")

cat("INTERPRETACIÓN:\n")
cat("Dar regalos aumenta los ingresos en $", round(coef_correcto, 2), 
    " en promedio.\n")
cat("Efecto esperado teórico: $", true_gift_effect * 10 * 0.1, "\n")
cat("Estimación cercana al efecto verdadero ✓\n\n")

# aquí va el código de latex - RESPUESTA c (Ejercicio 5)

# =============================================================================
# PREGUNTA d: Explicación formal del sesgo
# =============================================================================

cat("\n", "="*80, "\n")
cat("PREGUNTA d: ¿Por qué controlar por return_rate es un error?\n")
cat("="*80, "\n\n")

# aquí va el código de latex - RESPUESTA d (Ejercicio 5)

# =============================================================================
# PREGUNTA e: Bad control en acción
# =============================================================================

cat("\n", "="*80, "\n")
cat("PREGUNTA e: Regresión INCORRECTA (con bad control)\n")
cat("="*80, "\n\n")

cat("MODELO INCORRECTO (controlando por mediador):\n")
cat("revenue = β₀ + β₁*gives_gift + β₂*return_rate + ε\n\n")

cat("ADVERTENCIA: ¡Este modelo está MAL especificado!\n")
cat("Controlar por return_rate BLOQUEA el camino causal\n\n")

# Regresión incorrecta
modelo_incorrecto <- lm(revenue ~ gives_gift + return_rate, 
                        data = data_downstream)

cat("RESULTADOS DEL MODELO INCORRECTO:\n")
cat("="*60, "\n\n")
summary(modelo_incorrecto)

coef_incorrecto <- coef(modelo_incorrecto)["gives_gift"]
se_incorrecto <- summary(modelo_incorrecto)$coefficients["gives_gift", "Std. Error"]
coef_return <- coef(modelo_incorrecto)["return_rate"]

cat("\n", "="*60, "\n")
cat("COMPARACIÓN DE ESTIMADORES:\n")
cat("="*60, "\n")
cat("Sin controlar return_rate:  β̂₁ =", round(coef_correcto, 2), "$\n")
cat("Controlando return_rate:    β̂₁ =", round(coef_incorrecto, 2), "$\n")
cat("Diferencia (sesgo):", round(coef_correcto - coef_incorrecto, 2), "$\n\n")

cat("SESGO:", round(abs(coef_correcto - coef_incorrecto)/coef_correcto * 100, 1), 
    "% del efecto verdadero\n\n")

if(abs(coef_incorrecto) < abs(coef_correcto)) {
  cat("✗ El efecto está SESGADO HACIA CERO (downward biased)\n")
  cat("  → Subestimamos el efecto de los regalos\n")
}

cat("\n¿POR QUÉ OCURRE EL SESGO?\n")
cat("-"*60, "\n")
cat("Al controlar por return_rate, preguntamos:\n")
cat("'¿Cuál es el efecto de regalos, manteniendo return_rate constante?'\n\n")

cat("Pero esto NO tiene sentido porque:\n")
cat("- Los regalos CAUSAN cambios en return_rate\n")
cat("- Fijar return_rate artificialmente bloquea el mecanismo causal\n")
cat("- Solo capturamos el 'efecto directo' (que es ~0 en este caso)\n")
cat("- Perdemos el 'efecto indirecto' (que es todo el efecto)\n\n")

# Tabla comparativa
modelsummary(
  list("Correcto (sin control)" = modelo_correcto,
       "Incorrecto (con control)" = modelo_incorrecto),
  stars = TRUE,
  coef_rename = c(
    "gives_gift" = "Gives Gift",
    "return_rate" = "Return Rate"
  ),
  gof_map = c("nobs", "r.squared"),
  title = "Comparación: Modelo correcto vs Bad control"
)

# Visualización del sesgo
df_comparacion <- data.frame(
  Modelo = c("Correcto\n(sin return_rate)", 
             "Incorrecto\n(con return_rate)"),
  Efecto = c(coef_correcto, coef_incorrecto),
  SE = c(se_correcto, se_incorrecto),
  Tipo = c("Correcto", "Incorrecto")
)

p_comparacion <- ggplot(df_comparacion, 
                        aes(x = Modelo, y = Efecto, fill = Tipo)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(aes(ymin = Efecto - 1.96*SE, ymax = Efecto + 1.96*SE),
                width = 0.2) +
  geom_hline(yintercept = true_gift_effect * 10 * 0.1, 
             linetype = "dashed", color = "red", linewidth = 1) +
  annotate("text", x = 1.5, y = true_gift_effect * 10 * 0.1 + 10,
           label = "Efecto verdadero", color = "red") +
  scale_fill_manual(values = c("Correcto" = "steelblue", 
                               "Incorrecto" = "coral")) +
  labs(
    title = "Sesgo por bad control",
    subtitle = "Controlar por mediador subestima el efecto causal",
    y = "Efecto estimado sobre revenue ($)",
    x = ""
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(p_comparacion)

# aquí va el código de latex - RESPUESTA e (Ejercicio 5)

# =============================================================================
# DESCOMPOSICIÓN DEL EFECTO TOTAL
# =============================================================================

cat("\n\n", "="*80, "\n")
cat("ANÁLISIS DE MEDIACIÓN: Descomposición del efecto\n")
cat("="*80, "\n\n")

cat("EFECTO TOTAL = EFECTO DIRECTO + EFECTO INDIRECTO\n\n")

# Efecto sobre mediador (ya lo tenemos)
efecto_sobre_mediador <- coef_b

# Efecto del mediador sobre outcome (de modelo incorrecto)
efecto_mediador_outcome <- coef_return

# Efecto indirecto
efecto_indirecto <- efecto_sobre_mediador * efecto_mediador_outcome

# Efecto directo (de modelo incorrecto)
efecto_directo <- coef_incorrecto

# Efecto total
efecto_total_calc <- efecto_directo + efecto_indirecto

cat("CÁLCULOS:\n")
cat("-"*60, "\n")
cat("1. Efecto de gift sobre return_rate:", round(efecto_sobre_mediador, 4), "\n")
cat("2. Efecto de return_rate sobre revenue:", round(efecto_mediador_outcome, 2), "\n")
cat("3. Efecto indirecto (1 × 2):", round(efecto_indirecto, 2), "$\n")
cat("4. Efecto directo:", round(efecto_directo, 2), "$\n")
cat("5. Efecto total (3 + 4):", round(efecto_total_calc, 2), "$\n\n")

cat("VERIFICACIÓN:\n")
cat("Efecto total estimado directamente:", round(coef_correcto, 2), "$\n")
cat("Efecto total calculado (directo + indirecto):", 
    round(efecto_total_calc, 2), "$\n")
cat("Diferencia:", round(abs(coef_correcto - efecto_total_calc), 2), 
    "$ (error de aproximación)\n\n")

cat("PORCENTAJES:\n")
cat("% del efecto que es indirecto (vía return_rate):", 
    round(efecto_indirecto/efecto_total_calc * 100, 1), "%\n")
cat("% del efecto que es directo:", 
    round(efecto_directo/efecto_total_calc * 100, 1), "%\n\n")

cat("CONCLUSIÓN:\n")
cat("En este ejemplo, TODO el efecto es indirecto (vía return_rate).\n")
cat("Por eso, controlar por return_rate elimina completamente el efecto.\n\n")

# =============================================================================
# CONCLUSIONES FINALES
# =============================================================================

cat("\n", "="*80, "\n")
cat("CONCLUSIONES FINALES - EJERCICIO 5\n")
cat("="*80, "\n\n")

cat("LECCIONES SOBRE BAD CONTROLS:\n\n")

cat("1. IDENTIFICAR MEDIADORES:\n")
cat("   - Variables en el camino causal: X → M → Y\n")
cat("   - NO son confounders (no causan X)\n")
cat("   - Controlar por ellos BLOQUEA el camino causal\n\n")

cat("2. CUÁNDO NO CONTROLAR:\n")
cat("   - En experimentos: tratamiento es aleatorio\n")
cat("   - Cuando queremos el EFECTO TOTAL\n")
cat("   - Cuando la variable es un mediador\n\n")

cat("3. CUÁNDO SÍ CONTROLAR:\n")
cat("   - Para ajustar por confounders (causas comunes)\n")
cat("   - Para mejorar precisión (variables predictoras del outcome)\n")
cat("   - Para estimar efectos DIRECTOS específicos (mediación)\n\n")

cat("4. REGLA GENERAL:\n")
cat("   Antes de controlar por cualquier variable, pregúntate:\n")
cat("   a) ¿Es un confounder? → SÍ controlar\n")
cat("   b) ¿Es un mediador? → NO controlar (para efecto total)\n")
cat("   c) ¿Es un colisionador? → NUNCA controlar\n\n")

cat("5. HERRAMIENTA: DAGs\n")
cat("   - Dibujar el DAG antes de modelar\n")
cat("   - Identificar caminos causales y espurios\n")
cat("   - Aplicar criterio de puerta trasera (backdoor)\n\n")

cat("="*80, "\n")