# =============================================================================
# EJERCICIO 4: EVALUACIÓN DE ESCUELA DE VERANO (SUMMER CAMP)
# Partes a-f: Carga de datos, exploración y análisis de balance
# =============================================================================
#
# CONTEXTO: ¿Asistir a una escuela de verano mejora los puntajes en exámenes?
#
# CARACTERÍSTICAS DEL PROGRAMA:
# - Escuela de verano OPCIONAL entre año 5 y año 6 (niños ~10 años)
# - GRATUITA pero requiere participación activa de los padres
# - Puede enfocarse en currículo o en habilidades (ej. "grit")
#
# PREGUNTA DE INVESTIGACIÓN: ¿Mejora la participación los resultados?
#
# DESAFÍO: Posible SESGO DE SELECCIÓN porque:
#   - No es aleatoria
#   - Requiere iniciativa parental
#   - Familias más motivadas pueden auto-seleccionarse
# =============================================================================

# Limpiar entorno
rm(list = ls())

# Cargar librerías necesarias
library(tidyverse)      # Manipulación de datos
library(haven)          # Leer archivos .dta (Stata)
library(skimr)          # Resúmenes rápidos de datos
library(ggplot2)        # Gráficos
library(patchwork)      # Combinar gráficos
library(modelsummary)   # Tablas profesionales
library(knitr)          # Tablas simples

# =============================================================================
# INTRODUCCIÓN CONCEPTUAL: Sesgo de Selección en Programas Educativos
# =============================================================================

cat("="*80, "\n")
cat("SESGO DE SELECCIÓN EN EVALUACIÓN DE PROGRAMAS\n")
cat("="*80, "\n\n")

cat("PROBLEMA FUNDAMENTAL:\n")
cat("Si la participación es VOLUNTARIA, quienes participan pueden ser\n")
cat("sistemáticamente diferentes de quienes no participan.\n\n")

cat("TIPOS DE SELECCIÓN:\n")
cat("1. SELECCIÓN POSITIVA: Los 'mejores' participan\n")
cat("   - Familias más educadas, motivadas\n")
cat("   - Niños con mejor desempeño previo\n")
cat("   → Sobrestima el efecto del programa\n\n")

cat("2. SELECCIÓN NEGATIVA: Los 'peores' participan\n")
cat("   - Familias preocupadas por bajo rendimiento\n")
cat("   - Niños con dificultades académicas\n")
cat("   → Subestima el efecto del programa\n\n")

cat("SOLUCIONES:\n")
cat("- Diseño experimental (aleatorización)\n")
cat("- Variables instrumentales (IV)\n")
cat("- Diferencias-en-diferencias\n")
cat("- Matching / controles adecuados\n\n")

# =============================================================================
# PREGUNTA a: Cargar datos y transformar a formato largo
# =============================================================================

cat("\n", "="*80, "\n")
cat("PREGUNTA a: Cargar datos y transformar a formato 'tidy'\n")
cat("="*80, "\n\n")

# Nota: Asumiendo que el archivo summercamp.dta está en el directorio de trabajo
# Si no existe, crearemos datos simulados para ilustrar

# Intentar cargar datos reales
if(file.exists("summercamp.dta")) {
  summercamp <- read_dta("summercamp.dta")
  cat("✓ Datos reales cargados exitosamente\n\n")
} else {
  cat("⚠ Archivo summercamp.dta no encontrado.\n")
  cat("  Generando datos simulados para ilustración...\n\n")
  
  # Generar datos simulados realistas
  set.seed(2024)
  n <- 1000  # número de estudiantes
  
  summercamp <- tibble(
    person_id = 1:n,
    school_id = sample(1:20, n, replace = TRUE),
    female = rbinom(n, 1, 0.5),
    parental_income = exp(rnorm(n, mean = log(50000), sd = 0.6)),
    parental_schooling = pmax(8, pmin(20, rnorm(n, mean = 13, sd = 3))),
    
    # Puntaje año 5 (pre-tratamiento)
    test_year_5 = 50 + 
      0.3 * (parental_schooling - 13) * 5 +
      0.00001 * (parental_income - 50000) +
      rnorm(n, 0, 10),
    
    # Carta recordatoria (aleatorizada - esto es nuestro IV)
    letter = rbinom(n, 1, 0.5),
    
    # Participación en summer camp (depende de letter y características)
    summercamp_latent = -2 + 
      1.5 * letter +  # La carta aumenta participación
      0.05 * (parental_schooling - 13) +
      0.000015 * (parental_income - 50000) +
      0.01 * test_year_5 +
      rnorm(n, 0, 1),
    summercamp = as.numeric(summercamp_latent > 0),
    
    # Puntaje año 6 (post-tratamiento)
    # Efecto verdadero del summer camp: +5 puntos
    test_year_6 = test_year_5 + 
      5 * summercamp +  # Efecto causal
      0.2 * (parental_schooling - 13) * 3 +
      rnorm(n, 0, 8)
  ) %>%
    select(-summercamp_latent)  # Eliminar variable latente
  
  # Introducir algunos valores faltantes de manera no aleatoria
  # (familias de bajos ingresos más propensas a no reportar escolaridad)
  missing_prob <- 0.3 * exp(-parental_income / 40000)
  summercamp$parental_schooling[runif(n) < missing_prob] <- NA
  
  cat("✓ Datos simulados generados\n\n")
}

# Explorar estructura básica
cat("ESTRUCTURA DE LOS DATOS ORIGINALES:\n")
cat("-"*60, "\n")
cat("Dimensiones:", nrow(summercamp), "filas x", ncol(summercamp), "columnas\n\n")

cat("Variables disponibles:\n")
print(names(summercamp))
cat("\n")

cat("Primeras observaciones:\n")
print(head(summercamp))

cat("\n\nEXPLICACIÓN DE VARIABLES:\n")
cat("-"*60, "\n")
cat("person_id: Identificador único del estudiante\n")
cat("school_id: Identificador de la escuela\n")
cat("summercamp: 1 si participó, 0 si no\n")
cat("female: 1 si es mujer, 0 si hombre\n")
cat("parental_income: Ingreso parental (en $)\n")
cat("parental_schooling: Años de escolaridad de los padres\n")
cat("test_year_5: Puntaje en examen año 5 (PRE-tratamiento)\n")
cat("test_year_6: Puntaje en examen año 6 (POST-tratamiento)\n")
cat("letter: 1 si recibió carta recordatoria, 0 si no\n\n")

# =============================================================================
# TRANSFORMACIÓN A FORMATO LARGO (TIDY DATA)
# =============================================================================

cat("\n", "="*60, "\n")
cat("TRANSFORMACIÓN A FORMATO LARGO ('TIDY')\n")
cat("="*60, "\n\n")

cat("¿Por qué formato largo?\n")
cat("- Facilita análisis por año\n")
cat("- Permite gráficos agrupados por tiempo\n")
cat("- Estructura más flexible para análisis\n\n")

# Transformar usando pivot_longer
school_data <- summercamp %>%
  pivot_longer(
    cols = starts_with("test_year_"),  # Columnas que empiezan con "test_year_"
    names_to = "year",                 # Nueva columna con el nombre
    names_prefix = "test_year_",       # Remover este prefijo
    names_transform = list(year = as.integer),  # Convertir año a entero
    values_to = "test_score"           # Nueva columna con los valores
  )

cat("ANTES (formato ancho):\n")
cat("person_id | test_year_5 | test_year_6\n")
cat("    1     |     65      |     70\n\n")

cat("DESPUÉS (formato largo):\n")
cat("person_id | year | test_score\n")
cat("    1     |  5   |     65\n")
cat("    1     |  6   |     70\n\n")

cat("Nueva estructura:\n")
cat("Dimensiones:", nrow(school_data), "filas x", ncol(school_data), "columnas\n")
cat("Cada estudiante ahora tiene 2 filas (año 5 y año 6)\n\n")

print(head(school_data, 4))

# aquí va el código de latex - RESPUESTA a

# =============================================================================
# PREGUNTA b: Resumen rápido con skim()
# =============================================================================

cat("\n\n", "="*80, "\n")
cat("PREGUNTA b: Resumen descriptivo de los datos\n")
cat("="*80, "\n\n")

cat("Usando skimr::skim() para un resumen completo:\n\n")

# Resumen completo
skim_result <- skim(school_data)
print(skim_result)

# Análisis personalizado por tipo de variable
cat("\n\n", "="*60, "\n")
cat("ANÁLISIS DETALLADO POR TIPO DE VARIABLE\n")
cat("="*60, "\n\n")

# Variables numéricas
cat("VARIABLES NUMÉRICAS:\n")
vars_numericas <- c("test_score", "parental_income", "parental_schooling")
summary_num <- school_data %>%
  select(all_of(vars_numericas)) %>%
  summary()
print(summary_num)

# Variables categóricas
cat("\n\nVARIABLES CATEGÓRICAS:\n")
cat("\nSummercamp (participación):\n")
print(table(school_data$summercamp))
cat("Proporción:", mean(school_data$summercamp, na.rm = TRUE), "\n")

cat("\nFemale (género):\n")
print(table(school_data$female))
cat("Proporción mujeres:", mean(school_data$female, na.rm = TRUE), "\n")

cat("\nLetter (carta recordatoria):\n")
print(table(school_data$letter))
cat("Proporción que recibió carta:", mean(school_data$letter, na.rm = TRUE), "\n")

cat("\nYear (año del examen):\n")
print(table(school_data$year))

# aquí va el código de latex - RESPUESTA b

# =============================================================================
# PREGUNTA c: Análisis de valores faltantes (Missing values)
# =============================================================================

cat("\n\n", "="*80, "\n")
cat("PREGUNTA c: Patrón de valores faltantes\n")
cat("="*80, "\n\n")

cat("¿Los valores faltantes son aleatorios?\n")
cat("Esto es crucial porque:\n")
cat("- MCAR (Missing Completely At Random): Eliminar filas es OK\n")
cat("- MAR (Missing At Random): Necesitamos métodos más sofisticados\n")
cat("- MNAR (Missing Not At Random): Sesgo potencial serio\n\n")

# Contar valores faltantes
cat("VALORES FALTANTES POR VARIABLE:\n")
cat("-"*60, "\n")
missing_summary <- school_data %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "N_Missing") %>%
  mutate(Proporcion = N_Missing / nrow(school_data))

print(kable(missing_summary, digits = 4))

# Crear dummy para valores faltantes en parental_schooling
school_data <- school_data %>%
  mutate(missing_schooling = as.numeric(is.na(parental_schooling)))

cat("\n\nCREADA VARIABLE: missing_schooling\n")
cat("= 1 si parental_schooling es NA, 0 si no\n\n")

# Correlación entre missing y parental_income
cat("PRUEBA DE ALEATORIEDAD:\n")
cat("-"*60, "\n")
cat("Correlación entre missing_schooling y parental_income:\n\n")

cor_test <- cor.test(school_data$missing_schooling, 
                     log(school_data$parental_income),
                     use = "complete.obs")

print(cor_test)

cat("\nInterpretación:\n")
if(abs(cor_test$estimate) > 0.1 & cor_test$p.value < 0.05) {
  cat("✗ Correlación significativa y sustancial\n")
  cat("  → Los valores faltantes NO son aleatorios (MNAR o MAR)\n")
  cat("  → Familias de menor ingreso tienen más probabilidad de\n")
  cat("    no reportar escolaridad parental\n")
} else {
  cat("✓ Correlación no significativa o muy pequeña\n")
  cat("  → Los valores faltantes parecen ser aleatorios (MCAR)\n")
}

# Visualización
p_missing <- ggplot(school_data, aes(x = log(parental_income), 
                                     fill = factor(missing_schooling))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(
    values = c("0" = "steelblue", "1" = "red"),
    labels = c("Reportado", "Faltante")
  ) +
  labs(
    title = "Distribución de ingreso según reporte de escolaridad",
    subtitle = "¿Hay evidencia de valores faltantes no aleatorios?",
    x = "Log(Ingreso Parental)",
    y = "Densidad",
    fill = "Escolaridad parental"
  ) +
  theme_minimal()

print(p_missing)

# Comparar medias
cat("\n\nCOMPARACIÓN DE MEDIAS:\n")
cat("-"*60, "\n")
medias_missing <- school_data %>%
  group_by(missing_schooling) %>%
  summarise(
    Mean_Income = mean(parental_income, na.rm = TRUE),
    SD_Income = sd(parental_income, na.rm = TRUE),
    N = n()
  )

print(kable(medias_missing, digits = 2))

# Test t
cat("\n\nPRUEBA T DE DIFERENCIA DE MEDIAS:\n")
t_test_missing <- t.test(
  log(parental_income) ~ missing_schooling,
  data = school_data
)
print(t_test_missing)

# aquí va el código de latex - RESPUESTA c

# =============================================================================
# PREGUNTA d: Eliminar filas con NA
# =============================================================================

cat("\n\n", "="*80, "\n")
cat("PREGUNTA d: Eliminar observaciones con valores faltantes\n")
cat("="*80, "\n\n")

cat("SUPUESTO: Missing At Random (MAR)\n")
cat("Por lo tanto, eliminar filas con NA es una estrategia válida\n\n")

# Contar antes de eliminar
n_antes <- nrow(school_data)
n_na <- sum(!complete.cases(school_data))

cat("Observaciones antes:", n_antes, "\n")
cat("Observaciones con al menos un NA:", n_na, "\n")
cat("Proporción con NA:", round(n_na/n_antes * 100, 2), "%\n\n")

# Eliminar NA
analysisdata <- school_data %>%
  drop_na()

n_despues <- nrow(analysisdata)

cat("Observaciones después:", n_despues, "\n")
cat("Observaciones eliminadas:", n_antes - n_despues, "\n\n")

cat("✓ Dataset 'analysisdata' creado sin valores faltantes\n")

# aquí va el código de latex - RESPUESTA d

# =============================================================================
# PREGUNTA e: Estandarización de puntajes
# =============================================================================

cat("\n\n", "="*80, "\n")
cat("PREGUNTA e: ¿Por qué estandarizar puntajes?\n")
cat("="*80, "\n\n")

cat("ESTANDARIZACIÓN (Z-SCORES):\n")
cat("Convertir puntajes a media 0 y desviación estándar 1\n\n")

cat("VENTAJAS:\n")
cat("1. INTERPRETACIÓN INTUITIVA:\n")
cat("   - Un cambio de 1 unidad = 1 desviación estándar\n")
cat("   - Fácil comparar efectos entre diferentes tests\n\n")

cat("2. COMPARABILIDAD:\n")
cat("   - Podemos comparar puntajes de diferentes años\n")
cat("   - O entre diferentes tests con escalas distintas\n\n")

cat("3. MAGNITUD DEL EFECTO:\n")
cat("   - Los coeficientes se interpretan como 'effect sizes'\n")
cat("   - Estándar en literatura educativa\n\n")

cat("FÓRMULA:\n")
cat("z_it = (test_score_it - mean(test_score_t)) / sd(test_score_t)\n")
cat("donde t = año (5 o 6)\n\n")

cat("IMPORTANTE: Estandarizar POR AÑO separadamente\n")
cat("Así preservamos diferencias en dificultad del test\n\n")

# Estandarizar agrupando por año
analysisdata <- analysisdata %>%
  group_by(year) %>%
  mutate(
    test_score_std = (test_score - mean(test_score)) / sd(test_score)
  ) %>%
  ungroup()

# Verificar estandarización
cat("VERIFICACIÓN DE LA ESTANDARIZACIÓN:\n")
cat("-"*60, "\n")

verificacion <- analysisdata %>%
  group_by(year) %>%
  summarise(
    Media_Original = mean(test_score),
    SD_Original = sd(test_score),
    Media_Estandarizada = mean(test_score_std),
    SD_Estandarizada = sd(test_score_std)
  )

print(kable(verificacion, digits = 4))

cat("\n✓ Verificación exitosa:\n")
cat("  - Media estandarizada ≈ 0 (para cada año)\n")
cat("  - SD estandarizada ≈ 1 (para cada año)\n\n")

# Actualizar para usar test_score_std como test_score
analysisdata <- analysisdata %>%
  select(-test_score) %>%
  rename(test_score = test_score_std)

cat("Nota: De aquí en adelante, 'test_score' se refiere a\n")
cat("      puntajes estandarizados (z-scores)\n")

# aquí va el código de latex - RESPUESTA e

# =============================================================================
# PREGUNTA f: Gráfico de balance pre-tratamiento
# =============================================================================

cat("\n\n", "="*80, "\n")
cat("PREGUNTA f: Evidencia de sesgo de selección\n")
cat("="*80, "\n\n")

cat("PREGUNTA CLAVE: ¿Son comparables los grupos antes del tratamiento?\n\n")

cat("Si hay SESGO DE SELECCIÓN, esperamos ver:\n")
cat("- Diferencias sistemáticas en características pre-tratamiento\n")
cat("- Por ejemplo: participantes con mejores puntajes en año 5\n\n")

cat("PRUEBA: Comparar puntajes año 5 entre participantes y no participantes\n\n")

# Filtrar solo año 5 (pre-tratamiento)
data_year5 <- analysisdata %>%
  filter(year == 5)

# Calcular estadísticas por grupo
stats_balance <- data_year5 %>%
  group_by(summercamp) %>%
  summarise(
    N = n(),
    Media = mean(test_score),
    SD = sd(test_score),
    SE = SD / sqrt(N),
    CI_lower = Media - 1.96 * SE,
    CI_upper = Media + 1.96 * SE
  ) %>%
  mutate(Grupo = ifelse(summercamp == 1, 
                        "Participantes\n(summercamp=1)", 
                        "No participantes\n(summercamp=0)"))

print(kable(stats_balance, digits = 3))

# Prueba t de diferencia de medias
cat("\n\nPRUEBA T DE BALANCE:\n")
cat("-"*60, "\n")

t_test_balance <- t.test(
  test_score ~ summercamp,
  data = data_year5
)

print(t_test_balance)

cat("\n\nINTERPRETACIÓN:\n")
diferencia <- diff(t_test_balance$estimate)
p_val <- t_test_balance$p.value

cat("Diferencia de medias:", round(diferencia, 3), "SD\n")
cat("P-value:", round(p_val, 4), "\n\n")

if(p_val < 0.05) {
  cat("✗ HAY EVIDENCIA DE SESGO DE SELECCIÓN\n")
  cat("  Los grupos son significativamente diferentes antes del tratamiento\n")
  if(diferencia > 0) {
    cat("  → SELECCIÓN POSITIVA: Participantes tienen mejores puntajes\n")
  } else {
    cat("  → SELECCIÓN NEGATIVA: Participantes tienen peores puntajes\n")
  }
} else {
  cat("✓ NO hay evidencia fuerte de sesgo de selección\n")
  cat("  Los grupos son similares antes del tratamiento\n")
}

# Crear gráfico de barras con intervalos de confianza
p2 <- ggplot(stats_balance, aes(x = Grupo, y = Media, fill = Grupo)) +
  geom_bar(stat = "identity", width = 0.6, alpha = 0.7) +
  geom_errorbar(
    aes(ymin = CI_lower, ymax = CI_upper),
    width = 0.2,
    linewidth = 1
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_fill_manual(values = c("steelblue", "coral")) +
  labs(
    title = "Puntajes pre-tratamiento (Año 5) por grupo",
    subtitle = "Barras de error = IC 95% | Línea roja = media poblacional (0)",
    y = "Test Score Year 5 (estandarizado)",
    x = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14)
  )

print(p2)

# Gráfico de distribuciones
p_dist <- ggplot(data_year5, aes(x = test_score, 
                                 fill = factor(summercamp))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(
    values = c("0" = "steelblue", "1" = "coral"),
    labels = c("No participantes", "Participantes")
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Distribución de puntajes pre-tratamiento",
    subtitle = "¿Se superponen las distribuciones?",
    x = "Test Score Year 5 (estandarizado)",
    y = "Densidad",
    fill = "Grupo"
  ) +
  theme_minimal()

print(p_dist)

# aquí va el código de latex - RESPUESTA f

# =============================================================================
# PREGUNTA g: Notación formal del sesgo de selección
# =============================================================================

cat("\n\n", "="*80, "\n")
cat("PREGUNTA g: Formalización del sesgo de selección\n")
cat("="*80, "\n\n")

# aquí va el código de latex - RESPUESTA g

# =============================================================================
# PREGUNTA h: Tabla de balance completa
# =============================================================================

cat("\n\n", "="*80, "\n")
cat("PREGUNTA h: Tabla de balance de variables pre-tratamiento\n")
cat("="*80, "\n\n")

cat("Vamos a crear una tabla comparando TODAS las características\n")
cat("observables antes del tratamiento entre participantes y no participantes\n\n")

# Preparar datos para datasummary_balance
testdata <- analysisdata %>%
  filter(year == 5) %>%  # Solo año 5 (pre-tratamiento)
  ungroup() %>%
  mutate(
    Treated = ifelse(summercamp == 1, "Summer Camp", "No Summer Camp")
  ) %>%
  select(
    female, parental_schooling, parental_income, test_score, Treated
  ) %>%
  rename(
    "Female" = female,
    "Parental schooling (years)" = parental_schooling,
    "Parental income (log)" = parental_income,
    "Test Score" = test_score
  ) %>%
  mutate(
    `Parental income (log)` = log(`Parental income (log)`)
  )

# Crear tabla de balance
cat("TABLA DE BALANCE:\n")
cat("="*60, "\n\n")

balance_table <- datasummary_balance(
  ~ Treated,
  data = testdata,
  title = "Balance of pre-treatment variables",
  notes = "Notes: Mean values with standard deviations in parentheses. 
           P-value from t-test of difference in means.",
  fmt = '%.5f',
  dinm_statistic = "p.value",
  output = "markdown"
)

print(balance_table)

# Análisis manual para complementar
cat("\n\nANÁLISIS DETALLADO DE BALANCE:\n")
cat("="*60, "\n\n")

# Para cada variable, hacer test
variables <- c("Female", "Parental schooling (years)", 
               "Parental income (log)", "Test Score")

for(var in variables) {
  cat("\n", var, ":\n")
  cat("-"*40, "\n")
  
  formula_str <- paste("`", var, "` ~ Treated", sep = "")
  
  if(var == "Female") {
    # Para binarias, usar prop.test
    tabla <- table(testdata$Treated, testdata$Female)
    prop_test <- prop.test(tabla)
    cat("Proporción Summer Camp:", 
        round(tabla[1,2]/sum(tabla[1,]), 3), "\n")
    cat("Proporción No Summer Camp:", 
        round(tabla[2,2]/sum(tabla[2,]), 3), "\n")
    cat("P-value:", round(prop_test$p.value, 5), "\n")
  } else {
    # Para continuas, usar t.test
    t_result <- t.test(as.formula(formula_str), data = testdata)
    cat("Media Summer Camp:", round(t_result$estimate[1], 3), "\n")
    cat("Media No Summer Camp:", round(t_result$estimate[2], 3), "\n")
    cat("Diferencia:", round(diff(t_result$estimate), 3), "\n")
    cat("P-value:", round(t_result$p.value, 5), "\n")
    
    if(t_result$p.value < 0.05) {
      cat("→ DIFERENCIA SIGNIFICATIVA (p < 0.05)\n")
    } else {
      cat("→ No hay diferencia significativa\n")
    }
  }
}

cat("\n\n", "="*60, "\n")
cat("CONCLUSIÓN SOBRE EL BALANCE:\n")
cat("="*60, "\n\n")

# aquí va el código de latex - RESPUESTA h

# =============================================================================
# PREGUNTA i: Tabla de balance para carta recordatoria (letter)
# =============================================================================

cat("\n\n", "="*80, "\n")
cat("PREGUNTA i: Balance por carta recordatoria (letter)\n")
cat("="*80, "\n\n")

cat("OBJETIVO: ¿La asignación de la carta fue 'tan buena como aleatoria'?\n\n")

cat("Si la carta fue ASIGNADA ALEATORIAMENTE, esperamos:\n")
cat("- No hay diferencias sistemáticas entre quienes recibieron y no\n")
cat("- Balance perfecto en todas las características observables\n")
cat("- P-values altos (no significativos) en todas las pruebas\n\n")

# Preparar datos para tabla de balance por letter
testdata_letter <- analysisdata %>%
  filter(year == 5) %>%
  ungroup() %>%
  mutate(
    Letter_Status = ifelse(letter == 1, "Received Letter", "No Letter")
  ) %>%
  select(
    female, parental_schooling, parental_income, test_score, Letter_Status
  ) %>%
  rename(
    "Female" = female,
    "Parental schooling (years)" = parental_schooling,
    "Parental income (log)" = parental_income,
    "Test Score" = test_score
  ) %>%
  mutate(
    `Parental income (log)` = log(`Parental income (log)`)
  )

# Crear tabla de balance
cat("TABLA DE BALANCE POR ASIGNACIÓN DE CARTA:\n")
cat("="*60, "\n\n")

balance_table_letter <- datasummary_balance(
  ~ Letter_Status,
  data = testdata_letter,
  title = "Balance by letter assignment",
  notes = "Notes: Testing if letter assignment was 'as good as random'",
  fmt = '%.5f',
  dinm_statistic = "p.value",
  output = "markdown"
)

print(balance_table_letter)

# Análisis detallado
cat("\n\nANÁLISIS DETALLADO:\n")
cat("="*60, "\n\n")

for(var in variables) {
  cat("\n", var, ":\n")
  cat("-"*40, "\n")
  
  formula_str <- paste("`", var, "` ~ Letter_Status", sep = "")
  
  if(var == "Female") {
    tabla <- table(testdata_letter$Letter_Status, testdata_letter$Female)
    prop_test <- prop.test(tabla)
    cat("P-value:", round(prop_test$p.value, 5), "\n")
    if(prop_test$p.value > 0.05) {
      cat("✓ Balance logrado\n")
    } else {
      cat("✗ Desbalance detectado\n")
    }
  } else {
    t_result <- t.test(as.formula(formula_str), data = testdata_letter)
    cat("Diferencia:", round(diff(t_result$estimate), 3), "\n")
    cat("P-value:", round(t_result$p.value, 5), "\n")
    if(t_result$p.value > 0.05) {
      cat("✓ Balance logrado\n")
    } else {
      cat("✗ Desbalance detectado\n")
    }
  }
}

# Test conjunto de balance (F-test)
cat("\n\nTEST CONJUNTO DE BALANCE (F-test):\n")
cat("-"*60, "\n")

modelo_balance <- lm(
  letter ~ female + parental_schooling + log(parental_income) + test_score,
  data = analysisdata %>% filter(year == 5)
)

f_test <- summary(modelo_balance)$fstatistic
p_value_f <- pf(f_test[1], f_test[2], f_test[3], lower.tail = FALSE)

cat("F-statistic:", round(f_test[1], 3), "\n")
cat("P-value:", round(p_value_f, 5), "\n\n")

if(p_value_f > 0.05) {
  cat("✓ NO rechazamos que todas las covariables sean independientes de letter\n")
  cat("  → La asignación parece 'tan buena como aleatoria'\n")
} else {
  cat("✗ Rechazamos independencia conjunta\n")
  cat("  → Hay evidencia de desbalance en al menos una variable\n")
}

cat("\n\nCONCLUSIÓN SOBRE LA ASIGNACIÓN DE CARTA:\n")
cat("="*60, "\n")

# aquí va el código de latex - RESPUESTA i

# =============================================================================
# RESUMEN DE HALLAZGOS (Partes a-f)
# =============================================================================

cat("\n\n", "="*80, "\n")
cat("RESUMEN DE HALLAZGOS - EJERCICIO 4 (Partes a-f)\n")
cat("="*80, "\n\n")

cat("1. DATOS Y ESTRUCTURA:\n")
cat("   ✓ Datos cargados y transformados a formato largo\n")
cat("   ✓", nrow(analysisdata), "observaciones después de eliminar NA\n\n")

cat("2. VALORES FALTANTES:\n")
if(abs(cor_test$estimate) > 0.1) {
  cat("   ✗ NO son completamente aleatorios (MNAR)\n")
  cat("   → Familias de menor ingreso menos propensas a reportar\n")
} else {
  cat("   ✓ Parecen ser aleatorios (MCAR)\n")
}
cat("\n")

cat("3. ESTANDARIZACIÓN:\n")
cat("   ✓ Puntajes estandarizados por año\n")
cat("   → Coeficientes se interpretan como effect sizes\n\n")

cat("4. SESGO DE SELECCIÓN:\n")
if(p_val < 0.05) {
  cat("   ✗ DETECTADO: Grupos no son comparables\n")
  if(diferencia > 0) {
    cat("   → Selección POSITIVA (participantes mejores)\n")
  } else {
    cat("   → Selección NEGATIVA (participantes peores)\n")
  }
} else {
  cat("   ✓ No detectado: Grupos parecen comparables\n")
}
cat("\n")

cat("5. CARTA RECORDATORIA:\n")
if(p_value_f > 0.05) {
  cat("   ✓ Asignación 'tan buena como aleatoria'\n")
  cat("   → Puede servir como variable instrumental\n")
} else {
  cat("   ✗ Evidencia de desbalance\n")
  cat("   → Cuidado al usar como instrumento\n")
}

cat("\n", "="*80, "\n")