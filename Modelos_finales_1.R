# -----------------------------
# MODELO 1 REFINADO - TODAS LAS MUNICIPALIDADES
# -----------------------------
library(plm)
library(dplyr)
library(readxl)
library(ggplot2)

# Cargar datos
datos <- read_excel("Dataset_DISTRITAL.xlsx", sheet = "Hoja1")

# -----------------------------
# 1) Filtrado: unidades con suficiente T (>=4 observaciones)
# -----------------------------
min_T <- 4
unidades_ok <- datos %>%
  group_by(UBIGEO) %>%
  summarise(T_obs = n_distinct(AÑO)) %>%
  filter(T_obs >= min_T) %>%
  pull(UBIGEO)

datos_m1 <- datos %>%
  filter(UBIGEO %in% unidades_ok)

cat("Unidades retenidas:", length(unique(datos_m1$UBIGEO)), "con T >=", min_T, "\n")

# -----------------------------
# 2) Construcción de variables clave (según recomendaciones)
# -----------------------------
datos_m1 <- datos_m1 %>%
  mutate(
    # Ciclo electoral
    CICLO_ELECTORAL_2 = ifelse(CICLO_ELECTORAL == 2, 1, 0),
    CICLO_ELECTORAL_3 = ifelse(CICLO_ELECTORAL == 3, 1, 0),
    CICLO_ELECTORAL_4 = ifelse(CICLO_ELECTORAL == 4, 1, 0),
    # Infraestructura: separar mayor / menor
    INFRA_MAYOR = (coalesce(ESTADIOS,0) + coalesce(COMPLEJOS_DEPORTIVOS,0) + coalesce(COLISEOS_DEPORTIVOS,0)),
    INFRA_MENOR = (coalesce(LOSAS_MULTIDEPORTIVAS,0) + coalesce(LOSAS_FULBITO,0) +
                     coalesce(LOSAS_VOLEY,0) + coalesce(LOSAS_BASQUET,0) + coalesce(PARQUES_ZONALES,0)
                   + coalesce(PISCINAS,0) + coalesce(GIMNASIOS,0)),
    # Capacidad administrativa – desagregada
    PROFESIONALES = 1000*(PROFESIONALES/exp(LOG_POB)),
    # Participación / gobernanza
    TRANSPARENCIA = coalesce(TRANSPARENCIA, 0),
    PARTICIPACION = (coalesce(PARTICIPA_PP, 0) + coalesce(JUNTA_DELEGADOS_ACTIVIDAD, 0)),  
    CAPACIDAD_PLAN = coalesce(PDMC, 0) + coalesce(PEI, 0), # planificación y PEI
    # Variables fiscales / sectoriales
    PROP_PIM_TRANSFERENCIAS_PP0101 = coalesce(PROP_PIM_TRANSFERENCIAS_PP0101, NA),
    PROP_PP0101_SOBRE_TOTAL_PIM = coalesce(PROP_PP0101_SOBRE_TOTAL_PIM, NA),
    NIVEL_EJECUCION_PP0101 = coalesce(NIVEL_EJECUCION_PP0101, NA),
    # Demografía
    POB_VARONES = coalesce(POB_VARONES, NA),
    POB_15_64 = coalesce(POB_15_64, NA),
    POB_URBANA = coalesce(POB_URBANA, NA)
  )

# Si quieres usar proporción de gasto como variable dependiente, descomenta la siguiente línea:
datos_m1 <- datos_m1 %>% mutate(PROP_GASTO_DEPORTE = PROP_GASTO_INFRAESTRUCTURA_DEPORTIVA_Y_RECREATIVA_PP0101)

# -----------------------------
# 3) Selección final de variables y limpieza de filas con NA críticas
# -----------------------------
vars_modelo <- c("UBIGEO", "AÑO",
                 "PROP_GASTO_DEPORTE", # dependiente actual en tu script
                 "CICLO_ELECTORAL_2", "CICLO_ELECTORAL_3", "CICLO_ELECTORAL_4",
                 "NIVEL_EJECUCION_PP0101",
                 "PROP_PIM_TRANSFERENCIAS_PP0101",
                 "PROP_PP0101_SOBRE_TOTAL_PIM",
                 "POB_VARONES", "POB_15_64", "POB_URBANA",
                 "PROFESIONALES", "TRANSPARENCIA",
                 "INFRA_MAYOR", "INFRA_MENOR",
                 "PARTICIPACION", "CAPACIDAD_PLAN")

# Mantener solo filas con al menos la dependiente y las variables esenciales no-NA
datos_m1 <- datos_m1 %>%
  select(all_of(vars_modelo)) %>%
  filter(!is.na(PROP_GASTO_DEPORTE)) %>%
  # opcional: eliminar filas con NA en controles clave (ajusta según disponibilidad)
  filter(!is.na(PROP_PIM_TRANSFERENCIAS_PP0101) | TRUE) # si quieres ser laxo, usa TRUE

# -----------------------------
# 4) Convertir a panel (pdata.frame) con orden consistente
# -----------------------------
datos_panel <- datos_m1 %>%
  arrange(UBIGEO, AÑO) %>%
  pdata.frame(index = c("UBIGEO", "AÑO"))

# chequeos rápidos
cat("Dimensiones panel:", dim(datos_panel), "\n")
cat("Unidades (N):", length(unique(index(datos_panel, "UBIGEO"))), "\n")
cat("T (promedio):", mean(table(index(datos_panel, "UBIGEO"))), "\n")

# -----------------------------
# 5) Especificación GMM parsimoniosa (instrumentación conservadora)
# -----------------------------
# Instrumentamos solo la variable dinámica (lag 1) con rezago 2 (evitamos 3:4 para no proliferar).
# No incluimos dummies de año (evitan singularidad en System GMM).
formula_pgmm <- as.formula(
  paste0("PROP_GASTO_DEPORTE ~ lag(PROP_GASTO_DEPORTE, 1) + ",
         "CICLO_ELECTORAL_2 + CICLO_ELECTORAL_3 + CICLO_ELECTORAL_4 + ",
         "PROP_PIM_TRANSFERENCIAS_PP0101 + PROP_PP0101_SOBRE_TOTAL_PIM + ",
         "POB_VARONES + POB_15_64 + POB_URBANA + PROFESIONALES + TRANSPARENCIA + ",
         "NIVEL_EJECUCION_PP0101 + INFRA_MAYOR + INFRA_MENOR + PARTICIPACION + CAPACIDAD_PLAN | ",
         "lag(PROP_GASTO_DEPORTE, 2:3) +  lag(PROP_PIM_TRANSFERENCIAS_PP0101, 2)")
)

cat("Fórmula GMM:\n"); print(formula_pgmm)

# -----------------------------
# 6) Estimación Difference GMM (d) con control de errores
# -----------------------------

run_pgmm_diff <- function(formula, data_panel,
                          effect = "individual",
                          model = "twosteps",
                          collapse = TRUE) {
  res <- tryCatch({
    pgmm(
      formula = formula,
      data = data_panel,
      effect = effect,
      model = model,
      transformation = "d",   # Difference GMM
      collapse = collapse
    )
  }, error = function(e) {
    message("Error en Difference GMM: ", e$message)
    return(NULL)
  })
  return(res)
}

# Estimar Difference GMM
modelo_diff <- run_pgmm_diff(formula_pgmm, datos_panel)

if (is.null(modelo_diff)) {
  stop("No se pudo estimar el modelo Difference GMM. Revisar datos, instrumentación o especificación.")
}

modelo_final <- modelo_diff
modelo_tipo <- "Difference GMM"

# -----------------------------
# 7) Resumen y diagnósticos
# -----------------------------
cat("MODELO EJECUTADO (tipo):", modelo_tipo, "\n")

s <- summary(modelo_final)
print(s)

# Pruebas de diagnóstico (si están disponibles)
if (!is.null(s$test)) {
  cat("Pruebas de diagnóstico (AR(1), AR(2), Sargan/Hansen):\n")
  print(s$test)
}

# -----------------------------
# 8) Extraer coeficientes de interés y gráfico (CICLO_ELECTORAL_2, 3 y 4)
# -----------------------------

coef_tab <- tryCatch(coef(summary(modelo_final)), error = function(e) NULL)
vars_ciclo <- c("CICLO_ELECTORAL_2", "CICLO_ELECTORAL_3", "CICLO_ELECTORAL_4")
if (!is.null(coef_tab) && all(vars_ciclo %in% rownames(coef_tab))) {
  df_plot <- data.frame(
    Ciclo = c("Segundo año",
              "Tercer año (preelectoral)",
              "Cuarto año (electoral)"),
    Estimate = coef_tab[vars_ciclo, "Estimate"],
    SE = coef_tab[vars_ciclo, "Std. Error"]
  )
  # Mostrar resultados numéricos
  cat("Coeficientes del ciclo electoral:\n")
  print(
    data.frame(
      Variable = vars_ciclo,
      Estimate = round(df_plot$Estimate, 4),
      Std_Error = round(df_plot$SE, 4)
    )
  )
  
  # Gráfico de barras con IC al 95%
  ggplot(df_plot, aes(x = Ciclo, y = Estimate)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_errorbar(
      aes(ymin = Estimate - 1.96 * SE,
          ymax = Estimate + 1.96 * SE),
      width = 0.2
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    theme_minimal() +
    labs(
      title = "Efecto del ciclo electoral sobre el gasto deportivo",
      subtitle = "Modelo 1 – Difference GMM",
      y = "Efecto marginal",
      x = "Fase del ciclo electoral"
    ) +
    theme(axis.text.x = element_text(angle = 15, hjust = 1))
  
} else {
  message("No se encontraron todas las dummies del ciclo electoral en la tabla de coeficientes.")
}