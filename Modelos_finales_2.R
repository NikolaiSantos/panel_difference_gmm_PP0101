# -----------------------------
# MODELO 2.1 - MUNICIPALIDADES PROVINCIALES
# MODELO 2.2 - MUNICIPALIDADES DISTRITALES
# -----------------------------
library(plm)
library(dplyr)
library(readxl)
library(ggplot2)

# Cargar datos
datos <- read_excel("Dataset_DISTRITAL.xlsx", sheet = "Hoja1")

# -----------------------------
# IDENTIFICAR TIPO DE MUNICIPALIDAD
# -----------------------------
datos <- datos %>%
  mutate(ES_PROVINCIAL = substr(UBIGEO, nchar(UBIGEO) - 1, nchar(UBIGEO)) == "01")

datos_provinciales <- datos %>% filter(ES_PROVINCIAL)
datos_distritales   <- datos %>% filter(!ES_PROVINCIAL)

cat("Municipalidades provinciales:", n_distinct(datos_provinciales$UBIGEO), "\n")
cat("Municipalidades distritales:",   n_distinct(datos_distritales$UBIGEO), "\n")

# -----------------------------
# FUNCIÓN PARA ESTIMAR MODELO GMM POR TIPO
# -----------------------------
estimar_modelo_gmm <- function(datos_input, nombre_modelo, min_T = 4) {
  
  cat("\n==========================================\n")
  cat("ESTIMANDO:", nombre_modelo, "\n")
  cat("==========================================\n")
  
  # 1) Filtrado: unidades con suficiente T
  unidades_ok <- datos_input %>%
    group_by(UBIGEO) %>%
    summarise(T_obs = n_distinct(AÑO)) %>%
    filter(T_obs >= min_T) %>%
    pull(UBIGEO)
  
  datos_m <- datos_input %>%
    filter(UBIGEO %in% unidades_ok)
  
  cat("Unidades retenidas:", length(unique(datos_m$UBIGEO)), "con T >=", min_T, "\n")
  
  # 2) Construcción de variables
  datos_m <- datos_m %>%
    mutate(
      # Ciclo electoral
      CICLO_ELECTORAL_2 = ifelse(CICLO_ELECTORAL == 2, 1, 0),
      CICLO_ELECTORAL_3 = ifelse(CICLO_ELECTORAL == 3, 1, 0),
      CICLO_ELECTORAL_4 = ifelse(CICLO_ELECTORAL == 4, 1, 0),
      # Infraestructura
      INFRA_MAYOR = (coalesce(ESTADIOS,0) + coalesce(COMPLEJOS_DEPORTIVOS,0) + coalesce(COLISEOS_DEPORTIVOS,0)),
      INFRA_MENOR = (coalesce(LOSAS_MULTIDEPORTIVAS,0) + coalesce(LOSAS_FULBITO,0) +
                       coalesce(LOSAS_VOLEY,0) + coalesce(LOSAS_BASQUET,0) + coalesce(PARQUES_ZONALES,0)
                     + coalesce(PISCINAS,0) + coalesce(GIMNASIOS,0)),
      # Capacidad administrativa
      PROFESIONALES = 1000*(PROFESIONALES/exp(LOG_POB)),
      # Participación / gobernanza
      TRANSPARENCIA = coalesce(TRANSPARENCIA, 0),
      PARTICIPACION = (coalesce(PARTICIPA_PP, 0) + coalesce(JUNTA_DELEGADOS_ACTIVIDAD, 0)),  
      CAPACIDAD_PLAN = coalesce(PDMC, 0) + coalesce(PEI, 0),
      # Variables fiscales
      PROP_PIM_TRANSFERENCIAS_PP0101 = coalesce(PROP_PIM_TRANSFERENCIAS_PP0101, NA),
      PROP_PP0101_SOBRE_TOTAL_PIM = coalesce(PROP_PP0101_SOBRE_TOTAL_PIM, NA),
      NIVEL_EJECUCION_PP0101 = coalesce(NIVEL_EJECUCION_PP0101, NA),
      # Demografía
      POB_VARONES = coalesce(POB_VARONES, NA),
      POB_15_64 = coalesce(POB_15_64, NA),
      POB_URBANA = coalesce(POB_URBANA, NA),
      # Variable dependiente
      PROP_GASTO_DEPORTE = PROP_GASTO_INFRAESTRUCTURA_DEPORTIVA_Y_RECREATIVA_PP0101
    )
  
  # 3) Selección de variables
  vars_modelo <- c("UBIGEO", "AÑO",
                   "PROP_GASTO_DEPORTE",
                   "CICLO_ELECTORAL_2", "CICLO_ELECTORAL_3", "CICLO_ELECTORAL_4",
                   "NIVEL_EJECUCION_PP0101",
                   "PROP_PIM_TRANSFERENCIAS_PP0101",
                   "PROP_PP0101_SOBRE_TOTAL_PIM",
                   "POB_VARONES", "POB_15_64", "POB_URBANA",
                   "PROFESIONALES", "TRANSPARENCIA",
                   "INFRA_MAYOR", "INFRA_MENOR",
                   "PARTICIPACION", "CAPACIDAD_PLAN")
  
  datos_m <- datos_m %>%
    select(all_of(vars_modelo)) %>%
    filter(!is.na(PROP_GASTO_DEPORTE))
  
  # 4) Convertir a panel
  datos_panel <- datos_m %>%
    arrange(UBIGEO, AÑO) %>%
    pdata.frame(index = c("UBIGEO", "AÑO"))
  
  cat("Dimensiones panel:", dim(datos_panel), "\n")
  cat("Unidades (N):", length(unique(index(datos_panel, "UBIGEO"))), "\n")
  cat("T (promedio):", mean(table(index(datos_panel, "UBIGEO"))), "\n")
  
  # 5) Fórmula GMM
  formula_pgmm <- as.formula(
    paste0("PROP_GASTO_DEPORTE ~ lag(PROP_GASTO_DEPORTE, 1) + ",
           "CICLO_ELECTORAL_2 + CICLO_ELECTORAL_3 + CICLO_ELECTORAL_4 + ",
           "PROP_PIM_TRANSFERENCIAS_PP0101 + PROP_PP0101_SOBRE_TOTAL_PIM + ",
           "POB_VARONES + POB_15_64 + POB_URBANA + PROFESIONALES + TRANSPARENCIA + ",
           "NIVEL_EJECUCION_PP0101 + INFRA_MAYOR + INFRA_MENOR + PARTICIPACION + CAPACIDAD_PLAN | ",
           "lag(PROP_GASTO_DEPORTE, 2:3) + lag(PROP_PIM_TRANSFERENCIAS_PP0101, 2)")
  )
  
  # 6) Estimación
  modelo <- tryCatch({
    pgmm(
      formula = formula_pgmm,
      data = datos_panel,
      effect = "individual",
      model = "twosteps",
      transformation = "d",
      collapse = TRUE
    )
  }, error = function(e) {
    message("Error en estimación: ", e$message)
    return(NULL)
  })
  
  if (is.null(modelo)) {
    cat("No se pudo estimar el modelo para", nombre_modelo, "\n")
    return(NULL)
  }
  
  # 7) Resumen
  cat("\n--- RESULTADOS:", nombre_modelo, "---\n")
  s <- summary(modelo)
  print(s)
  
  if (!is.null(s$test)) {
    cat("\nPruebas de diagnóstico:\n")
    print(s$test)
  }
  
  return(list(modelo = modelo, datos_panel = datos_panel, nombre = nombre_modelo))
}

# -----------------------------
# ESTIMAR AMBOS MODELOS
# -----------------------------
modelo_2_1 <- estimar_modelo_gmm(datos_provinciales, "Modelo 2.1 - Municipalidades provinciales de todo el país")
modelo_2_2 <- estimar_modelo_gmm(datos_distritales, "Modelo 2.2 - Municipalidades distritales de todo el país")

# -----------------------------
# FUNCIÓN PARA GRAFICAR RESULTADOS
# -----------------------------
graficar_ciclo_electoral <- function(resultado_modelo) {
  
  if (is.null(resultado_modelo)) {
    message("Modelo no disponible para graficar")
    return(NULL)
  }
  
  modelo <- resultado_modelo$modelo
  nombre <- resultado_modelo$nombre
  
  coef_tab <- tryCatch(coef(summary(modelo)), error = function(e) NULL)
  vars_ciclo <- c("CICLO_ELECTORAL_2", "CICLO_ELECTORAL_3", "CICLO_ELECTORAL_4")
  
  if (!is.null(coef_tab) && all(vars_ciclo %in% rownames(coef_tab))) {
    df_plot <- data.frame(
      Ciclo = c("Segundo año",
                "Tercer año (preelectoral)",
                "Cuarto año (electoral)"),
      Estimate = coef_tab[vars_ciclo, "Estimate"],
      SE = coef_tab[vars_ciclo, "Std. Error"]
    )
    
    cat("\n--- Coeficientes del ciclo electoral:", nombre, "---\n")
    print(
      data.frame(
        Variable = vars_ciclo,
        Estimate = round(df_plot$Estimate, 4),
        Std_Error = round(df_plot$SE, 4),
        Significancia = ifelse(abs(df_plot$Estimate / df_plot$SE) > 1.96, "Sí (p<0.05)", "No")
      )
    )
    
    p <- ggplot(df_plot, aes(x = Ciclo, y = Estimate)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      geom_errorbar(
        aes(ymin = Estimate - 1.96 * SE,
            ymax = Estimate + 1.96 * SE),
        width = 0.2
      ) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      theme_minimal() +
      labs(
        title = paste("Efecto del ciclo electoral -", nombre),
        subtitle = "Difference GMM",
        y = "Efecto marginal",
        x = "Fase del ciclo electoral"
      ) +
      theme(axis.text.x = element_text(angle = 15, hjust = 1))
    
    print(p)
    return(df_plot)
  } else {
    message("No se encontraron coeficientes del ciclo electoral")
    return(NULL)
  }
}

# -----------------------------
# GENERAR GRÁFICOS
# -----------------------------
if (!is.null(modelo_2_1)) {
  coef_2_1 <- graficar_ciclo_electoral(modelo_2_1)
}

if (!is.null(modelo_2_2)) {
  coef_2_2 <- graficar_ciclo_electoral(modelo_2_2)
}

# -----------------------------
# COMPARACIÓN ENTRE MODELOS
# -----------------------------
if (!is.null(modelo_2_1) && !is.null(modelo_2_2)) {
  cat("\n==========================================\n")
  cat("COMPARACIÓN DE RESULTADOS\n")
  cat("==========================================\n")
  
  cat("\nNúmero de observaciones:\n")
  cat("Provinciales:", nrow(modelo_2_1$datos_panel), "\n")
  cat("Distritales:", nrow(modelo_2_2$datos_panel), "\n")
  
  cat("\nNúmero de municipalidades:\n")
  cat("Provinciales:", length(unique(index(modelo_2_1$datos_panel, "UBIGEO"))), "\n")
  cat("Distritales:", length(unique(index(modelo_2_2$datos_panel, "UBIGEO"))), "\n")
}