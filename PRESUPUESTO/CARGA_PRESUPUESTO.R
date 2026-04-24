# ---------------------------
# Cargar librerías necesarias
# ---------------------------
library(vroom)      # Para lectura eficiente de archivos grandes
library(dplyr)      # Para manipulación de datos
library(stringr)    # Para manipulación de strings
library(curl)       # Para descargas controladas
library(purrr)      # Para programación funcional
library(tidyr)      # Para pivotear tablas
library(writexl)    # Para exportar a Excel

# ---------------------------
# URLs de los archivos
# ---------------------------
urls <- c(
  "https://fs.datosabiertos.mef.gob.pe/datastorefiles/2013-Gasto-Devengado.csv",
  "https://fs.datosabiertos.mef.gob.pe/datastorefiles/2014-Gasto-Devengado.csv",
  "https://fs.datosabiertos.mef.gob.pe/datastorefiles/2015-Gasto-Devengado.csv",
  "https://fs.datosabiertos.mef.gob.pe/datastorefiles/2016-Gasto-Devengado.csv",
  "https://fs.datosabiertos.mef.gob.pe/datastorefiles/2017-Gasto-Devengado.csv",
  "https://fs.datosabiertos.mef.gob.pe/datastorefiles/2018-Gasto-Devengado.csv",
  "https://fs.datosabiertos.mef.gob.pe/datastorefiles/2019-Gasto-Devengado.csv",
  "https://fs.datosabiertos.mef.gob.pe/datastorefiles/2020-Gasto-Devengado.csv",
  "https://fs.datosabiertos.mef.gob.pe/datastorefiles/2021-Gasto-Devengado.csv",
  "https://fs.datosabiertos.mef.gob.pe/datastorefiles/2022-Gasto-Devengado.csv",
  "https://fs.datosabiertos.mef.gob.pe/datastorefiles/2023-Gasto-Devengado.csv",
  "https://fs.datosabiertos.mef.gob.pe/datastorefiles/2024-Gasto-Devengado-Diario.csv"
)

# ---------------------------
# 1. Identificar nombre exacto de la columna "NIVEL_GOBIERNO_NOMBRE"
# ---------------------------
identificar_nombre_columna <- function(url_muestra) {
  temp_file <- tempfile(fileext = ".csv")
  h <- new_handle()
  handle_setopt(h, range = "0-100000")  # Solo descargar primeros KB
  curl_download(url_muestra, temp_file, handle = h)
  encabezados <- vroom(temp_file, num_threads = 1, n_max = 0)
  nombres_columnas <- names(encabezados)
  idx <- detect_index(nombres_columnas, ~ str_detect(.x, "NIVEL_GOBIERNO_NOMBRE"))
  if (idx == 0) {
    stop("No se encontró una columna de nivel de gobierno en: ", url_muestra)
  }
  unlink(temp_file)
  return(nombres_columnas[idx])
}

nombre_columna_nivel_gobierno <- identificar_nombre_columna(urls[1])
cat("Columna identificada:", nombre_columna_nivel_gobierno, "\n")

# ---------------------------
# 2. Procesar archivos
# ---------------------------
PRESUPUESTO <- NULL

for (url in urls) {
  anio <- str_extract(url, "\\d{4}")
  cat("Procesando año", anio, "...\n")
  temp_file <- tempfile(fileext = ".csv")
  
  # Descargar archivo completo
  tryCatch({
    curl_download(url, temp_file)
    
    # Leer solo columnas necesarias
    datos <- vroom(
      temp_file,
      col_select = all_of(c(
        nombre_columna_nivel_gobierno,
        "DEPARTAMENTO_EJECUTORA_NOMBRE",
        "PROVINCIA_EJECUTORA_NOMBRE",
        "DISTRITO_EJECUTORA_NOMBRE",
        "FUENTE_FINANCIAMIENTO_NOMBRE",
        "RUBRO_NOMBRE", "MONTO_PIM",
        "MONTO_DEVENGADO_ANUAL"
      )),
      num_threads = 4,
      progress = FALSE
    ) %>%
      filter(.data[[nombre_columna_nivel_gobierno]] == "GOBIERNOS LOCALES") %>%
      mutate(ANO_EJE = as.integer(anio)) %>%
      select(
        ANO_EJE,
        DEPARTAMENTO_EJECUTORA_NOMBRE,
        PROVINCIA_EJECUTORA_NOMBRE,
        DISTRITO_EJECUTORA_NOMBRE,
        FUENTE_FINANCIAMIENTO_NOMBRE,
        RUBRO_NOMBRE,
        MONTO_PIM,
        MONTO_DEVENGADO_ANUAL
      )
    
    # Añadir al dataset principal
    if (is.null(PRESUPUESTO)) {
      PRESUPUESTO <- datos
    } else {
      PRESUPUESTO <- bind_rows(PRESUPUESTO, datos)
    }
    
    cat("  Año", anio, "procesado con", nrow(datos), "filas.\n")
    
    # Limpiar
    rm(datos)
    gc(verbose = FALSE)
    unlink(temp_file)
    
  }, error = function(e) {
    cat("  Error en año", anio, ":", e$message, "\n")
    unlink(temp_file)
  })
}

# ---------------------------
# Limpieza de datos
# ---------------------------
PRESUPUESTO <- PRESUPUESTO %>%
  mutate(DEPARTAMENTO_EJECUTORA_NOMBRE = ifelse(DEPARTAMENTO_EJECUTORA_NOMBRE == "PROVINCIA CONSTITUCIONAL DEL CALLAO", 
                                                "CALLAO", 
                                                DEPARTAMENTO_EJECUTORA_NOMBRE))
gc(verbose = FALSE)

# ---------------------------
# Datasets de devengado
# ---------------------------
PRESUPUESTO_DEVENGADO_TOTAL <- PRESUPUESTO %>%
                              filter(
                                MONTO_DEVENGADO_ANUAL != 0,
                                !is.na(MONTO_DEVENGADO_ANUAL)
                              )

PRESUPUESTO_DEVENGADO_TOTAL <- PRESUPUESTO_DEVENGADO_TOTAL %>%
                                  group_by(ANO_EJE, DEPARTAMENTO_EJECUTORA_NOMBRE, PROVINCIA_EJECUTORA_NOMBRE, DISTRITO_EJECUTORA_NOMBRE) %>%
                                  summarise(
                                    MONTO_DEVENGADO = sum(MONTO_DEVENGADO_ANUAL, na.rm = TRUE)
                                  ) %>%
                                  arrange(ANO_EJE, DEPARTAMENTO_EJECUTORA_NOMBRE, PROVINCIA_EJECUTORA_NOMBRE, DISTRITO_EJECUTORA_NOMBRE)
#write_xlsx(PRESUPUESTO_DEVENGADO_TOTAL, "E:/Dropbox/Trabajos/TESIS MAESTRIA/Tesis 2/Datasets/PRESUPUESTO/PRESUPUESTO_DEVENGADO_TOTAL.xlsx")
write_xlsx(PRESUPUESTO_DEVENGADO_TOTAL, "/home/nikolai/Dropbox/Trabajos/TESIS MAESTRIA/Tesis 2/Datasets/PRESUPUESTO/PRESUPUESTO_DEVENGADO_TOTAL.xlsx")

PRESUPUESTO_DEVENGADO_RUBRO <- PRESUPUESTO %>%
                                filter(
                                  MONTO_DEVENGADO_ANUAL != 0,
                                  !is.na(MONTO_DEVENGADO_ANUAL)
                                )

PRESUPUESTO_DEVENGADO_RUBRO <- PRESUPUESTO_DEVENGADO_RUBRO %>%
                                select(ANO_EJE, DEPARTAMENTO_EJECUTORA_NOMBRE, PROVINCIA_EJECUTORA_NOMBRE, DISTRITO_EJECUTORA_NOMBRE, RUBRO_NOMBRE, MONTO_DEVENGADO_ANUAL) %>%
                                pivot_wider(
                                  names_from = RUBRO_NOMBRE,
                                  values_from = MONTO_DEVENGADO_ANUAL,
                                  values_fn = sum,
                                  values_fill = 0
                                )
#write_xlsx(PRESUPUESTO_DEVENGADO_RUBRO, "E:/Dropbox/Trabajos/TESIS MAESTRIA/Tesis 2/Datasets/PRESUPUESTO/PRESUPUESTO_DEVENGADO_RUBRO.xlsx")
write_xlsx(PRESUPUESTO_DEVENGADO_RUBRO, "/home/nikolai/Dropbox/Trabajos/TESIS MAESTRIA/Tesis 2/Datasets/PRESUPUESTO/PRESUPUESTO_DEVENGADO_RUBRO.xlsx")

# ---------------------------
# Datasets de PIM
# ---------------------------
PRESUPUESTO_PIM_TOTAL <- PRESUPUESTO %>%
                          filter(
                            MONTO_PIM != 0,
                            !is.na(MONTO_PIM)
                          )
                        
                        PRESUPUESTO_PIM_TOTAL <- PRESUPUESTO_PIM_TOTAL %>%
                          group_by(ANO_EJE, DEPARTAMENTO_EJECUTORA_NOMBRE, PROVINCIA_EJECUTORA_NOMBRE, DISTRITO_EJECUTORA_NOMBRE) %>%
                          summarise(
                            MONTO_PIM = sum(MONTO_PIM, na.rm = TRUE)
                          ) %>%
                          arrange(ANO_EJE, DEPARTAMENTO_EJECUTORA_NOMBRE, PROVINCIA_EJECUTORA_NOMBRE, DISTRITO_EJECUTORA_NOMBRE)
#write_xlsx(PRESUPUESTO_PIM_TOTAL, "E:/Dropbox/Trabajos/TESIS MAESTRIA/Tesis 2/Datasets/PRESUPUESTO/PRESUPUESTO_PIM_TOTAL.xlsx")
write_xlsx(PRESUPUESTO_PIM_TOTAL, "/home/nikolai/Dropbox/Trabajos/TESIS MAESTRIA/Tesis 2/Datasets/PRESUPUESTO/PRESUPUESTO_PIM_TOTAL.xlsx")

# ---------------------------
# 4. Guardar como archivos
# ---------------------------
saveRDS(PRESUPUESTO, file = "PRESUPUESTO.rds")