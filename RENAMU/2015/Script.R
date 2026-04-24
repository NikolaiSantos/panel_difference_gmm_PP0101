# Cargar librerías necesarias
library(tidyverse)
library(readxl)
library(writexl)
library(fs)
library(stringr)  # Para funciones de manipulación de cadenas

# Función para corregir la columna idmunici
corregir_idmunici <- function(df) {
  if ("idmunici" %in% colnames(df)) {
    # Convertir a carácter si no lo es
    df$idmunici <- as.character(df$idmunici)
    
    # Eliminar espacios en blanco al inicio y final
    df$idmunici <- trimws(df$idmunici)
    
    # Rellenar con ceros a la izquierda para asegurar 6 dígitos
    df$idmunici <- str_pad(df$idmunici, width = 6, pad = "0", side = "left")
    
    # Verificar que todos los valores tengan exactamente 6 caracteres
    if (any(nchar(df$idmunici) != 6)) {
      warning("Algunos valores en idmunici no tienen exactamente 6 dígitos después de la corrección")
    }
  }
  return(df)
}

# Definir ruta base
#ruta_base <- "/home/nikolai/Dropbox/Trabajos/TESIS MAESTRIA/Tesis 2/Datasets/RENAMU/2015/"
ruta_base <- "D:/Dropbox/Trabajos/TESIS MAESTRIA/Tesis 2/Datasets/RENAMU/2015"

# Listar todas las carpetas en la ruta base
carpetas <- dir_ls(ruta_base, type = "directory")

# Inicializar dataframe base con las columnas necesarias
columnas_deseadas <- c("P19_5", "P20", "P21_2_T", "P25_1", "P25_2", "P64A_1", "P64A_2", "P64A_3",
                       "P64A_4", "P64A_5", "P64A_6", "P64A_7", "P64A_8", "P64A_9", "P64A_10", "P64A_11",
                       "P110_4", "P111", "P111_2")

# Buscar y cargar CDIR.csv con separador punto y coma
cdf_dir <- NULL
for (carpeta in carpetas) {
  archivos <- dir_ls(carpeta, regexp = "CDIR\\.csv$")
  if (length(archivos) > 0) {
    # Leer el archivo CSV con separador punto y coma
    cdf_dir <- read_delim(archivos[1], delim = ";", show_col_types = FALSE)
    
    # Corregir la columna idmunici
    cdf_dir <- corregir_idmunici(cdf_dir)
    
    break
  }
}

# Verificar si se encontró CDIR.csv
if (is.null(cdf_dir)) {
  stop("No se encontró el archivo CDIR.csv en ninguna carpeta")
}

# Verificar que CDIR.csv tenga la columna idmunici
if (!"idmunici" %in% colnames(cdf_dir)) {
  stop("El archivo CDIR.csv no contiene la columna 'idmunici'")
}

# Inicializar el dataframe resultado con las columnas de CDIR
resultado <- cdf_dir %>% 
  select(idmunici, catmuni)

# Convertir idmunici a carácter para asegurar compatibilidad (ya está corregido pero aseguramos)
resultado$idmunici <- as.character(resultado$idmunici)

# Crear un vector para registrar qué columnas se encontraron
columnas_encontradas <- character(0)

# Recorrer todas las carpetas y archivos para buscar las columnas deseadas
for (carpeta in carpetas) {
  # Listar todos los archivos CSV en la carpeta actual
  archivos_csv <- dir_ls(carpeta, regexp = "\\.csv$")
  
  for (archivo in archivos_csv) {
    # Saltar el archivo CDIR.csv que ya usamos como base
    if (basename(archivo) == "CDIR.csv") next
    
    tryCatch({
      # Leer el archivo CSV con separador punto y coma
      df_temp <- read_delim(archivo, delim = ";", show_col_types = FALSE)
      
      # Corregir la columna idmunici en este dataframe
      df_temp <- corregir_idmunici(df_temp)
      
      # Verificar si tiene columna idmunici
      if ("idmunici" %in% colnames(df_temp)) {
        # Convertir idmunici a carácter (aunque ya está corregido)
        df_temp$idmunici <- as.character(df_temp$idmunici)
        
        # Verificar qué columnas deseadas están presentes en este archivo
        cols_presentes <- intersect(columnas_deseadas, colnames(df_temp))
        
        if (length(cols_presentes) > 0) {
          # Registrar las columnas encontradas
          columnas_encontradas <- union(columnas_encontradas, cols_presentes)
          
          # Seleccionar solo las columnas necesarias: idmunici y las columnas deseadas presentes
          df_seleccionado <- df_temp %>% 
            select(idmunici, all_of(cols_presentes))
          
          # Combinar con el resultado usando left_join
          resultado <- resultado %>% 
            left_join(df_seleccionado, by = "idmunici")
        }
      }
    }, error = function(e) {
      message(sprintf("Error al procesar el archivo %s: %s", archivo, e$message))
    })
  }
}

# Verificar qué columnas deseadas no se encontraron
columnas_faltantes <- setdiff(columnas_deseadas, columnas_encontradas)
if (length(columnas_faltantes) > 0) {
  message(sprintf("Las siguientes columnas no se encontraron en ningún archivo: %s", 
                  paste(columnas_faltantes, collapse = ", ")))
  
  # Añadir columnas faltantes con valores NA
  for (col in columnas_faltantes) {
    resultado[[col]] <- NA
  }
}

# Asegurar que todas las columnas deseadas estén en el orden especificado
columnas_ordenadas <- c("idmunici", "catmuni", columnas_deseadas)
resultado <- resultado %>% select(all_of(columnas_ordenadas))

str(resultado)

# Exportar el resultado
write_xlsx(resultado, path = file.path(ruta_base, "RENAMU_2015.xlsx"))
write.csv(resultado, file = file.path(ruta_base, "RENAMU_2015.csv"),
          row.names = FALSE, fileEncoding = "UTF-8", na = "")

# Mostrar resumen del resultado
cat(sprintf("\nProceso completado exitosamente.\n"))
cat(sprintf("Se procesaron %d carpetas\n", length(carpetas)))
cat(sprintf("Se encontraron %d de %d columnas deseadas\n", 
            length(columnas_encontradas), length(columnas_deseadas)))
cat(sprintf("Dimensiones del dataframe final: %d filas x %d columnas\n", 
            nrow(resultado), ncol(resultado)))
cat(sprintf("Archivo exportado como: %s\n", file.path(ruta_base, "RENAMU_2015.xlsx")))

# Mostrar las primeras filas del resultado para verificar la corrección de idmunici
cat("\nMuestra de los primeros valores de idmunici (deben tener 6 dígitos):\n")
print(head(resultado$idmunici))

# Verificar si hay algún valor que no tenga 6 dígitos
valores_incorrectos <- resultado$idmunici[nchar(resultado$idmunici) != 6]
if (length(valores_incorrectos) > 0) {
  warning(sprintf("Atención: %d valores en idmunici no tienen exactamente 6 dígitos", 
                  length(valores_incorrectos)))
  print(head(valores_incorrectos))
} else {
  cat("\nVerificación exitosa: Todos los valores de idmunici tienen exactamente 6 dígitos.\n")
}

# Mostrar las primeras filas del resultado
print(head(resultado))
