library(tidyverse)

# Tratamiento de datos para empresas ####
load(file = "empresas_totales.RData")

# Se parte de datos enlistados por fila. Cada fila es una empresa. Algunas tienen web y otras no.
empresas <- empresas_totales

# Lista de empresas
empresas_lista <- as.list(empresas)

lista_separada <- empresas_lista %>%
  map(~ str_split(., "\n")[[1]])

# Encontrar la longitud máxima para las sublistas
max_length <- max(map_int(lista_separada, length))

# Asegurar que todas las sublistas tengan la misma longitud rellenando con NA si es necesario
empresas_uniformes <- lista_separada %>%
  map(~ c(., rep(NA, max_length - length(.))))

# Convertir la lista uniforme en un tibble
tibble_data <- empresas_uniformes %>%
  map_df(~ set_names(., paste0("Col", 1:max_length)))

# Filtramos todas las filas de páginas
df <- tibble_data %>% 
  filter(!grepl("Página", Col1))

# En la columna descripción eliminamos la palabra y los paréntesis
df$Col2 <- gsub("Descripción ", "", df$Col2) 
df$Col2 <- gsub("\\(", "", df$Col2) 
df$Col2 <- gsub("\\) ", "", df$Col2) 

# La columna 5 es la columna adonde se encuentra el parque. La completamos
df$Col5 <- if_else(grepl("Web", df$Col3) == TRUE, df$Col5, df$Col4)

# Ahora si podemos eliminar la palabra web
df$Col3 <- gsub("Web ", "", df$Col3)
df$Col3 <- if_else(grepl("Radicada", df$Col3) == TRUE, NA, df$Col3)

# La columna cuatro no sirve
df$Col4 <- NULL

# Tratamiento para datos de parques ####