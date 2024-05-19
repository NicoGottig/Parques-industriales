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

# Nombro columnas y guardo
colnames(df) <- c("empresa", "descripcion", "web", "parque_1", paste0("parque_",seq(2:11)))
write_delim(df, "empresas:totales.txt", delim = "\t")

# Tratamiento para datos de parques ####
load("atributos_parques1.RData")
load("atributos_parques2.RData")
df <- bind_rows(atributos_parques, atributos_parques2)

# Utilizaremos un dataframe para las características y otro para los atributos
df$caracteristicas <- as.character(unlist(df$caracteristicas))
df$atributos <- as.character(unlist(df$atributos))

df <- df %>%
  separate(caracteristicas, into = paste0("caracteristica_", 1:8), sep = "\n", fill = "right")

df <- df %>%
  separate(atributos, into = paste0("atributos_", 1:59), sep = "\n", fill = "right")

naniar::miss_var_summary(df)

# Ahora tenemos que identificar las características y los atributos y hacer la ingeniería de factores
colnames(df)[2] <- "ubicacion"
df$ubicacion <- gsub("Ubicación", "",df$ubicacion)
df$ubicacion <- gsub("\\(", "",df$ubicacion)
df$ubicacion <- gsub("\\)", "",df$ubicacion)

colnames(df)[3] <- "provincia"
df$provincia <- gsub("Provincia ", "",df$provincia)

# Los codigos postales, algunos, contenian '(C)'
colnames(df)[4] <- "ciudad"
df$ciudad <- gsub("Ciudad ", "", df$ciudad)
df$ciudad <- gsub("\\(C\\)", "", df$ciudad)

colnames(df)[5] <- "cod_postal"
df$cod_postal <- gsub("Código postal ", "", df$cod_postal)

# Creamos un vector para el total de empresas radicadas
total_radicadas <- if_else(grepl("radicadas ", df$caracteristica_5) == TRUE, df$caracteristica_5, NA)
total_radicadas <- if_else(grepl("radicadas ", df$caracteristica_6) == TRUE, df$caracteristica_6, total_radicadas)
total_radicadas <- if_else(grepl("radicadas ", df$caracteristica_7) == TRUE, df$caracteristica_7, total_radicadas)
total_radicadas <- if_else(grepl("radicadas ", df$caracteristica_8) == TRUE, df$caracteristica_8, total_radicadas)
df$total_radicadas <- total_radicadas

# Creamos un vector para la werb
web <- if_else(grepl("Web ", df$caracteristica_5) == TRUE, df$caracteristica_5, NA)
web <- if_else(grepl("Web ", df$caracteristica_6) == TRUE, df$caracteristica_6, web)
web <- if_else(grepl("Web ", df$caracteristica_7) == TRUE, df$caracteristica_7, web)
web <- if_else(grepl("Web ", df$caracteristica_8) == TRUE, df$caracteristica_8, web)
df$web <- web

# Filtramos la columna calle
colnames(df)[6] <- "calle"
df <- df %>%
  mutate(calle = if_else(grepl("radicadas ", calle), NA, calle))
df <- df %>%
  mutate(calle = if_else(grepl("Web ", calle), NA, calle))

# Borramos columnas 7, 8 y 9
df[,7:9] <- NULL

# Ahora vamos a procesar los atributos. Como son muchas columnas armamos un for
colnames(df)

# Tipo de suelo
tipo_suelo <- rep(NA, nrow(df))
for (i in 7:65) {
  for(j in 1:nrow(df)){
    tipo_suelo[j] <- if_else(grepl("Tipo de Suelo", df[j,i]) == TRUE, df[j,i+1], tipo_suelo[j])
  }
}
df$tipo_suelo <- tipo_suelo

# Tipo de propiedad
tipo_propiedad <- rep(NA, nrow(df))
for (i in 7:65) {
  for(j in 1:nrow(df)){
    tipo_propiedad[j] <- if_else(grepl("Regimen de la Propiedad", df[j,i]) == TRUE, df[j,i+1], tipo_propiedad[j])
    
  }
}
df$tipo_propiedad <- tipo_propiedad

# Estado de actividad
estado_parque <- rep(NA, nrow(df))
for (i in 7:65) {
  for(j in 1:nrow(df)){
    estado_parque[j] <- if_else(grepl("Estado Actividad del Parque", df[j,i]) == TRUE, df[j,i+1], estado_parque[j])
    
  }
}
df$estado_parque <- estado_parque

# Superficie
superficie <- rep(NA, nrow(df))
for (i in 7:65) {
  for(j in 1:nrow(df)){
    superficie[j] <- if_else(grepl("Superficie Total del Predio", df[j,i]) == TRUE, df[j,i+1], superficie[j])
  }
}
df$superficie <- superficie

# Coordenadas
coord <- rep(NA, nrow(df))
for (i in 7:65) {
  for(j in 1:nrow(df)){
    coord[j] <- if_else(grepl("Localizacion - Coordenadas", df[j,i]) == TRUE, df[j,i+1], coord[j])
  }
}
df$coord <- coord

# Parque - web
web <- rep(NA, nrow(df))
for (i in 7:65) {
  for(j in 1:nrow(df)){
    web[j] <- if_else(grepl("Parque - WEB", df[j,i]) == TRUE, df[j,i+1], web[j])
  }
}
df$tiene_web <- web

# Registro - renpi
RENPI <- rep(NA, nrow(df))
for (i in 7:65) {
  for(j in 1:nrow(df)){
    RENPI[j] <- if_else(grepl("RENPI - Registro", df[j,i]) == TRUE, df[j,i+1], RENPI[j])
  }
}
df$registro_RENPI <- RENPI

# Año de creacion
anio <- rep(NA, nrow(df))
for (i in 7:65) {
  for(j in 1:nrow(df)){
    anio[j] <- if_else(grepl("Año de Creacion del Parque", df[j,i]) == TRUE, df[j,i+1], anio[j])
  }
}
df$anio_creacion <- anio

# Normativa
norma <- rep(NA, nrow(df))
for (i in 7:65) {
  for(j in 1:nrow(df)){
    norma[j] <- if_else(grepl("Normativa Provincial - Municipal", df[j,i])==TRUE, df[j,i+1], norma[j])
  }
}
df$normativa <- norma

# Ahora tenemos que tomar los elementos de infraestructura del parque que son varios. Buscamos la primera columna donde inician.

# Atributos 7 es la primera columna
# Hacemos un listado con los 610*54 elemrentos y vemos los que más se repiten. Esos serán las características de infraestructura.
df_selected <- df[, 13:67]
values_vector <- unlist(df_selected)
values_vector <- values_vector %>% table() %>% data.frame()

# Atributos de infraestructura:
# Energia electrica
# Calles internas pavimentadas
# Agua potable
# Alumbrado publico
# Cerramiento perimetral
# Desague pluvial
# Red de gas
# Telefonia internet banda ancha
# Desague sanitario - industrial
# Mantenimiento areas comunes
# Areas verdes
# Seguridad privada
# Telefono
# Estacionamieno automoviles
# Subestacion electrica
# Estacionamiento camiones
# Nomenclatura de calles
# Sistema contra incendio
# Iluminacion
# Señalizacion
# Oficinas administrativas
# Control de acceso personal y vehiculos
# Transporte urbano
# Forestacion perimetral
# Servicio de vigilancia camaras monitoreo
# Sala de eventos especiales
# Correo
# Agua Industrial
# Telefonia redes fibra optica
# Portal de acceso
# Balanza
# Garita de seguridad
# Planta tratamiento de agua
# Acceso pavimentado
# Planta tratamiento de efluentes
# Restaurante comedor
# Servicio de vigilancia fisico
# Aduana interior
# Banco
# Servicio medico asistencial
# Area comercial
# Parquizacion veredas y canteros
# Areas recreativas
# Capacitacion I+D+I Innovacion
# Incubadora empresas
# Calles internas afirmadas
# Cordon cuneta
# Estacion de servicio
# Red ferroviaria
# Aulas de capacitacion
# Deposito fiscal
# Universidades y Facultades
# Guarderia
# Vestuario con duchas y lockers
# Estacion de servicio

# Ahora necesitamos hacer un algoritmo que recorra todas las columnas de atributos buscando los de la lista e indicando
# en una columna si lo posee. Ejemplo para una variable:

# Crear una columna tiene.Agua.potable con valores iniciales de 0
df <- df %>% mutate(tiene.Agua.potable = 0)

# Usar map2 y mutate para verificar todas las columnas de interés
df <- df %>% mutate(tiene.Agua.potable = pmap_lgl(select(df, 7:65), ~ any(grepl("Agua potable", c(...)))) %>% as.integer())

# ahora hacemos lo mismo para las restantes variables
df <- df %>% mutate(tiene.Energia.electrica = 0)
df <- df %>% mutate(tiene.Calles.internas.pavimentadas = 0)
df <- df %>% mutate(tiene.Alumbrado.publico = 0)
df <- df %>% mutate(tiene.Cerramiento.perimetral = 0)
df <- df %>% mutate(tiene.Desague.pluvial = 0)
df <- df %>% mutate(tiene.Red.de.gas = 0)
df <- df %>% mutate(tiene.Telefonia.internet.banda.ancha = 0)
df <- df %>% mutate(tiene.Desague.sanitario.industrial = 0)
df <- df %>% mutate(tiene.Mantenimiento.areas.comunes = 0)
df <- df %>% mutate(tiene.Areas.verdes = 0)
df <- df %>% mutate(tiene.Seguridad.privada = 0)
df <- df %>% mutate(tiene.Telefono = 0)
df <- df %>% mutate(tiene.Estacionamieno.automoviles = 0)
df <- df %>% mutate(tiene.Subestacion.electrica = 0)
df <- df %>% mutate(tiene.Estacionamiento.camiones = 0)
df <- df %>% mutate(tiene.Nomenclatura.de.calles = 0)
df <- df %>% mutate(tiene.Sistema.contra.incendio = 0)
df <- df %>% mutate(tiene.Iluminacion = 0)
df <- df %>% mutate(tiene.Señalizacion = 0)
df <- df %>% mutate(tiene.Oficinas.administrativas = 0)
df <- df %>% mutate(tiene.Control.de.acceso.personal.y.vehiculos = 0)
df <- df %>% mutate(tiene.Transporte.urbano = 0)
df <- df %>% mutate(tiene.Forestacion.perimetral = 0)
df <- df %>% mutate(tiene.Servicio.de.vigilancia.camaras.monitoreo = 0)
df <- df %>% mutate(tiene.Sala.de.eventos.especiales = 0)
df <- df %>% mutate(tiene.Correo = 0)
df <- df %>% mutate(tiene.Agua.Industrial = 0)
df <- df %>% mutate(tiene.Telefonia.redes.fibra.optica = 0)
df <- df %>% mutate(tiene.Portal.de.acceso = 0)
df <- df %>% mutate(tiene.Balanza = 0)
df <- df %>% mutate(tiene.Garita.de.seguridad = 0)
df <- df %>% mutate(tiene.Planta.tratamiento.de.agua = 0)
df <- df %>% mutate(tiene.Acceso.pavimentado = 0)
df <- df %>% mutate(tiene.Planta.tratamiento.de.efluentes = 0)
df <- df %>% mutate(tiene.Restaurante.comedor = 0)
df <- df %>% mutate(tiene.Servicio.de.vigilancia.fisico = 0)
df <- df %>% mutate(tiene.Aduana.interior = 0)
df <- df %>% mutate(tiene.Banco = 0)
df <- df %>% mutate(tiene.Servicio.medico.asistencial = 0)
df <- df %>% mutate(tiene.Area.comercial = 0)
df <- df %>% mutate(tiene.Parquizacion.veredas.y.canteros = 0)
df <- df %>% mutate(tiene.Areas.recreativas = 0)
df <- df %>% mutate(tiene.Capacitacion.IDI.Innovacion = 0)
df <- df %>% mutate(tiene.Incubadora.empresas = 0)
df <- df %>% mutate(tiene.Calles.internas.afirmadas = 0)
df <- df %>% mutate(tiene.Cordon.cuneta = 0)
df <- df %>% mutate(tiene.Estacion.de.servicio = 0)
df <- df %>% mutate(tiene.Red.ferroviaria = 0)
df <- df %>% mutate(tiene.Aulas.de.capacitacion = 0)
df <- df %>% mutate(tiene.Deposito.fiscal = 0)
df <- df %>% mutate(tiene.Universidades.y.Facultades = 0)
df <- df %>% mutate(tiene.Guarderia = 0)
df <- df %>% mutate(tiene.Vestuario.con.duchas.y.lockers = 0)
df <- df %>% mutate(tiene.Estacion.de.servicio = 0)

# Completo
df <- df %>% mutate(tiene.Energia.electrica = pmap_lgl(select(df, 7:65), ~ any(grepl('Energia electrica', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Calles.internas.pavimentadas = pmap_lgl(select(df, 7:65), ~ any(grepl('Calles internas pavimentadas', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Alumbrado.publico = pmap_lgl(select(df, 7:65), ~ any(grepl('Alumbrado publico', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Cerramiento.perimetral = pmap_lgl(select(df, 7:65), ~ any(grepl('Cerramiento perimetral', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Desague.pluvial = pmap_lgl(select(df, 7:65), ~ any(grepl('Desague pluvial', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Red.de.gas = pmap_lgl(select(df, 7:65), ~ any(grepl('Red de gas', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Telefonia.internet.banda.ancha = pmap_lgl(select(df, 7:65), ~ any(grepl('Telefonia internet banda ancha', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Desague.sanitario.industrial = pmap_lgl(select(df, 7:65), ~ any(grepl('Desague sanitario - industrial', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Mantenimiento.areas.comunes = pmap_lgl(select(df, 7:65), ~ any(grepl('Mantenimiento areas comunes', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Areas.verdes = pmap_lgl(select(df, 7:65), ~ any(grepl('Areas verdes', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Seguridad.privada = pmap_lgl(select(df, 7:65), ~ any(grepl('Seguridad privada', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Telefono = pmap_lgl(select(df, 7:65), ~ any(grepl('Telefono', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Estacionamieno.automoviles = pmap_lgl(select(df, 7:65), ~ any(grepl('Estacionamieno automoviles', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Subestacion.electrica = pmap_lgl(select(df, 7:65), ~ any(grepl('Subestacion electrica', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Estacionamiento.camiones = pmap_lgl(select(df, 7:65), ~ any(grepl('Estacionamiento camiones', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Nomenclatura.de.calles = pmap_lgl(select(df, 7:65), ~ any(grepl('Nomenclatura de calles', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Sistema.contra.incendio = pmap_lgl(select(df, 7:65), ~ any(grepl('Sistema contra incendio', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Iluminacion = pmap_lgl(select(df, 7:65), ~ any(grepl('Iluminacion', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Señalizacion = pmap_lgl(select(df, 7:65), ~ any(grepl('Señalizacion', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Oficinas.administrativas = pmap_lgl(select(df, 7:65), ~ any(grepl('Oficinas administrativas', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Control.de.acceso.personal.y.vehiculos = pmap_lgl(select(df, 7:65), ~ any(grepl('Control de acceso personal y vehiculos', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Transporte.urbano = pmap_lgl(select(df, 7:65), ~ any(grepl('Transporte urbano', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Forestacion.perimetral = pmap_lgl(select(df, 7:65), ~ any(grepl('Forestacion perimetral', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Servicio.de.vigilancia.camaras.monitoreo = pmap_lgl(select(df, 7:65), ~ any(grepl('Servicio de vigilancia camaras monitoreo', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Sala.de.eventos.especiales = pmap_lgl(select(df, 7:65), ~ any(grepl('Sala de eventos especiales', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Correo = pmap_lgl(select(df, 7:65), ~ any(grepl('Correo', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Agua.Industrial = pmap_lgl(select(df, 7:65), ~ any(grepl('Agua Industrial', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Telefonia.redes.fibra.optica = pmap_lgl(select(df, 7:65), ~ any(grepl('Telefonia redes fibra optica', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Portal.de.acceso = pmap_lgl(select(df, 7:65), ~ any(grepl('Portal de acceso', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Balanza = pmap_lgl(select(df, 7:65), ~ any(grepl('Balanza', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Garita.de.seguridad = pmap_lgl(select(df, 7:65), ~ any(grepl('Garita de seguridad', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Planta.tratamiento.de.agua = pmap_lgl(select(df, 7:65), ~ any(grepl('Planta tratamiento de agua', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Acceso.pavimentado = pmap_lgl(select(df, 7:65), ~ any(grepl('Acceso pavimentado', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Planta.tratamiento.de.efluentes = pmap_lgl(select(df, 7:65), ~ any(grepl('Planta tratamiento de efluentes', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Restaurante.comedor = pmap_lgl(select(df, 7:65), ~ any(grepl('Restaurante comedor', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Servicio.de.vigilancia.fisico = pmap_lgl(select(df, 7:65), ~ any(grepl('Servicio de vigilancia fisico', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Aduana.interior = pmap_lgl(select(df, 7:65), ~ any(grepl('Aduana interior', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Banco = pmap_lgl(select(df, 7:65), ~ any(grepl('Banco', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Servicio.medico.asistencial = pmap_lgl(select(df, 7:65), ~ any(grepl('Servicio medico asistencial', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Area.comercial = pmap_lgl(select(df, 7:65), ~ any(grepl('Area comercial', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Parquizacion.veredas.y.canteros = pmap_lgl(select(df, 7:65), ~ any(grepl('Parquizacion veredas y canteros', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Areas.recreativas = pmap_lgl(select(df, 7:65), ~ any(grepl('Areas recreativas', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Capacitacion.IDI.Innovacion = pmap_lgl(select(df, 7:65), ~ any(grepl('Capacitacion I+D+I Innovacion', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Incubadora.empresas = pmap_lgl(select(df, 7:65), ~ any(grepl('Incubadora empresas', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Calles.internas.afirmadas = pmap_lgl(select(df, 7:65), ~ any(grepl('Calles internas afirmadas', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Cordon.cuneta = pmap_lgl(select(df, 7:65), ~ any(grepl('Cordon cuneta', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Estacion.de.servicio = pmap_lgl(select(df, 7:65), ~ any(grepl('Estacion de servicio', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Red.ferroviaria = pmap_lgl(select(df, 7:65), ~ any(grepl('Red ferroviaria', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Aulas.de.capacitacion = pmap_lgl(select(df, 7:65), ~ any(grepl('Aulas de capacitacion', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Deposito.fiscal = pmap_lgl(select(df, 7:65), ~ any(grepl('Deposito fiscal', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Universidades.y.Facultades = pmap_lgl(select(df, 7:65), ~ any(grepl('Universidades y Facultades', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Guarderia = pmap_lgl(select(df, 7:65), ~ any(grepl('Guarderia', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Vestuario.con.duchas.y.lockers = pmap_lgl(select(df, 7:65), ~ any(grepl('Vestuario con duchas y lockers', c(...)))) %>% as.integer())
df <- df %>% mutate(tiene.Estacion.de.servicio = pmap_lgl(select(df, 7:65), ~ any(grepl('Estacion de servicio', c(...)))) %>% as.integer())


# Eliminamos las columnas que no son importantes 

# Se quedan:
# nombre 1
# ubicacion 2
# provincia 3
# ciudad 4
# cod_postal 5
# calle 6
# 66:130
df_final <- df[,c(1:6, 66:130)]

# Le agregamos las empresas que faltan completar
load("enlaces_empresas.RData")
empresas_faltantes <- enlaces_empresas %>% 
  filter(nchar(enlace) > 48)

empresas_faltantes[, 3:69] <- NA
colnames(empresas_faltantes) <- c("nombre", "ubicacion", colnames(df_final)[3:69])
df_final <- rbind(df_final, empresas_faltantes)


# Guardar todo en xlsx ####
openxlsx::write.xlsx(df_final, "info_parques_completa.xlsx", delim = "\t")
openxlsx::write.xlsx(enlaces_empresas, "enlaces_parques_info.xlsx", delim = "\t")
openxlsx::write.xlsx(enlaces_empresas, "enlaces_parques_info.xlsx", delim = "\t")
openxlsx::write.xlsx(df, "empresas_individuales.xlsx", delim = "\t")

