library(tidyverse)
library(RSelenium)

# Scrapping para empresas ####

# Configuro el servidor
rdriver <- rsDriver(browser = "firefox")
remDr <- rdriver[["client"]]

# Activo navegación
web_empresas <- "https://parquesindustriales.com.ar/empresas"
remDr$navigate(web_empresas)

# Para cada empresa almacenos sus datos. Hay empresas
# que no tienen WEB o que tienen mas de DOS RESIDENCIAS

# Ejecución del bot
empresas_totales <- NULL
empresas_xpag <- NULL
emp_i <- '//*[@id="body"]/div[3]/div/div[1]/div[1]/div[3]/div/section/div['
boton <- '//*[@id="body"]/div[3]/div/div[1]/div[1]/div[3]/div/section/div[11]/button[3]'

# 420 páginas son 419 clicks...
for (i in 1:10) {
  
  
  for (j in 1:11) {
    elemento <- paste0(emp_i, j, ']')
    
    resultado <- tryCatch({
      suppressMessages({
        elemento_texto <- remDr$findElement('xpath', elemento)
        texto <- elemento_texto$getElementText() %>% unlist(.) %>% ifelse(length(.) == 0, NA, .)
        print(texto %>% str_split("\n")  %>% .[[1]] %>% head(1))
      })
    }, 
    error = function(e) {
      NA_character_
    }
    )
    
    empresas_xpag <- rbind(empresas_xpag, texto)
    
  }
  
  # Uno datos
  empresas_totales <- rbind(empresas_totales, empresas_xpag)
  empresas_xpag <- NULL
  
  # pulsamos el boton y esperamos dos segundos
  boton_siguiente <- remDr$findElement('xpath', boton) # elemento boton
  boton_siguiente$sendKeysToElement(list(key = "enter"))
  print(paste0("Realizada página ", i, " - %", round(100*i/419, 2)))
  print("Cambiando página...")
  
  Sys.sleep(2)
  
}

save(empresas_totales, file = "empresas_totales.RData")


# Scrapping de parques ####

# Configuro el servidor
rdriver <- rsDriver(browser = "firefox")
remDr <- rdriver[["client"]]

# Activo navegación
web_parques <- "https://parquesindustriales.com.ar/listado-parques"
remDr$navigate(web_parques)

# Primero vamos a necesitar un listado de parques con sus respectivos links (62 paginas)
enlaces_empresas <- NULL
for (i in 1:10) {
  
  
  for (j in 1:15) {
    i_parque <- paste0('//*[@id="body"]/div[3]/div/div[1]/div[1]/div[3]/div/section/div[', j, ']/button')
    i_enlace <- paste0('//*[@id="body"]/div[3]/div/div[1]/div[1]/div[3]/div/section/div[', j, ']/div/p/a')
    
    resultado <- tryCatch({
      suppressMessages({
        
        nombre_parque <- remDr$findElement('xpath',  i_parque)
        texto <- nombre_parque$getElementText() %>% unlist(.) 
        print(texto)
        
        enlace <- remDr$findElement('xpath',  i_enlace)
        link <- enlace$getElementAttribute("href")[[1]]
        print(link)
        
        
      })
    }, 
    
    error = function(e) {
      NA_character_
      return(NULL)
    }
    )
    
    tmp <- data.frame(parque = texto, enlace = link)
    enlaces_empresas <- rbind(enlaces_empresas, tmp)
    tmp <- NULL
  }
  
  # Cambio de página
  boton_siguiente <- remDr$findElement('xpath', '//*[@id="body"]/div[3]/div/div[1]/div[1]/div[3]/div/section/div[11]/button[3]') # elemento boton
  boton_siguiente$sendKeysToElement(list(key = "enter"))
  print(paste0("Realizado página ", i, "- %", round(100*i/62,2)))
  print("Cambiando página....")
  Sys.sleep(3)
}

# Limpieza rápida de la base
enlaces_empresas <- enlaces_empresas %>% 
  filter(parque != "◀ Anterior" & nchar(parque) > 1)
save(enlaces_empresas, file = "enlaces_empresas.RData")

# Esto lo hacemos para las pruebas
enlaces_empresas <- enlaces_empresas %>% 
  filter(nchar(enlace) == 48)

# Ahora extraemos la información de cada parque específico (esto estará muy sujeto a errores)
df <- enlaces_empresas
df_total <- NULL

for (i in 1:nrow(df)) {
  
  # cargo la pagina
  web <- df$enlace[i] # acordarse el i
  remDr$navigate(web)
  print("Cargando página....")
  Sys.sleep(2)
  
  # Extraigo nombre
  n <- remDr$findElement('xpath', '//*[@id="body"]/div[3]/div/div[1]/div[1]/div[3]/div/h1')
  
  # Características
  caracteristicas <- remDr$findElement('xpath', '//*[@id="body"]/div[3]/div/div[1]/div[1]/div[3]/div/div[1]')
  
  # amplio info
  ampliar <- remDr$findElement('xpath', '//*[@id="show_attrib"]') 
  ampliar$sendKeysToElement(list(key = "enter"))
  print("Buscando atributos....")
  Sys.sleep(2)
  
  # Atributos
  atributos <- remDr$findElement('xpath', '//*[@id="attrib"]/table')
  
  # Extraigo texto de los elementos
  nombre <- n$getElementText() %>% unlist(.) 
  car <- caracteristicas$getElementText() %>% unlist(.) 
  atr <- atributos$getElementText() %>% unlist(.) 
  
  # Armo fila del dataframe
  tmp <- data.frame(
    nombre = nombre,
    caracteristicas = car,
    atributos = atr
  )
  
  # uno al df total
  df_total <- rbind(df_total, tmp)
  print(paste("Página", i, "finalizada. Último parque:"))
  print(tail(df_total[,1],1))

}

save(df_total, file = "atributos_parques.RData")

