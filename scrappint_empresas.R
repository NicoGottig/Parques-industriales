library(tidyverse)
library(RSelenium)

# Scrapping para empresas

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
  
  print(paste0("Inicio página ", i, " - %", round(100*i/419, 2)))
  
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
  print("Cambiando página...")
  Sys.sleep(2)
  
}


write.csv(empresas_totales, "empresas_totales.csv")













