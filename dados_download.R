
## Etapa: DOWNLOAD DE DADOS PÚBLICOS ##


## Pacotes utilizados
library(tidyverse)
library(purrr)


## IDD (2014-2019)

  # Downloads
  # Informe ano inicial e ano final. Os limites são 2014-2019
  ano_ini <- 2014
  ano_fim <- 2019
  ano <- c(2014:2019)
  
  # Destino dos arquivos
  
  map(ano_ini:ano_fim, function(ano){
    
    for (i in 2014:2019){
      path<- paste0("http://download.inep.gov.br/microdados/microdados_idd_",
                    ano,".zip")
      
    }
        download.file(path, destfile = paste0('raw/idd/microdados_idd_', 
                                          ano, '.zip'), mode='wb')
     })
  

## ENADE(2014-2019)
  
  # Downloads
  # Informe ano inicial e ano final. Os limites são 2004-2019
  ano <- c(2014:2016, 2018, 2019) # Exceto 2017 apresenta um caminho diferente
  
  # Destino dos arquivos

  map(ano, function(ano){
    
    for (i in ano){
      path<- paste0("http://download.inep.gov.br/microdados/Enade_Microdados/microdados_enade_",
                    ano,".zip")
      
    }
    download.file(path, destfile = paste0('raw/enade/microdados_enade_', 
                                          ano, '.zip'), mode='wb')
  })
  
  # 2017 apresenta um caminho diferente
  path<- paste0("https://download.inep.gov.br/microdados/Enade_Microdados/microdados_Enade_2017_portal_2018.10.09.zip")
  download.file(path, destfile = paste0('raw/enade/microdados_enade_2017.zip'), mode='wb')
       

               
               
               
               
               
  
