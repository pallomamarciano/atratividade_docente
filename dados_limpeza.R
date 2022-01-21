########################################
#### Projeto "Atratividade Docente" ####
####### LEPES e Profissão Docente ######
########################################


## Etapa: LIMPEZA DE DADOS PÚBLICOS ##


## Pacotes utilizados
library(tidyverse)
library(archive)
library(writexl)


## ENADE (2014-2019)
  
  # Ler os arquivos com os dados sem dezipar o conteúdo da pasta
  d.2014 <- read_csv2(archive_read(paste0('raw/enade/microdados_enade_2014.zip'), 
                                    '3.DADOS/MICRODADOS_ENADE_2014.txt'))    
  d.2015 <- read_csv2(archive_read(paste0('raw/enade/microdados_enade_2015.zip'), 
                                    '3.DADOS/MICRODADOS_ENADE_2015.txt')) 
  d.2016 <- read_csv2(archive_read(paste0('raw/enade/microdados_enade_2016.zip'), 
                                   '2.DADOS/microdados_enade_2016.csv'))
  d.2017 <- read_csv2(archive_read(paste0('raw/enade/microdados_enade_2017.zip'), 
                                    '3.DADOS/MICRODADOS_ENADE_2017.txt')) 
  d.2018 <- read_csv2(archive_read(paste0('raw/enade/microdados_enade_2018.zip'), 
                                   '2018/3.DADOS/microdados_enade_2018.txt'))
  d.2019 <- read_csv2(archive_read(paste0('raw/enade/microdados_enade_2019.zip'), 
                                    '3.DADOS/microdados_enade_2019.txt')) 
  
  # Selecionar variáveis de interesse em cada ano
  var.2014 <- c('NU_ANO', 'CO_GRUPO', 'CO_CURSO', 'CO_MODALIDADE', 'CO_MUNIC_CURSO',
                'CO_UF_CURSO', 'NU_IDADE', 'TP_SEXO', 'ANO_IN_GRAD',
                'TP_PRES', 'TP_PR_GER', 'NT_GER', 'NT_FG', 'NT_OBJ_FG', 'NT_DIS_FG',
                'NT_FG_D1', 'NT_FG_D1_PT', 'NT_FG_D1_CT', 'NT_FG_D2','NT_FG_D2_PT',
                'NT_FG_D2_CT', 'NT_CE', 'NT_OBJ_CE', 'NT_DIS_CE', 'NT_CE_D1',
                'NT_CE_D2', 'NT_CE_D3', 'QE_I02', 'QE_I04', 'QE_I05', 'QE_I08')
  
  var.2019 <- tibble(c('NU_ANO', 'CO_GRUPO', 'CO_CURSO', 'CO_MODALIDADE', 'CO_MUNIC_CURSO',
                  'CO_UF_CURSO', 'NU_IDADE', 'TP_SEXO', 'ANO_FIM_EM', 'ANO_IN_GRAD',
                  'TP_PRES', 'TP_PR_GER', 'NT_GER', 'NT_FG', 'NT_OBJ_FG', 'NT_DIS_FG',
                  'NT_FG_D1', 'NT_FG_D1_PT', 'NT_FG_D1_CT', 'NT_FG_D2','NT_FG_D2_PT',
                  'NT_FG_D2_CT', 'NT_CE', 'NT_OBJ_CE', 'NT_DIS_CE', 'NT_CE_D1',
                  'NT_CE_D2', 'NT_CE_D3', 'QE_I02', 'QE_I04', 'QE_I05', 'QE_I08'))

  a <- d.2014 %>% 
    select(all_of(var.2014)) %>% 
    mutate(ANO_FIM_EM = NA)
  
  b <- d.2015 %>% 
    select(all_of(var.2014)) %>% 
    mutate(ANO_FIM_EM = NA)
  
  c <- d.2016 %>% 
    rename_with(toupper) %>% 
    select(c('NU_ANO', 'CO_GRUPO', 'CO_CURSO', 'CO_MODALIDADE', 'CO_MUNIC_CURSO',
                  'CO_UF_CURSO', 'NU_IDADE', 'TP_SEXO', 'ANO_IN_GRAD',
                  'TP_PRES', 'TP_PR_GER', 'NT_GER', 'NT_FG', 'NT_OBJ_FG', 'NT_DIS_FG',
                  'NT_FG_D1', 'NT_FG_D1_PT', 'NT_FG_D1_CT', 'NT_FG_D2','NT_FG_D2_PT',
                  'NT_FG_D2_CT', 'NT_CE', 'NT_OBJ_CE', 'NT_DIS_CE', 'NT_CE_D1',
                  'NT_CE_D2', 'NT_CE_D3', 'QE_I2', 'QE_I4', 'QE_I5', 'QE_I8')) %>% 
    mutate(ANO_FIM_EM = NA)
  
  d <- d.2017 %>% 
    select(all_of(var.2019))
  
  e <- d.2018 %>% 
    select(all_of(var.2019))
  
  f <- d.2019 %>% 
    select(all_of(var.2019))
  
  
  
  # Salvar banco com todos os anos
  write_csv(db, 'clean/enade.csv')
  write_xlsx(as.data.frame(names(db)), 'metadata/enade_dic.xlxs')
  
  
## ENEM (2014-2019)
  
  # Ler os arquivos com os dados sem dezipar o conteúdo da pasta
  d.2014 <- read_delim(archive_read(paste0('raw/idd/microdados_idd_2014.zip'), 
                                    'Dados/microdados_idd_2014.csv'))    
  d.2015 <- read_delim(archive_read(paste0('raw/idd/microdados_idd_2015.zip'), 
                                    'microdados_idd_2015/2.Dados/microdados_idd_2015.csv')) 
  d.2016 <- read_csv2(archive_read(paste0('raw/idd/microdados_idd_2016.zip'), 
                                   '2.Dados/microdados_idd_2016.csv'))
  d.2017 <- read_delim(archive_read(paste0('raw/idd/microdados_idd_2017.zip'),
                                   '3.DADOS/MICRODADOS_IDD_2017.txt')) 
  d.2018 <- read_csv2(archive_read(paste0('raw/idd/microdados_idd_2018.zip'), 
                                   '3.DADOS/microdados_idd_2018.txt'))
  d.2019 <- read_delim(archive_read(paste0('raw/idd/microdados_idd_2019.zip'), 
                                    'microdados_idd_2019/2019/3.DADOS/microdados_idd_2019.txt',
  ),col_types = cols('...1' = col_skip())) 
  
  # Renomear variáveis e unir bancos similares
  # 2017-2019 
  varlist <- names(d.2019)
  
  a <- d.2019
  
  b <- d.2018
  names(b) <- varlist
  
  c <- d.2017
  names(c) <- varlist
  
  db.1 <- rbind(c, b, a)
  
  # Variáveis não presentes nos anos anteriores a 2017 e sem informação relevante
  # para a análise
  db.1 <- db.1 %>% 
    select(-c('IN_REGULAR', 'TP_INSCRICAO_ADM'))
  
  # 2015-2016
  varlist.2 <- names(d.2016)
  
  d <- d.2016
  
  e <- d.2015
  colnames(e) <- varlist.2
  
  db.2 <- rbind(e, d)
  
  db.2 <- db.2 %>% 
    rename(CO_MUNIC_CURSO = CO_MUNIC,
           ANO_IN_GRAD = NU_ANO_INICIO_GRADUACAO,
           VL_NT_ENEM_CN = ENEM_NT_CN,
           VL_NT_ENEM_CH = ENEM_NT_CH,
           VL_NT_ENEM_LC = ENEM_NT_LC,
           VL_NT_ENEM_MT = ENEM_NT_MT) %>% 
    mutate(TP_INSCRICAO = case_when(TP_INSCRICAO == 1 ~ 0,
                                    TP_INSCRICAO == 0 ~ 1))
  
  # 2014
  f <- d.2014
  
  #varlist.5 <- names(f)
  
  # Renomear variáveis com base no dicionário dos dados mais recentes (2019)
  f <- f %>%
    rename_with(toupper) %>% 
    select(-c(CO_UF_CURSO, CO_REGIAO_CURSO)) %>% 
    rename(CO_CATEGAD = CO_CATAD,
           CO_ORGACAD = CO_ORGAC,
           ANO_ENEM = ID_ENEM,
           VL_NT_ENEM_CN = ENEM_NT_CN,
           VL_NT_ENEM_CH = ENEM_NT_CH,
           VL_NT_ENEM_LC = ENEM_NT_LC,
           VL_NT_ENEM_MT = ENEM_NT_MT) %>% 
    mutate(CO_MODALIDADE = NA)
  
  
  # Unir todos os anos
  db <- rbind(f, db.2, db.1)
  
  db <- db %>% 
    rename_with(tolower) %>% 
    # Ajustar com os valores das variáveis para 2019
    mutate(tp_inscricao = case_when(tp_inscricao == 0 ~ 1,
                                    TRUE ~ 0),
           co_categad = case_when(co_categad == 1 ~ 1,
                                  co_categad == 2 ~ 2,
                                  co_categad == 3 ~ 3,
                                  co_categad == 4 ~ 4,
                                  co_categad == 5 ~ 5,
                                  co_categad == 93 ~ 1,
                                  co_categad == 115 ~ 2,
                                  co_categad == 116 ~ 3,
                                  co_categad == 118 ~ 4,
                                  co_categad == 121 ~ 5,
                                  co_categad == 10001 ~ 2,
                                  co_categad == 10002 ~ 1,
                                  co_categad == 10003 ~ 3,
                                  co_categad == 10004 ~ 4,
                                  co_categad == 10005 ~ 4,
                                  co_categad == 10006 ~ 4,
                                  co_categad == 10007 ~ 5,
                                  co_categad == 10008 ~ 5,
                                  TRUE ~ 5))

           
  # Salvar banco com todos os anos
  write_csv(db, 'clean/enem.csv')
  
  
  

               
               
               
               
               
               
  