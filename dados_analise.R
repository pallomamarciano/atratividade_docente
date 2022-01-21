########################################
#### Projeto "Atratividade Docente" ####
####### LEPES e Profissão Docente ######
########################################


###### Etapa: ANÁLISE PRELIMINAR #######


# Pacotes utilizados -------------------
library(tidyverse)
library(stargazer)
library(binsreg)
library(vtable)
library(plyr)
library(ggpubr)
library(wesanderson)


# ENEM (2014-2019) ---------------------

  # Abrir banco
  enem <- read_csv('clean/enem.csv')
  
  
  # Pergunta norteadora 1:
  # Qual o desempenho médio nas avaliações do ensino médio e no ensino 
  # superior dos estudantes que concluíram cursos de pedagogia e licenciatura
  # relativamente aos demais egressos?
  
    # Codebook grupos de cursos
    grupo <- read.csv("metadata/co_grupo.csv", encoding="LATIN1", sep=";")
    
    # Juntando bancos
    db <- enem %>% 
      left_join(grupo) %>% 
      # Classificando as áreas de graduação (licenciatura ou não)
      mutate(lic = case_when(grepl('Licenciatura', area_enade) ~ 'Licenciatura',
                                   TRUE ~ 'Bacharelado'),
             # Calculando a nota geral do ENEM
             enem = ((vl_nt_enem_cn + vl_nt_enem_ch + vl_nt_enem_lc + 
                        vl_nt_enem_mt)/4),
             # Categoria do agrupamento de médias
             media = case_when(enem < 450 ~ "0-450",
                               enem > 450 & enem < 500 ~ '451-500',
                               enem > 500 & enem < 550 ~ '501-550',
                               enem > 551 & enem < 600 ~ '551-600',
                               TRUE ~ 'mais de 600'))
    
    
    # Distribuição da Nota Geral do ENEM para 2014-2016 (ciclo.1) --------------
      ciclo.1 <- db %>%
        filter(nu_ano %in% (2014:2016)) %>% 
        select(c(nu_ano, co_grupo, co_categad, co_munic_curso,
                 ano_in_grad, ano_enem, enem, vl_nt_enem_cn,
                 vl_nt_enem_ch, vl_nt_enem_lc, vl_nt_enem_mt, lic,
                 area_enade, area_nome, area_lic, analise, graph, media)) 
      
    
      # Comparação Geral 
        # Estatísticas Descritivas
        sumtable(ciclo.1,
                   vars = c('enem', 'vl_nt_enem_cn', 'vl_nt_enem_ch',
                            'vl_nt_enem_lc', 'vl_nt_enem_mt'),
                   out = 'csv',
                   file = 'output/tables/sum_ciclo1',
                   summ = list(c('notNA(x)', 'mean(x)', 'median(x)', 'sd(x)',
                                 'min(x)', 'pctile(x)[25]',
                                 'pctile(x)[75]', 'max(x)')),
                   summ.names = list(c('N','Média', 'Mediana', 'Desvio Padrão',
                                       'Mínimo', 'Percentil (25%)',
                                       'Percentil (75%)', 'Máximo')),
                   group.long = FALSE,
                   digits = 2, fixed.digits = TRUE,
                   labels = c('Nota Geral', 'Ciências da Natureza',
                              'Ciências Humanas e Sociais',
                              'Linguagens', 'Matemática'),
                   fit.page = '.9\\textwidth',
                   note.align = 'l')
        
        # Histograma com curva de densidade
        
        
        titulo <- "Distribuição da Nota Geral do ENEM dos Participantes da Prova do ENADE (ciclo 2014-2016)" 
        fonte <- 'Fonte: Lepes com microdados do ENADE-IDD - INEP/MEC.'
        
        ciclo.1 %>% 
          ggplot(aes(x=enem)) + 
          geom_histogram(aes(y=..density..),      # Histograma com densidade no eixo y
                         binwidth=10,
                         colour="black", fill="white") +
          geom_density(alpha=.25, fill="#FF6666") + # Sobreposição com gráfico de densidade transparente
          labs(title = titulo, # Informações
               caption = fonte,
               x ="Média aritmétrica das quatro áreas",
               y = "Densidade") +
          theme_minimal() +
          theme(text = element_text(size = 10, family = "serif"), # Formatação
                plot.title = element_text(hjust =0.5, face = "bold"),
                plot.caption = element_text(hjust = -0.15, vjust = 0.2),
                axis.title.x = element_text(vjust = 0.2)
          )
        
      # Comparação Licenciaturas e Bacharelados 
      # Arquivo com a tabela completa: 'output/tables/licenc_bach_descritiva_2014-2016.xlsx
        # Estatísticas Descritivas
        # 
        # Média
        ciclo.1 %>% 
          #filter(analise == 1) %>%
          sumtable(ciclo.1,
                   vars = c('enem', 'vl_nt_enem_cn', 'vl_nt_enem_ch',
                            'vl_nt_enem_lc', 'vl_nt_enem_mt'),
                   out = 'csv',
                   file = 'output/tables/sum_ciclo1_media.csv',
                   summ = list(c('mean(x)')),
                   summ.names = list(c('Média')),
                   group = 'area_nome',
                   group.long = FALSE,
                   add.median = FALSE,
                   digits = 2, fixed.digits = TRUE,
                   labels = c('Nota Geral', 'Ciências da Natureza',
                              'Ciências Humanas e Sociais',
                              'Linguagens', 'Matemática'))
        
        # N
        ciclo.1 %>% 
          #filter(analise == 1) %>%
          sumtable(ciclo.2,
                   vars = c('enem'),
                   out = 'csv',
                   file = 'output/tables/sum_ciclo1_n.csv',
                   summ = list(c('notNA(x)')),
                   summ.names = list(c('N')),
                   group = 'area_nome',
                   group.long = FALSE,
                   add.median = FALSE,
                   digits = 2, fixed.digits = TRUE,
                   labels = c('Nota Geral'))
        
        
        # # Histograma com curva de densidade e média
        #   # Média dos dois grupos 
        #   mean <- drop_na(ciclo.1) %>% 
        #     ddply("lic", summarise, grp.mean=mean(enem))
        #   head(media)
        #   
        #   # Gráfico de densidade com média
        #   
        #   titulo <- "Distribuição da Nota Geral - ENEM (2014-2016)"
        #   
        #   ciclo.1 %>% 
        #     ggplot(aes(x = enem, colour = lic, fill = lic)) + 
        #     geom_density(alpha=0.4) + 
        #     geom_vline(data = media, aes(xintercept = grp.mean, colour = lic),
        #                linetype = "dashed"
        #                )+
        #     labs(title = titulo, # Informações
        #          caption = fonte,
        #          x = "Média aritmétrica das quatro áreas",
        #          y = "Densidade",
        #          ) +
        #     theme_minimal() +
        #     theme(text = element_text(size = 10, family = "serif"), # Formatação
        #           plot.title = element_text(hjust =.5, face = "bold"),
        #           plot.caption = element_text(hjust = -.15, vjust = .2),
        #           axis.title.x = element_text(vjust = .2),
        #           legend.title = element_blank())        
        
         
            # Gráfico de barras
            
          titulo <- "Concluintes Ensino Superior (2014-2016) - Áreas Selecionadas: Percentual formandos 
                     com notas no ENEM abaixo de diferentes cortes"  
          cores <- c('royalblue', 'light blue', 'yellow', 'orange', 'red')
          
          
              # Geral
              ciclo.1 %>% 
                # Filtrando pelas áreas listadas na proposta 
                filter(graph == 1) %>% 
                # Agrupando por área e pelos grupos de médias de notas
                group_by(area_nome, media) %>% 
                # Calculando a porcentagem de cada média nos cursos numa nova variável
                dplyr::summarise(n = n(),
                                 ) %>% 
                mutate(freq = formattable::percent(n/sum(n))) %>%
                # Variáveis do gráfico com 'forcats' para reverter as ordem dos valores
                ggplot(aes(y = freq, x = fct_reorder2(area_lic, media, freq),
                           fill = forcats::fct_rev(media))) +
                # Tipo de gráfico
                geom_bar(position = 'fill', stat = "identity") +
                # Texto para as porcentagens calculadas
                geom_text(aes(label = paste0(sprintf("%1.1f", freq*100),"%")),
                          position =  position_fill(vjust = 0.5),
                          size = 3) +
                # Escola de porcentagem no eixo 'y'
                scale_y_continuous(label = scales::percent) +
                # Definição manual das cores dos grupos de médias definidos
                scale_fill_manual(values = cores) +
                # Título do gráfico e fonte dos dados
                labs(title = titulo, 
                      caption = fonte) +
                # Estilo do gráfico
                theme_minimal() +
                # Formatação dos textos do gráfico
                # Todo gráfico
                theme(text = element_text(size = 10), # Formatação
                      # Título
                      plot.title = element_text(hjust =.5, face = "bold"),
                      # Fonte
                      plot.caption = element_text(hjust = 0.0),
                      plot.caption.position = 'plot',
                      # Eixo 'x'
                      axis.text.x = element_text(angle = 45, hjust = 1),
                      # Para retirar o título do eixo 'x'
                      axis.title.x = element_blank(),
                      # Para retirar o título do eixo 'y'
                      axis.title.y = element_blank(),
                      # Para retirar o título da legenda
                      legend.title = element_blank(),
                      # Definir a posição da legenda no gráfico
                      legend.position = 'bottom')
              
              # Rede Pública
              
              titulo <- "Concluintes Ensino Superior (2014-2016) - Áreas Selecionadas: Percentual formandos com notas 
                     no ENEM abaixo de diferentes cortes por Dependência Administrativa da IES"
              theme <- 
                theme_minimal() +
                # Formatação dos textos do gráfico
                # Todo gráfico
                theme(text = element_text(size = 10), # Formatação
                      # Título centralizado
                      plot.title = element_text(hjust =.5, face = "bold"),
                      # Fonte à esquerda da parte inferior do gráfico
                      plot.caption = element_text(hjust = 0.0),
                      plot.caption.position = 'plot',
                      # Eixo 'x' rótulos com ângulo de 45° à direita
                      axis.text.x = element_text(angle = 45, hjust = 1),
                      # Para retirar o título do eixo 'x'
                      axis.title.x = element_blank(),
                      # Para retirar o título do eixo 'y'
                      axis.title.y = element_blank(),
                      # Para retirar o título da legenda
                      legend.title = element_blank(),
                      # Definir a posição da legenda no gráfico
                      legend.position = 'bottom',
                      # Centralização subtítulo
                      plot.subtitle = element_text(hjust=0.5))
              
              ciclo.1 %>% 
                # Filtrando pelas áreas listadas na proposta 
                filter(graph == 1,
                       co_categad %in% 1:3) %>% 
                # Agrupando por área e pelos grupos de médias de notas
                group_by(area_nome, media) %>% 
                # Calculando a porcentagem de cada média nos cursos numa nova variável
                dplyr::summarise(n = n(),
                ) %>% 
                mutate(freq = formattable::percent(n/sum(n))) %>%
                # Variáveis do gráfico com 'forcats' para reverter as ordem dos valores
                ggplot(aes(y = freq, x = fct_reorder2(area_lic, media, freq), 
                           fill = forcats::fct_rev(media))) +
                # Tipo de gráfico
                geom_bar(position = 'fill', stat = "identity") +
                # Texto para as porcentagens calculadas
                geom_text(aes(label = paste0(sprintf("%1.1f", freq*100),"%")),
                          position =  position_fill(vjust = 0.5),
                          size = 3) +
                # Escola de porcentagem no eixo 'y'
                scale_y_continuous(label = scales::percent) +
                # Definição manual das cores dos grupos de médias definidos
                scale_fill_manual(values = cores) +
                # Título do gráfico e fonte dos dados
                labs(title = titulo,
                     subtitle = 'Rede Pública',
                     caption = fonte) +
                # Estilo do gráfico
                theme
              
              # Rede Privada
              ciclo.1 %>% 
                # Filtrando pelas áreas listadas na proposta 
                filter(graph == 1,
                       co_categad %in% 4:5) %>% 
                # Agrupando por área e pelos grupos de médias de notas
                group_by(area_nome, media) %>% 
                # Calculando a porcentagem de cada média nos cursos numa nova variável
                dplyr::summarise(n = n(),
                ) %>% 
                mutate(freq = formattable::percent(n/sum(n))) %>%
                # Variáveis do gráfico com 'forcats' para reverter as ordem dos valores
                ggplot(aes(y = freq, x = fct_reorder2(area_lic, media, freq),
                           fill = forcats::fct_rev(media))) +
                # Tipo de gráfico
                geom_bar(position = 'fill', stat = "identity") +
                # Texto para as porcentagens calculadas
                geom_text(aes(label = paste0(sprintf("%1.1f", freq*100),"%")),
                          position =  position_fill(vjust = 0.5),
                          size = 3) +
                # Escola de porcentagem no eixo 'y'
                scale_y_continuous(label = scales::percent) +
                # Definição manual das cores dos grupos de médias definidos
                scale_fill_manual(values = cores) +
                # Título do gráfico e fonte dos dados
                labs(title = titulo,
                     subtitle = 'Rede Privada',
                     caption = fonte) +
                # Estilo do gráfico
                theme
              
              # Nota maior que 600 no ENEM
              
              titulo <- "Percentual de Concluintes no ensino superior (2014-2016) com nota acima de 600 no Enem"
            
              theme <- 
              # Estilo do gráfico
              theme_minimal() +
                # Formatação dos textos do gráfico
                # Todo gráfico
                theme(text = element_text(size = 10), # Formatação
                      # Título centralizado
                      plot.title = element_text(face = "bold", hjust =.5),
                      # Subtítulo centralizado
                      plot.subtitle = element_text(hjust=0.5),
                      # Fonte à esquerda da parte inferior do gráfico
                      plot.caption = element_text(hjust = 0.0),
                      plot.caption.position = 'plot',
                      # Eixo 'x' rótulos com ângulo de 45° à direita
                      axis.text.x = element_text(),
                      # Para retirar o título do eixo 'x'
                      axis.title.x = element_blank(),
                      # Para retirar o título do eixo 'y'
                      axis.title.y = element_blank(),
                      # Para retirar o título da legenda
                      legend.title = element_blank(),
                      # Definir a posição da legenda no gráfico
                      legend.position = 'bottom',
                      panel.grid.minor = element_blank(),
                      panel.grid.major.x = element_line())  # Vertical minor grid lines
                
              
              d <- ciclo.1 %>% 
                # Filtrando pelas áreas listadas na proposta 
                #filter(analise == 1) %>% 
                # Agrupando por área e pelos grupos de médias de notas
                group_by(area_nome) %>% 
                # Calculando o número total de alunos por área
                dplyr::summarise(total = n())
              
              d1 <- ciclo.1 %>% 
                # Filtrando pelas áreas listadas na proposta e se obtive nota maior que 600 no ENEM
                filter(#analise == 1,
                       enem >= 600) %>% 
                # Calculando a porcentagem de alunos concluintes com nota maior que 600 no ENEM
                group_by(area_nome, lic) %>%
                dplyr::summarise(mais.600 = n()) %>% 
                left_join(d) %>% 
                mutate(freq = formattable::percent(mais.600/total))
              
                # Variáveis do gráfico com 'x' em ordem alfabética
                d1 %>% 
                  #filter(row_number(desc(freq)) <= 30) %>% 
                  ggplot(aes(y = (freq), x = reorder(area_nome, freq))) +
                  # Tipo de gráfico
                  geom_bar(stat = "identity", position = "dodge",
                           aes(fill = lic)) +
                  # Escola de porcentagem no eixo 'y'
                  scale_y_continuous(label = scales::percent, limits = c(0, 1),
                                     breaks = seq(0, 1, by = .2)) +
                  scale_fill_manual(values = c('grey34', 'royalblue')) +
                  # Texto para as porcentagens calculadas
                  geom_text(aes(label = freq), hjust = -0.1, size = 2.5) +
                  # Título do gráfico e fonte dos dados
                  labs(title = titulo, 
                       caption = fonte) +
                  # Estilo do gráfico
                  theme +
                  # Mudar a direção das barras
                  coord_flip()
                
                
              # # Coorte de 10% com as maiores médias no ENEM de cada curso
              #   
              #   d1 <- ciclo.1 %>% 
              #     # Filtrando se obtive nota maior que 600 no ENEM
              #     filter(enem >= 600) %>% 
              #     # Calculando a porcentagem de alunos concluintes com maiores
              #     # notas
              #     group_by(area_nome, lic) %>%
              #     dplyr::summarise(mais.600 = n()) %>% 
              #     left_join(d) %>% 
              #     mutate(freq = formattable::percent(mais.600/total))
              #   
              #   # Variáveis do gráfico com 'x' em ordem alfabética
              #   d1 %>% 
              #     filter(row_number(desc(freq)) <= 30) %>% 
              #     ggplot(aes(y = (freq), x = reorder(area_nome, freq))) +
              #     # Tipo de gráfico
              #     geom_bar(stat = "identity", position = "dodge",
              #              aes(fill = lic)) +
              #     # Escola de porcentagem no eixo 'y'
              #     scale_y_continuous(label = scales::percent, limits = c(0, 1),
              #                        breaks = seq(0, 1, by = .2)) +
              #     scale_fill_manual(values = c('grey34', 'royalblue')) +
              #     # Texto para as porcentagens calculadas
              #     geom_text(aes(label = freq), hjust = -0.1, size = 2.5) +
              #     # Título do gráfico e fonte dos dados
              #     labs(title = titulo,
              #          #subtitle = "30 primeiros colocados de cada curso", 
              #          caption = fonte) +
              #     # Estilo do gráfico
              #     theme +
              #     # Mudar a direção das barras
              #     coord_flip()
              #   
               
            
                # # Boxplot
            # ciclo.1 <- drop_na(ciclo.1) %>% 
            # filter(co_grupo %in% c(licen, dir, ch, eng, tec, med, saude)) %>%
            # mutate(comp = case_when(co_grupo %in% ch ~ 'Ciências Humanas',
            #                         co_grupo %in% dir ~ 'Direito',
            #                         co_grupo %in% eng ~ 'Engenharias',
            #                         co_grupo %in% tec ~ 'Tecnologias',
            #                         co_grupo %in% med ~ 'Medicina',
            #                         co_grupo %in% saude ~ 'Saúde',
            #                         TRUE ~ 'Licenciaturas')) %>% 
            # ggboxplot(x = "comp", y = "enem", 
            #           color = "comp", 
            #           order = c("Tecnologias", "Licenciaturas", "Saúde", 
            #                     "Direito", "Ciências Humanas", 
            #                     "Engenharias",  "Medicina"),
            #           ylab = "Nota Geral", xlab = "Área")+
            #   labs(title = "Distribuição da Nota Geral - ENEM (2017-2019)", # Informações
            #        caption = 'Fonte: Microdados ENEM (INEP, 2021).'
            #        ) +
            #   theme(text = element_text(size = 10, family = "serif"), # Formatação
            #         plot.title = element_text(hjust =0.5, face = "bold"),
            #         plot.caption = element_text(hjust = -.1, vjust = .2),
            #         axis.title.x = element_text(vjust = 0.2),
            #         legend.position = "none")
            
            # # ANOVA test
            # res.aov <- aov(enem ~ comp, data = ciclo.1)
            # summary(res.aov)
            # summary(res.aov)
            
        

                
    # Distribuição da Nota Geral do ENEM para 2017-2019 (ciclo.2) --------------
      ciclo.2 <- db %>%
          filter(nu_ano %in% (2017:2019)) %>% 
          select(c(nu_ano, co_grupo, co_categad, co_munic_curso,
                   ano_in_grad, ano_enem, enem, vl_nt_enem_cn,
                   vl_nt_enem_ch, vl_nt_enem_lc, vl_nt_enem_mt, lic,
                   area_enade, area_nome, area_lic, analise, graph, media))                    
         
        
        # Comparação Geral 
        # Estatísticas Descritivas
        sumtable(ciclo.2,
                 vars = c('enem', 'vl_nt_enem_cn', 'vl_nt_enem_ch',
                          'vl_nt_enem_lc', 'vl_nt_enem_mt'),
                 out = 'csv',
                 file = 'output/tables/sum_ciclo1',
                 summ = list(c('notNA(x)', 'mean(x)', 'median(x)', 'sd(x)',
                               'min(x)', 'pctile(x)[25]',
                               'pctile(x)[75]', 'max(x)')),
                 summ.names = list(c('N','Média', 'Mediana', 'Desvio Padrão',
                                     'Mínimo', 'Percentil (25%)',
                                     'Percentil (75%)', 'Máximo')),
                 group.long = FALSE,
                 digits = 2, fixed.digits = TRUE,
                 labels = c('Nota Geral', 'Ciências da Natureza',
                            'Ciências Humanas e Sociais',
                            'Linguagens', 'Matemática'),
                 fit.page = '.9\\textwidth',
                 note.align = 'l')
        
        # Histograma com curva de densidade
        
        titulo <- "Distribuição da Nota Geral do ENEM dos Participantes da Prova do ENADE (ciclo 2017-2019)" 
        
        
        ciclo.2 %>% 
          ggplot(aes(x=enem)) + 
          geom_histogram(aes(y=..density..),      # Histograma com densidade no eixo y
                         binwidth=10,
                         colour="black", fill="white") +
          geom_density(alpha=.25, fill="#FF6666") + # Sobreposição com gráfico de densidade transparente
          labs(title = titulo, # Informações
               caption = fonte,
               x ="Média aritmétrica das quatro áreas",
               y = "Densidade") +
          theme_minimal() +
          theme(text = element_text(size = 10, family = "serif"), # Formatação
                plot.title = element_text(hjust =0.5, face = "bold"),
                plot.caption = element_text(hjust = 0.0, vjust = 0.2),
                axis.title.x = element_text(vjust = 0.2)
          )
        

        # Comparação Licenciaturas e Bacharelados 
        # Arquivo com a tabela completa: 'output/tables/licenc_bach_descritiva_2017-2019.xlsx

        #
        # Estatísticas Descritivas
        
        # Média
        ciclo.2 %>% 
          #filter(analise == 1) %>%
          sumtable(ciclo.2,
                   vars = c('enem', 'vl_nt_enem_cn', 'vl_nt_enem_ch',
                            'vl_nt_enem_lc', 'vl_nt_enem_mt'),
                   out = 'csv',
                   file = 'output/tables/sum_ciclo2_media.csv',
                   summ = list(c('mean(x)')),
                   summ.names = list(c('Média')),
                   group = 'area_nome',
                   group.long = FALSE,
                   add.median = FALSE,
                   digits = 2, fixed.digits = TRUE,
                   labels = c('Nota Geral', 'Ciências da Natureza',
                              'Ciências Humanas e Sociais',
                              'Linguagens', 'Matemática'))
        
        # N
        ciclo.2 %>% 
          #filter(analise == 1) %>%
          sumtable(ciclo.2,
                   vars = c('enem'),
                   out = 'csv',
                   file = 'output/tables/sum_ciclo2_n.csv',
                   summ = list(c('notNA(x)')),
                   summ.names = list(c('N')),
                   group = 'area_nome',
                   group.long = FALSE,
                   add.median = FALSE,
                   digits = 2, fixed.digits = TRUE,
                   labels = c('Nota Geral'))
        
        
        # Gráfico de barras
        
        # Cores da legenda para os cinco grupos de médias
        cores <- c('royalblue', 'light blue', 'yellow', 'orange', 'red')
        
        # Título do gráfico e fonte dos dados
        titulo <- 
          "Concluintes Ensino Superior (2017-2019) - Áreas Selecionadas: Percentual 
          formandos com notas no ENEM abaixo de diferentes cortes"
        
        theme <- 
          # Estilo do gráfico
          theme_minimal() +  
          # Formatação dos textos do gráfico
          theme(text = element_text(size = 10),                      
                # Título
                plot.title = element_text(hjust =.5, face = "bold"),  
                # Fonte - localização na horizontal
                plot.caption = element_text(hjust = 0.0),   
                # Posição do texto da fonte
                plot.caption.position = 'plot',#                     
                # Eixo 'x'
                axis.text.x = element_text(angle = 45, hjust = 1),
                # Para retirar o título do eixo 'x'
                axis.title.x = element_blank(),
                # Para retirar o título do eixo 'y'
                axis.title.y = element_blank(),
                # Para retirar o título da legenda
                legend.title = element_blank(),
                # Definir a posição da legenda no gráfico
                legend.position = 'bottom',
                # Centralização subtítulo
                plot.subtitle = element_text(hjust=0.5)) 
        
        
          
          # Média Geral 
          ciclo.2 %>% 
            # Filtrando pelas áreas listadas na proposta 
            filter(graph == 1) %>%
            # Agrupando por área e pelos grupos de médias de notas
            group_by(area_lic, media) %>%    
            # Calculando a porcentagem de cada média nos cursos numa nova variável
            dplyr::summarise(n = n()) %>%                           
            mutate(freq = formattable::percent(n/sum(n))) %>% 
            # Variáveis do gráfico com 'forcats' para reverter as ordem dos valores
            ggplot(aes(y = freq, x = fct_reorder2(area_lic, media, freq),
                       fill = forcats::fct_rev(media))) + 
            # Tipo de gráfico
            geom_bar(position = 'fill', stat = "identity") + 
            # Texto para as porcentagens calculadas
            geom_text(aes(label = paste0(sprintf("%1.1f", freq*100), "%")),
                      position =  position_fill(vjust = 0.5),
                      size = 3) +      
            # Escala de porcentagem no eixo 'y'
            scale_y_continuous(label = scales::percent) +  
            # Definição manual das cores dos grupos de médias definidos
            scale_fill_manual(values = cores) + 
            labs(title = titulo,
                 caption = fonte) +
            theme
          
          

          titulo <- "Concluintes Ensino Superior (2017-2019) - Áreas Selecionadas: Percentual formandos com notas  
                    no ENEM abaixo de diferentes cortes por Dependência Administrativa da IES"
          
          # Rede Pública
          ciclo.2 %>% 
            # Filtrando pelas áreas listadas na proposta 
            filter(graph == 1,
                   co_categad %in% 1:3) %>% 
            # Agrupando por área e pelos grupos de médias de notas
            group_by(area_lic, media) %>%    
            # Calculando a porcentagem de cada média nos cursos numa nova variável
            dplyr::summarise(n = n()) %>%                           
            mutate(freq = formattable::percent(n/sum(n))) %>% 
            # Variáveis do gráfico com 'forcats' para reverter as ordem dos valores
            ggplot(aes(y = freq, x = reorder(area_lic, media), 
                       fill = forcats::fct_rev(media))) + 
            # Tipo de gráfico
            geom_bar(position = 'fill', stat = "identity") + 
            # Texto para as porcentagens calculadas
            geom_text(aes(label = paste0(sprintf("%1.1f", freq*100), "%")),
                      position =  position_fill(vjust = 0.5),
                      size = 3) +      
            # Escala de porcentagem no eixo 'y'
            scale_y_continuous(label = scales::percent) +  
            # Definição manual das cores dos grupos de médias definidos
            scale_fill_manual(values = cores) + 
            labs(title = titulo,
                 subtitle = 'Rede Pública',
                 caption = fonte) +
            theme
          
          # Rede Privada
          ciclo.2 %>% 
            # Filtrando pelas áreas listadas na proposta 
            filter(graph == 1,
                   co_categad %in% 4:5) %>% 
            # Agrupando por área e pelos grupos de médias de notas
            group_by(area_lic, media) %>%    
            # Calculando a porcentagem de cada média nos cursos numa nova variável
            dplyr::summarise(n = n()) %>%                           
            mutate(freq = formattable::percent(n/sum(n))) %>% 
            # Variáveis do gráfico com 'forcats' para reverter as ordem dos valores
            ggplot(aes(y = freq, x = fct_reorder2(area_lic, media, freq),
                       fill = forcats::fct_rev(media))) + 
            # Tipo de gráfico
            geom_bar(position = 'fill', stat = "identity") + 
            # Texto para as porcentagens calculadas
            geom_text(aes(label = paste0(sprintf("%1.1f", freq*100), "%")),
                      position =  position_fill(vjust = 0.5),
                      size = 3) +      
            # Escala de porcentagem no eixo 'y'
            scale_y_continuous(label = scales::percent) +  
            # Definição manual das cores dos grupos de médias definidos
            scale_fill_manual(values = cores) + 
            labs(title = titulo,
                 subtitle = 'Rede Privada',
                 caption = fonte) +
            theme
          
          # Título do gráfico
          titulo <- 
            "Percentual de Concluintes no ensino superior (2017-2019) com nota acima de 600 no Enem"
          
          # Nota maior que 600 no ENEM
          # Estilo do gráfico
          theme <- 
            # Estilo do gráfico
            theme_minimal() +
            # Formatação dos textos do gráfico
            # Todo gráfico
            theme(text = element_text(size = 10), # Formatação
                  # Título centralizado
                  plot.title = element_text(face = "bold", hjust = .5),
                  # Fonte à esquerda da parte inferior do gráfico
                  plot.caption = element_text(hjust = 0.0),
                  plot.caption.position = 'plot',
                  # Eixo 'x' rótulos 
                  axis.text.x = element_text(),
                  # Para retirar o título do eixo 'x'
                  axis.title.x = element_blank(),
                  # Para retirar o título do eixo 'y'
                  axis.title.y = element_blank(),
                  # Para retirar o título da legenda
                  legend.title = element_blank(),
                  # Definir a posição da legenda no gráfico
                  legend.position = 'bottom',
                  panel.grid.minor = element_blank(),
                  panel.grid.major = element_line()) 
          
          
          d <- ciclo.2 %>% 
            # Agrupando por área e pelos grupos de médias de notas
            group_by(area_nome) %>% 
            # Calculando o número total de alunos por área
            dplyr::summarise(total = n())
          
          d2 <- ciclo.2 %>% 
            # Filtrando se obtive nota maior que 600 no ENEM
            filter(enem >= 600) %>% 
            # Calculando a porcentagem de alunos concluintes com nota maior que
            # 600 no ENEM
            group_by(area_nome, lic) %>%
            dplyr::summarise(mais.600 = n()) %>% 
            right_join(d) %>% 
            mutate(freq = formattable::percent(mais.600/total))
          
          # Variáveis do gráfico com áreas em ordem alfabética
          d2 %>% 
            #filter(row_number(desc(freq)) <= 30) %>% 
            ggplot(aes(y = (freq), x = reorder(area_nome, freq))) +
            # Tipo de gráfico
            geom_bar(stat = "identity", aes(fill = lic)) +
            # Escola de porcentagem no eixo 'y'
            scale_y_continuous(label = scales::percent, limits = c(0, 1),
                               breaks = seq(0, 1, by = .2)) +
            scale_fill_manual(values = c('grey34', 'royalblue')) +
            # Texto para as porcentagens calculadas
            geom_text(aes(label = freq), hjust = -0.1, size = 2.5) +
            # Título do gráfico e fonte dos dados
            labs(title = titulo,
                 caption = fonte) +
            # Estilo
            theme +
            # Mudar a direção das barras
            coord_flip()
    
          
          
      # Distribuição da Nota Geral do ENEM para 2014-2016 e 2017-2019-------
          comp.ciclos <- db %>%
            select(c(nu_ano, co_grupo, co_categad, co_munic_curso,
                     ano_in_grad, ano_enem, enem, vl_nt_enem_cn,
                     vl_nt_enem_ch, vl_nt_enem_lc, vl_nt_enem_mt, lic,
                     area_enade, area_nome, area_lic, analise, graph, media)) %>% 
            mutate(ciclo = case_when(nu_ano %in% 2014:2016 ~ 'Ciclo (2014-2016)',
                                     TRUE ~ 'Ciclo (2017-2019)'))
          
          
          # Título do gráfico
          titulo <- 
            "Percentual de Participantes da Prova do ENADE (2014-2019) com nota acima de 600 no Enem"
          
          # Estilo do gráfico
          theme <- 
            # Estilo do gráfico
            theme_minimal() +
            # Formatação dos textos do gráfico
            # Todo gráfico
            theme(text = element_text(size = 10), # Formatação
                  # Título centralizado
                  plot.title = element_text(face = "bold", hjust =.5),
                  # Fonte à esquerda da parte inferior do gráfico
                  plot.caption = element_text(hjust = 0.0),
                  plot.caption.position = 'plot',
                  # Eixo 'x' rótulos 
                  axis.text.x = element_text(),
                  # Para retirar o título do eixo 'x'
                  axis.title.x = element_blank(),
                  # Para retirar o título do eixo 'y'
                  axis.title.y = element_blank(),
                  # Para retirar o título da legenda
                  legend.title = element_blank(),
                  # Definir a posição da legenda no gráfico
                  legend.position = 'bottom',
                  panel.grid.minor = element_blank(),
                  panel.grid.major = element_line()) 
          
          
          # Nota maior que 600 no ENEM
          d <- comp.ciclos %>% 
            # Agrupando por área e pelos grupos de médias de notas
            group_by(area_nome, ciclo) %>% 
            # Calculando o número total de alunos por área
            dplyr::summarise(total = n())
             # Calculando a porcentagem de alunos concluintes com nota maior que
          
          d3 <- comp.ciclos %>% 
            # 600 no ENEM
            # Filtrando se obtive nota maior que 600 no ENEM
            filter(enem >= 600) %>% 
            group_by(area_nome, ciclo) %>%
            dplyr::summarise(mais.600 = n()) %>% 
            right_join(d) %>% 
            mutate(freq = formattable::percent(mais.600/total))
          
          # Variáveis do gráfico com áreas em ordem alfabética
          d3 %>% 
            ggplot(aes(y = (freq), x = reorder(area_nome, freq),
                       fill = (as.factor(ciclo)))) +
            # Tipo de gráfico
            geom_bar(stat = "identity", position = position_dodge()) +
            # Escola de porcentagem no eixo 'y'
            scale_y_continuous(label = scales::percent, limits = c(0, 1),
                               breaks = seq(0, 1, by = .2)) +
            # Texto para as porcentagens calculadas
            #geom_text(aes(label = freq), hjust = -0.1, size = 2.5) +
            # Título do gráfico e fonte dos dados
            labs(title = titulo,
                 caption = fonte) +
            # Estilo
            theme +
            # Mudar a direção das barras
            coord_flip()
          