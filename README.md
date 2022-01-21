# atratividade_docente
Download and Analysis of Brazilian Graduate Students and their High School Performance

ATENÇÃO!!!

TODOS OS CAMINHOS UTILIZADOS NESSE PROJETO SÃO BASEADOS NO 'R PROJECT'.
ENTÃO, PARA TER ACESSO AOS DADOS E ANÁLISES SEM A NECESSIDADE
DE INDICAR O CAMINHO NO SEU COMPUTADOR, SIGA OS SEGUINTES PASSOS:

. Não abra nenhum arquivo da pasta antes de fazer o passo a seguir;
. Acesse o arquivo 'G:/Shared drives/prof_docente_oferta_professores_enem/data_and_analysis/atratatividade_docente.Rproj'
ou abra o R e vá em 'Abrir Projeto' e siga na pasta do projeto até abrir o arquivo 'atratatividade_docente.Rproj'
. Abra os arquivos de código ou os arquivos que desejar


CONTEÚDO
. Download de dados públicos
. Limpeza de dados 
. Análise 


. Download de dados públicos
Acesso aos dados do ENEM, ENADE, Censo Escolar do Ensino Superior, SAEB
Código:'/code/dados_download.R'
Arquivos com as bases originais:
- ENEM	(anos 2004-2006; 2015-2018) : '/raw/enem'
- IDD	(anos 2014-2019) : '/raw/idd'
- Enade	(anos 2014-2019) : '/raw/enade'
- CENSO	(anos - ) : '/raw/censo_superior.csv'
- SAEB	(anos - ) : '/raw/saeb.csv'


. Limpeza de dados 
Acesso às bases limpas do ENEM, ENADE, Censo Escolar do Ensino Superior, SAEB
Código:'/code/dados_limpeza.R'

Arquivos com bases limpas:
- IDD	(anos 2014-2019) : '/clean/idd.csv'
- Enade	(anos 2014-2019) : '/clean/enade.csv'
- ENEM 	(anos 2014-2019) : '/clean/enem.csv'

Arquivos com os metadados:
- ENEM(anos 2014-2019) : '/metadata/enem_dic.xlxs'


. Análise preliminar
Acesso às análises preliminares do ENEM e do ENADE
Código	:'code/dados_analise.R'
Análise	: 'Analise20220119.docx'
Plots	: 'output/plots'
Tabelas : 'output/tables
