# Artigo Rafael
# Abrindo SIm via BDD - Autor: Marcello Filgueiras

library(tidyverse)
library(basedosdados)



# Importing ---------------------------------------------------------------

 
# Defina o seu projeto no Google Cloud
set_billing_id("marcellosf")
# Para carregar o dado direto no R
query_dicionario <- "SELECT * FROM `basedosdados.br_ms_sim.dicionario`"
dicionario <- read_sql(query_dicionario)


# Baixando somente colunas de interesse para pesquisa do Rafael
query_microdados_rafael <- " SELECT ano, data_obito, causa_basica, idade, atestado
FROM `basedosdados.br_ms_sim.microdados` 
WHERE ano IN (2018,2019)"

sim_microdados_rafael <- read_sql(query_microdados_rafael)

#Verificando se tem 2020:  Não tem

query_microdados_2020 <- " SELECT ano, data_obito, causa_basica, idade, atestado
FROM `basedosdados.br_ms_sim.microdados` 
WHERE ano = 2020
LIMIT 100"

sim_2020<- read_sql(query_microdados_2020)



#Filtros e Classificações -----------------------------------

#buscando algumas doenças específicas

    regex_cids<- paste("K35",
                       "K36",
                       "K37",
                       "K38",
                       "K8",
                       "Q206",
                       "D121",
                       "D373",
                       "C181",
                       sep= "|")


hernias_regex<- paste("K461",
                      "K411",
                      "K414",
                      "K401",
                      "K404",
                      "K421",
                      "K431",
                      "K451",
                      sep = "|")
        
        cids_vetor<- c("K35", #trenss gastricos em geral
                       "K36",
                       "K37",
                       "K38",
                       "K8", # K80 é a de doenças das vias biliares
                       "Q206",
                       "D121",
                       "D373",
                       "C181")
    
    hernias_vetor<- c( "K461",
                       "K411",
                       "K414",
                       "K401",
                       "K404",
                       "K421",
                       "K431",
                       "K451")
    
    
    
#Se estiver com saco cheio de digitar vários subgrupos de CIDs, use a função abaixo
    
criadora_de_cids<- function (x) {
  
  paste(x, c("1","2","3", "4", "5", "6","7","8","9"),
        sep = "", collapse = "|" )
}

#Cid K80 é a de doenças das vias biliares

doenças = c("K80", "K81", "K82", "K83")

cid_rafael <- purrr::map_chr(doenças, criadora_de_cids)%>%
  str_c(sep = "|", collapse = "|")%>%
  regex()



# Filtrando


sim_bdd_2018_filtro_causa_basica <- sim_microdados_rafael %>%
  select (ano, data_obito, causa_basica, atestado) %>%
  filter(causa_basica %in% cids_vetor|
           causa_basica %in% hernias_vetor) %>%
  mutate(mes = paste(str_sub(data_obito, 1,7),01, sep="-")%>%
           lubridate::ymd(),
         ano = as.double(ano))

# Função não é necessária porque existe uma coluna chamada Atestado com todas as linhas =D
# Poder da Base Tidy!

filtradora_doenças <- function (x) {
  dplyr::filter(str_detect(LINHAA, x)|
                  str_detect(LINHAB, x)|
                  str_detect(LINHAC, x)|
                  str_detect(LINHAD, x)|
                  str_detect(LINHAII, x)|
                  str_detect(CAUSABAS, x)|
                  str_detect(CAUSAMAT, x)|
                  str_detect(CAUSABAS_O, x))
}


# Verificando Características da base 



# VerificContando Ano

sim_bdd_2020_filtro_causa_basica %>%
  count(ano)

#Contando Causa Base

sim_bdd_2020_filtro_causa_basica %>%
  group_by(causa_basica)%>%
  count(ano) %>%
  ungroup()%>%
  mutate(ano=as.numeric(ano))%>%
  ggplot(aes(y= causa_basica, x= n, fill=as.factor(ano))) + geom_col(position="fill")





#Modeling ---------------------------------------------------



#Visualização -----------------------------------------------

#"Número de Mortes desses trem gástrico por mês em 2020"

sim_bdd_2020_filtro_causa_basica %>%
  filter(causa_basica %in% cids_vetor) %>%
  group_by(mes) %>%
  count(causa_basica)%>%
  tsibble::as_tsibble(index=mes, key = causa_basica) %>%
  ggplot(aes(x=mes, y=n, fill= causa_basica)) + geom_col(color= "black") +
  labs(title= "Número de Mortes desses trem gástrico por em 2018 a 2019",
       caption = "OpenDataSus - Sistema Informatizado de Mortalidade - Dados Preliminares de 2020") +
  scale_x_date( breaks= "1 month") +
  theme_minimal() + theme(axis.text.x = element_text(face = "bold", 
                                                     size = 10, angle = 90),
                          plot.title = element_text(size = 16, face= "bold"))

#"Número de Mortes por Hérnia por mês em 2020"

sim_bdd_2020_filtro_causa_basica %>%
  filter(causa_basica %in% hernias_vetor) %>%
  group_by(mes) %>%
  count(causa_basica)%>%
  tsibble::as_tsibble(index=mes, key = causa_basica) %>%
  ggplot(aes(x=mes, y=n, fill= causa_basica)) + geom_col(color= "black") +
  labs(title= "Número de Mortes por Hérnia por mês em 2018 a 2019",
       caption = "OpenDataSus - Sistema Informatizado de Mortalidade - Dados Preliminares de 2020") +
  scale_x_date( breaks= "1 month") +
  theme_minimal() +
  theme(axis.text.x = element_text(face = "bold", size = 10, angle = 90),
        plot.title = element_text(size = 16, face= "bold"))


#Exportação -------------------------------------------------



