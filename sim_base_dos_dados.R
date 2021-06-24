# Artigo Rafael
# Abrindo SIm via BDD - Autor: Marcello Filgueiras

library(tidyverse)
library(basedosdados)

# Defina o seu projeto no Google Cloud
set_billing_id("marcellosf")
# Para carregar o dado direto no R
query_dicionario <- "SELECT * FROM `basedosdados.br_ms_sim.dicionario`"
dicionario <- read_sql(query_dicionario)



query_microdados <- " SELECT ano, data_obito, causa_basica, idade, linha_a, linha_b, linha_c, linha_d, linha_ii
FROM `basedosdados.br_ms_sim.microdados` 
WHERE ano IN (2018,2019,2020,2021)"

query_microdados_2020 <- " SELECT ano, data_obito, causa_basica, idade, linha_a, linha_b, linha_c, linha_d, linha_ii
FROM `basedosdados.br_ms_sim.microdados` 
WHERE ano = 2020
LIMIT 100"

sim_2020<- read_sql(query_microdados_2020)
                  
sim_microdados <- read_sql(query_microdados)

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


#Contando Ano

sim_microdados %>%
  filter(str_detect(causa_basica, regex_cids))%>%
  count(ano)

#Contando Causa Base

sim_microdados %>%
  filter(str_detect(causa_basica, regex_cids))%>%
  group_by(causa_basica)%>%
  count(ano) %>%
  ungroup()%>%
  mutate(ano=as.numeric(ano))%>%
  ggplot(aes(y= causa_basica, x= n, fill=as.factor(ano))) + geom_col(position="fill")


#Cid K80 é a de doenças das vias biliares

hernias_com_grangena <- str_c("K461",
                              "K411",
                              "K414",
                              "K401",
                              "K404",
                              "K421",
                              "K431",
                              "K451",
                              sep = "|")

hernias_com_obstrução <- str("k46.0",
                             "k41",
                             "k41.4",
                             "k40.1",
                             "k40.4",
                             "k42.1",
                             "k43.1",
                             "k45.1",
                             sep = "")


criadora_de_cids<- function (x) {
  
  paste(x, c("1","2","3", "4", "5", "6","7","8","9"),
        sep = "", collapse = "|" )
}

doenças = c("K80", "K81", "K82", "K83")

cid_rafael <- purrr::map_chr(doenças, criadora_de_cids)%>%
  str_c(sep = "|", collapse = "|")%>%
  regex()







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

doenças_rafael2 <- sistema_mortalidade_ods_2020_raw %>%
  dplyr::filter(str_detect(LINHAA, x)|
                  str_detect(LINHAB, x)|
                  str_detect(LINHAC, x)|
                  str_detect(LINHAD, x)|
                  str_detect(LINHAII, x)|
                  str_detect(CAUSABAS, x)|
                  str_detect(CAUSAMAT, x)|
                  str_detect(CAUSABAS_O, x))


doenças_rafael <- sistema_mortalidade_ods_2020_raw %>%
  filter( str_detect(LINHAA, cid_rafael)|
            str_detect(LINHAB, cid_rafael)|
            str_detect(LINHAC, cid_rafael)|
            str_detect(LINHAD, cid_rafael)|
            str_detect(LINHAII, cid_rafael)|
            str_detect(CAUSABAS, cid_rafael)|
            str_detect(CAUSAMAT, cid_rafael)|
            str_detect(CAUSABAS_O, cid_rafael))



trens_gastricos<- sistema_mortalidade_ods_2020_raw%>%
  filter(str_detect(LINHAA, cids_trens_gastricos)|
           str_detect(LINHAB, cids_trens_gastricos)|
           str_detect(LINHAC, cids_trens_gastricos)|
           str_detect(LINHAD, cids_trens_gastricos)|
           str_detect(LINHAII, cids_trens_gastricos)|
           str_detect(CAUSABAS, cids_trens_gastricos)|
           str_detect(CAUSAMAT, cids_trens_gastricos)|
           str_detect(CAUSABAS_O, cids_trens_gastricos)
  )





hernias_com_grangena_df<- sistema_mortalidade_ods_2020_raw%>%
  filter(str_detect(LINHAA, hernias_com_grangena)|
           str_detect(LINHAB, hernias_com_grangena)|
           str_detect(LINHAC, hernias_com_grangena)|
           str_detect(LINHAD, hernias_com_grangena)|
           str_detect(LINHAII, hernias_com_grangena)|
           str_detect(CAUSABAS, hernias_com_grangena)|
           str_detect(CAUSAMAT, hernias_com_grangena)|
           str_detect(CAUSABAS_O, hernias_com_grangena)
  )



#Modeling ---------------------------------------------------


#Visualização -----------------------------------------------


#Exportação -------------------------------------------------



