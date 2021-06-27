# Análise mortalidade por uns trem gástrico - Artigo Rafael
# Autor: Marcello Filgueiras 
#Abrindo base de 2020 pelo OPEN DATA SUS
# META É anos = (2017201820192020)


library(tidyverse)

#Importação -------------------------------------------------

#https://bvsms.saude.gov.br/bvs/publicacoes/sis_mortalidade.pdf


library(readr)
sim_ods_2020_raw <- read_csv2("data_raw/dobrano_.csv")



# Tidying -----------------------------------------------------------------

#de acordo com o padrão da bdd+



sim_ods_2020_tidy<- sim_ods_2020_raw %>%
  janitor::clean_names()%>%
  dplyr::rename("tipo_obito" = "tipobito",
                "data_obito" = "dtobito",
                "hora_obito" = "horaobito",
                "data_nascimento"= "dtnasc",
                "raca_cor"= "racacor",
                "estado_civil" = "estciv",
                "escolaridade" = "esc",
                "escolaridade_2010" = "esc2010",
                "ocupacao"= "ocup",
                "local_ocorrencia" = "lococor",
                "linha_a" = "linhaa",
                "linha_b"= "linhab",
                "linha_c" = "linhac",
                "linha_d" = "linhad",
                "linha_ii" = "linhaii",
                "causa_basica" = "causabas",
                "data_atestado" = "dtatestado",
                "data_cadastro" = "dtcadastro",
                "data_recebimento" = "dtrecebim",
                "data_recoriga" = "dtrecoriga",
                "data_conclusao_investigacao" = "dtconinv",
                "data_cadastro_informacao" = "dtcadinf",
                "data_conclusao_caso"= "dtconcaso") %>%
  mutate(
        data_obito = lubridate::dmy(data_obito),
        ano = lubridate::year(data_obito),
        data_atestado = lubridate::dmy(data_atestado),
        #data_cadastro = lubridate::dmy(data_cadastro),
        #data_recebimento = lubridate::dmy(data_recebimento),
        data_recoriga = lubridate::dmy(data_recoriga),
        #data_conclusao_investigacao = lubridate::dmy(data_conclusao_investigacao),
        #data_cadastro_informacao = lubridate::dmy(data_cadastro_informacao),
        #data_conclusao_caso = lubridate::dmy(data_conclusao_caso),
        #hora_obito =  lubridate::hm(hora_obito), error: "Some strings failed to parse, or all strings are NAs "
        tipo_obito = case_when(
            tipo_obito == "1"~ "Fetal",
            tipo_obito == "2"~ "Não Fetal",
            TRUE ~ as.character(tipo_obito)),
        raca_cor = case_when(
            raca_cor == "1"~ "Branca",
            raca_cor == "2" ~ "Preta",
            raca_cor == "3" ~ "Amarela",
            raca_cor == "4" ~ "Parda",
            raca_cor == "5" ~ "Indígena",
            TRUE ~ as.character(raca_cor)),
        estado_civil = case_when(
            estado_civil== "1" ~ "Solteiro(a)",
            estado_civil == "2" ~ "Casado(a)",
            estado_civil == "3" ~ "Viúvo(a)",
            estado_civil == "4" ~ "Separado(a)",
            estado_civil == "5" ~ "União consensual",
            #estado_civil == "9" ~ NA, "cant put a logical into a character vector"
            TRUE ~ as.character(estado_civil)),
        cirurgia = case_when(
            cirurgia == "1" ~ "Sim",
            cirurgia == "2" ~ "Não",
            TRUE ~ as.character(cirurgia)),
        local_ocorrencia = case_when(
            local_ocorrencia == "1" ~ "Hospital",
            local_ocorrencia == "2" ~ "Outro estabelecimento de saúde",
            local_ocorrencia == "3" ~ "Domicílio",
            local_ocorrencia == "4" ~ "Via pública",
            local_ocorrencia == "5" ~ "Outros",
            TRUE ~ as.character(local_ocorrencia)))



#deu falha ao parsear, não entendo pq
# pós: lubridate não funciona com parte da coluna em NA

# sim_ods_2020_tidy %>% pull(data_obito) %>% is.na() %>% table()
#sim_ods_2020_tidy %>% pull(data_atestado) %>% is.na() %>% table()


#sim_ods_2020_tidy %>%
  #select( data_atestado, data_cadastro,data_recebimento,
 #           data_recoriga, data_conclusao_investigacao, data_cadastro_informacao,
  #          data_conclusao_caso)%>%
#  mutate(across(
 #   .cols = c(data_atestado, data_cadastro,data_recebimento,
  #            data_recoriga, data_conclusao_investigacao,
   #           data_cadastro_informacao,data_conclusao_caso),
    #  lubridate::mdy(.x)
#    )) #%>% view()

        
        "microdados
        tipo_nivel_investigador
        E
        1996(1)2019
        Estadual
        551
        microdados
        tipo_nivel_investigador
        R
        1996(1)2019
        Regional
        552
        microdados
        tipo_nivel_investigador
        M
        1996(1)2019
        Municipal
        553
        
  )"
#verificando caraterísticas da base

#só temos dados até setembro? sim na antiga. Base mais atualizada não
sim_ods_2020_tidy %>%
  select (ano, data_obito, causa_basica, atestado, idade) %>%
  mutate(mes = paste(str_sub(data_obito, 1,7),01, sep="-")%>%
           lubridate::ymd()) %>%
  count(mes)





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
  
  
hernias_regex<- str_c("K461",
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

#Filtrando


  sim_ods_2020_filtro_causa_basica <- sim_ods_2020_tidy %>%
  select (ano, data_obito, causa_basica, atestado) %>%
    filter(causa_basica %in% cids_vetor|
           causa_basica %in% hernias_vetor) %>%
    mutate(mes = paste(str_sub(data_obito, 1,7),01, sep="-")%>%
             lubridate::ymd())
  
  sim_ods_2020_tidy %>%
    pull(idade) %>%
    str_length() %>%
    table()
    
  


#Se estiver com saco cheio de digitar vários subgrupos de CIDs, use a função abaixo

criadora_de_cids<- function (x) {
  
   paste(x, c("1","2","3", "4", "5", "6","7","8","9"),
         sep = "", collapse = "|" )
}

doenças_vias_biliares = c("K80", "K81", "K82", "K83") 

cid_rafael <- purrr::map_chr(doenças, criadora_de_cids)%>%
  str_c(sep = "|", collapse = "|")



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


#Modeling ---------------------------------------------------


#Visualização -----------------------------------------------

#"Número de Mortes desses trem gástrico por mês em 2020"

sim_ods_2020_filtro_causa_basica %>%
  filter(causa_basica %in% cids_vetor) %>% 
  mutate(mes = paste(str_sub(data_obito, 1,7),01, sep="-")%>%
           lubridate::ymd())%>%
  group_by(mes) %>%
  count(causa_basica)%>%
  tsibble::as_tsibble(index=mes, key = causa_basica) %>%
  ggplot(aes(x=mes, y=n, fill= causa_basica)) + geom_col(color= "black") +
  labs(title= "Número de Mortes desses trem gástrico por mês em 2020",
       caption = "OpenDataSus - Sistema Informatizado de Mortalidade - Dados Preliminares de 2020") +
  scale_x_date( breaks= "1 month") +
  theme_minimal() + theme(axis.text.x = element_text(face = "bold", 
                                                     size = 10, angle = 90),
                          plot.title = element_text(size = 16, face= "bold"))

#"Número de Mortes por Hérnia por mês em 2020"

sim_ods_2020_filtro_causa_basica %>%
  filter(causa_basica %in% hernias_vetor) %>% 
  mutate(mes = paste(str_sub(data_obito, 1,7),01, sep="-")%>%
           lubridate::ymd())%>%
  group_by(mes) %>%
  count(causa_basica)%>%
  tsibble::as_tsibble(index=mes, key = causa_basica) %>%
  ggplot(aes(x=mes, y=n, fill= causa_basica)) + geom_col(color= "black") +
  labs(title= "Número de Mortes por Hérnia por mês em 2020",
       caption = "OpenDataSus - Sistema Informatizado de Mortalidade - Dados Preliminares de 2020") +
  scale_x_date( breaks= "1 month") +
  theme_minimal() +
  theme(axis.text.x = element_text(face = "bold", size = 10, angle = 90),
        plot.title = element_text(size = 16, face= "bold"))


#Exportação -------------------------------------------------

