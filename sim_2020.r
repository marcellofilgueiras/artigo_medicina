# Análise mortalidade por uns trem gástrico - Artigo Rafael
# Autor: Marcello Filgueiras 
# META É anos = (2017201820192020)


library(tidyverse)

#Importação -------------------------------------------------

#https://bvsms.saude.gov.br/bvs/publicacoes/sis_mortalidade.pdf


library(readr)
sistema_mortalidade_ods_2020_raw <- read_csv2("data_raw/dobr2020_opendata.csv")

sistema_mortalidade_ods_2020_tidy<- sistema_mortalidade_ods_2020_raw %>%
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
                "linha_a" = "linhaa",
                "linha_b"= "linhab",
                "linha_c" = "linhac",
                "linha_d" = "linhad",
                "linha_ii" = "linhaii",
                "causa_basica" = "causabas",
                "data_atestado"= "dtatestado") %>%
  mutate(tipo_obito=case_when(
            tipo_obito == "1"~ "Fetal",
            tipo_obito == "2"~ "Não Fetal",
            TRUE ~ as.character(tipo_obito)),
        raca_cor = case_when(
            raca_cor == "1"~ "Branca",
            raca_cor == "2" ~ "Preta",
            raca_cor == "3" ~ "Amarela",
            raca_cor == "4" ~ "Parda",
            raca_cor == "5" ~ "Indígena",
            TRUE ~ as.character(raca_cor))
  )


  cle
  count(CAUSABAS) %>%
  arrange(desc(n)) %>%
  view()

#Tidying ----------------------------------------------------


#Filtros e Classificações -----------------------------------

cids_trens_gastricos <- str_c("K351", "K350", "K359", "K37", "K381", "K382",
                          "K389", "K383", "K380", "Q206", "D121", "D373",
                          "C181", "K388", "K36", sep = "|")%>%
  regex()

cids_trens_gastricos2 <- str_c("K35\\.1", "K35\\.0", "K35\\.9", "K37", "K38\\.1", "K38\\.2",
                              "K38\\.9", "K38\\.3", "K38\\.0", "Q20\\.6", "D12\\.1", "D37\\.3",
                              "C18\\.1", "K38\\.8", "K36", sep = "|")%>%
  regex()

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