# Artigo Rafael
#  Autor: Marcello Filgueiras



# Join na base do ODS (2020) e da BDD (2019 pra trás ----------------------

sim_join_2018_2020 <- full_join(sim_bdd_2018_filtro_causa_basica,
                                sim_ods_2020_filtro_causa_basica)



# Visualizando ------------------------------------------------------------


sim_join_2018_2020 %>%
  filter(causa_basica %in% cids_vetor) %>% 
  mutate(mes = paste(str_sub(data_obito, 1,7),01, sep="-")%>%
           lubridate::ymd())%>%
  group_by(mes) %>%
  count(causa_basica)%>%
  tsibble::as_tsibble(index=mes, key = causa_basica) %>%
  ggplot(aes(x=mes, y=n, fill= causa_basica)) + geom_col(color= "black") +
  labs(title= "Número de Mortes desses trem gástrico por mês",
       subtitle = "Dados de 2018 - 2020",
       caption = "Fonte: Sistema Informatizado de Mortalidade (SIM) - Open Data Sus e Base dos Dados") +
  scale_x_date( breaks= "1 month") +
  theme_minimal() + theme(axis.text.x = element_text(face = "bold", 
                                                     size = 10, angle = 90),
                          plot.title = element_text(size = 16, face= "bold"))

#"Número de Mortes por Hérnia por mês em 2020"

sim_join_2018_2020 %>%
  filter(causa_basica %in% hernias_vetor) %>% 
  mutate(mes = paste(str_sub(data_obito, 1,7),01, sep="-")%>%
           lubridate::ymd())%>%
  group_by(mes) %>%
  count(causa_basica)%>%
  tsibble::as_tsibble(index=mes, key = causa_basica) %>%
  ggplot(aes(x=mes, y=n, fill= causa_basica)) + geom_col(color= "black") +
  labs(title= "Número de Mortes por Hérnia por mês",
       subtitle = "Dados de 2018 - 2020",
       caption = "Fonte: Sistema Informatizado de Mortalidade (SIM) - Open Data Sus e Base dos Dados") +
  scale_x_date( breaks= "1 month") +
  theme_minimal() +
  theme(axis.text.x = element_text(face = "bold", size = 10, angle = 90),
        plot.title = element_text(size = 16, face= "bold"))


