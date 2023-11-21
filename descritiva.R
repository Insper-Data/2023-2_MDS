library(PNADcIBGE)
library(srvyr)
library(tidyverse)
library(RColorBrewer)
library(sf)
library(geobr)
library(stringi)
library(openxlsx)

df <- PNADcIBGE::get_pnadc(2022, topic = 4, labels = TRUE, design = TRUE)

#Por onde pessoas não ocupadas buscam emprego, separado BF ----
busca_emprego <- df %>% 
  srvyr::as_survey(strata = stype) %>% 
  srvyr::group_by(UF, V4072A, VI5002A) %>% 
  srvyr::survey_tally() %>% 
  drop_na()

busca_emprego.tabela <- busca_emprego %>%
  group_by(UF, VI5002A) %>% 
  summarize(sum_n = sum(n)) %>% 
  right_join(busca_emprego) %>% 
  mutate(V4072A = fct_collapse(
    V4072A,
    "Fez ou inscreveu-se em concurso" = c("Fez ou inscreveu-se em concurso"),
    "Consultou parente, amigo ou colega" = c("Consultou parente, amigo ou colega"),
    "Tomou medida para iniciar o próprio negócio" = c("Tomou medida para iniciar o próprio negócio (recursos financeiros, local para instalação, equipamentos, legalização etc.)"),
    "Consultou ou inscreveu-se em agência de emprego" = c("Consultou ou inscreveu-se em agência de emprego municipal, estadual ou no Sistema Nacional de Emprego (SINE)",
                                                          "Consultou ou inscreveu-se em agência de emprego privada ou sindicato"),
    "Entrou em contato com empregador" = c("Entrou em contato com empregador (pessoalmente, por telefone, por email ou pelo portal da empresa, inclusive enviando currículo)"),
    "Não tomou providência / Tomou outra providência" = c("Tomou outra providência, especifique:",
                                                          "Não tomou providência efetiva"))) %>% 
  group_by(UF, VI5002A, V4072A) %>% 
  summarize(sum_n = mean(sum_n),
            n = sum(n)) %>% 
  mutate(prop = n/sum_n) %>% 
  filter(VI5002A != "Ignorado") %>% 
  mutate(VI5002A = ifelse(VI5002A == "Sim", "Recebe Bolsa Família", "Não Recebe Bolsa Família"),
         UF = fct_relevel(UF, sort))

busca_emprego.tabela %>% 
  ggplot(aes(x = prop, y = reorder(UF, desc(UF)), fill = reorder(V4072A, desc(V4072A)))) +
  geom_col(position = "stack") +
  facet_wrap(~ VI5002A) +
  labs(title = "Como as pessoas buscam empregos", 
       x = "Maneiras que as pessoas buscam emprego (%)",
       y = "Unidade Federativa", fill = "Opções de resposta na PNAD") +
  theme(legend.position = "bottom", legend.direction="vertical") +
  scale_x_continuous(labels = scales::percent) +
  scale_fill_viridis_d(direction = -1)

ggsave("imagens/busca_emprego.png", width = 10, height = 8, dpi = 600)

busca_emprego.tabela %>% 
  mutate(prop = prop %>% round(3)) %>% 
  pivot_wider(names_from = VI5002A, values_from = prop, id_cols = c(UF, V4072A)) %>% 
  replace(is.na(.), 0) %>%
  openxlsx::write.xlsx("tabelas/busca_emprego.xlsx")

#Mapa de carteira assinada
estados_geometria <- read_state(year = 2020, showProgress = FALSE) %>% 
  select(UF = name_state, geometry = geom) %>% 
  mutate(UF = stri_trans_general(str = str_to_title(UF), 
                                 id = "Latin-ASCII"))
carteira_assinada <- df %>% 
  srvyr::as_survey(strata = stype) %>% 
  srvyr::group_by(UF, V4029, VI5002A) %>% 
  srvyr::survey_tally() %>% 
  pivot_wider(names_from = V4029, values_from = n, id_cols = c(UF, VI5002A)) %>% 
  filter(VI5002A != "Ignorado") %>% 
  mutate(tx_carteira_assinada = Sim/(Sim + Não),
         UF = stri_trans_general(str = str_to_title(UF), 
                                      id = "Latin-ASCII"),
         VI5002A = ifelse(VI5002A == "Sim", "Recebe Bolsa Família", "Não Recebe Bolsa Família")) %>%
  left_join(estados_geometria)

carteira_assinada %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = tx_carteira_assinada)) +
  geom_sf_label(aes(geometry = geometry, 
                    fill = tx_carteira_assinada,
                    label = tx_carteira_assinada %>% 
                      round(3) %>% 
                      scales::percent()),
                color = "white") +
  scale_fill_viridis_c(begin = 0, end = 1, labels = scales::percent) +
  facet_wrap(~ VI5002A) +
  labs(title = "Percentual de trabalhadores que possuem carteira assinada") +
  theme(legend.position = "none", strip.text.x = element_text(size = 30),
        plot.title = element_text(size=35))

ggsave("imagens/mapa_carteira_assinada.png", height = 12, width = 24, dpi = 600)

carteira_assinada %>% 
  select(-geometry) %>% 
  pivot_wider(names_from = VI5002A, values_from = tx_carteira_assinada, id_cols = UF) %>% 
  openxlsx::write.xlsx("tabelas/carteira_assinada.xlsx")
