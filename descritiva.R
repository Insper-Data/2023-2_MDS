library(PNADcIBGE)
library(srvyr)
library(tidyverse)
library(geobr)
library(sf)
library(stringi)

df <- PNADcIBGE::get_pnadc(2022, topic = 4, labels = TRUE, design = TRUE)

#Por onde pessoas não ocupadas buscam emprego, separado BF ----
busca_emprego <- df %>% 
  srvyr::as_survey(strata = stype) %>% 
  srvyr::group_by(UF, V4072A, VI5002A) %>% 
  srvyr::survey_tally() %>% 
  drop_na()

busca_emprego %>% 
  group_by(UF, VI5002A) %>% 
  summarize(sum_n = sum(n)) %>% 
  right_join(busca_emprego) %>% 
  mutate(prop = n/sum_n) %>% 
  filter(VI5002A != "Ignorado") %>% 
  mutate(VI5002A = ifelse(VI5002A == "Sim", "Recebe Bolsa Família", "Não Recebe Bolsa Família"),
         UF = fct_relevel(UF, sort)) %>% 
  ggplot(aes(x = prop, y = reorder(UF, desc(UF)), fill = reorder(V4072A, desc(V4072A)))) +
  geom_col(position = "stack") +
  facet_wrap(~ VI5002A) +
  labs(title = "Como as pessoas buscam empregos", 
       x = "Maneiras que as pessoas buscam emprego (%)",
       y = "Unidade Federativa", fill = "Opções de resposta na PNAD") +
  theme(legend.position = "bottom", legend.direction="vertical") +
  scale_x_continuous(labels = scales::percent) +
  scale_fill_viridis_d(direction = -1)

ggsave("busca_emprego.png", width = 10, height = 8, dpi = 600)

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
  theme_void() +
  theme(legend.position = "none", strip.text.x = element_text(size = 30),
        plot.background = element_rect(fill = 'white'))

ggsave("mapa_carteira_assinada.png", height = 12, width = 24, dpi = 600)

