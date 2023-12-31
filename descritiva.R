library(PNADcIBGE)
library(srvyr)
library(tidyverse)
library(RColorBrewer)
library(sf)
library(geobr)
library(stringi)
library(openxlsx)


df <- PNADcIBGE::get_pnadc(2023, 3, labels = FALSE, design = TRUE)

#PNAD CONTINUA
df <- PNADcIBGE::read_pnadc(microdata = "dados_pnad/PNADC_032023.txt", 
                            input_txt = "dados_pnad/input_PNADC_trimestral.txt") %>% 
  pnadc_design()

#PNAD ANUAL TRIMESTRE 4
df <- PNADcIBGE::read_pnadc(microdata = "dados_pnad/PNADC_2022_trimestre4.txt", 
                            input_txt = "dados_pnad/input_PNADC_trimestre4.txt") %>% 
  pnadc_design()

municipios <- readxl::read_excel("municipios.xls", skip = 4) %>% 
  select(UF, nome = Nome_UF) %>% 
  distinct()


#Força de trabalho

forca_trabalho <- readxl::read_excel("tabelas/forca_trabalho.xlsx", skip = 3)

forca_trabalho %>% 
  slice(1:47) %>% 
  select(data = "...2",
         populacao = "Pessoas de 14 anos ou mais de idade (Mil pessoas)",
         ocupadas_formais = "Pessoas de 14 anos ou mais de idade ocupadas na semana de referência (Mil pessoas)",
         ocupadas_informais = "Pessoas de 14 anos ou mais de idade ocupadas, em situação de informalidade, na semana de referência (Mil pessoas)",
         desocupadas = "Pessoas de 14 anos ou mais de idade, desocupadas na semana de referência (Mil pessoas)",
         fora_forca = "Pessoas de 14 anos ou mais de idade, fora da força de trabalho, na semana de referência (Mil pessoas)") %>% 
  mutate(across(c(populacao:fora_forca), ~ na_if(., "...")),
         across(c(populacao:fora_forca), as.numeric),
         ocupadas_formais = ifelse(is.na(ocupadas_informais), ocupadas_formais, ocupadas_formais - ocupadas_informais)) %>% 
  pivot_longer(ocupadas_formais:fora_forca) %>% 
  mutate(ano = str_sub(data, 14, 19) %>% as.numeric(),
         trimestre = str_sub(data, 1, 1) %>% as.numeric(),
         data = clock::year_quarter_day(ano, trimestre, 1) %>% as.Date()) %>% 
  ggplot(aes(x = data)) +
  geom_col(aes(y = value, fill = factor(name, labels = c("Desocupadas",
                                                         "Fora da força de trabalho",
                                                         "Ocupadas",
                                                         "Ocupadas, em empregos informais")))) +
  geom_line(aes(y = populacao)) +
  scale_fill_viridis_d() +
  labs(x = "Data", y = "Número de pessoas (milhares)", fill = "", title = "Força de trabalho no Brasil")

ggsave("imagens/forca_trabalho.png", width = 12, height = 7, dpi = 600)

#Inclusão Digital BRASIL----
inclusao_digital <- df %>% 
  srvyr::as_survey(strata = stype) %>% 
  srvyr::group_by(UF, VI5002A, across(c(starts_with("S07002"), starts_with("S07004"), starts_with("S07004A"), S07006, S07007))) %>% 
  srvyr::survey_tally() %>% 
  select(-n_se)

inclusao_digital %>% 
  left_join(municipios) %>% 
  select(-UF, UF = nome) %>% 
  filter(VI5002A != 9) %>% 
  pivot_longer(!c(UF, n, VI5002A)) %>% 
  drop_na() %>% 
  mutate(sim = ifelse(value == 1, n, 0)) %>% 
  group_by(UF, VI5002A, name) %>% 
  summarize(prop = sum(sim)/sum(n)) %>% 
  pivot_wider(id_cols = c(UF, VI5002A), names_from = name, values_from = prop) %>% 
  mutate(VI5002A = ifelse(VI5002A == 1, "Recebe", "Não Recebe")) %>% 
  select(UF, "Bolsa Família" = VI5002A, everything()) %>% 
  openxlsx::write.xlsx("tabelas/inclusao_digital_UF.xlsx")

# Motivos para não ter internet ----
freq_internet <- df %>% 
  srvyr::as_survey(strata = stype) %>% 
  srvyr::group_by(UF, VI5002A, S07001A) %>% 
  srvyr::survey_tally() %>% 
  drop_na() %>% 
  filter(VI5002A != 9) %>% 
  select(-n_se) %>% 
  mutate(VI5002A = ifelse(VI5002A == 1, "recebe", "nao_recebe") )

freq_n_total <- freq_internet %>% 
  group_by(UF, VI5002A) %>% 
  summarize(n_total = sum(n))
  
freq_internet %>% 
  left_join(freq_n_total) %>% 
  ungroup() %>% 
  mutate(prop = n / n_total,
         across(c(S07001A), ~ ifelse(. == 1, prop, 0))) %>% 
  group_by(UF, VI5002A) %>% 
  summarize(across(c(S07001A), sum)) %>% 
  pivot_wider(id_cols = UF, names_from = VI5002A, values_from = c(S07001A)) %>% 
  left_join(municipios) %>% 
  select(-UF, UF = nome) %>% 
  openxlsx::write.xlsx("tabelas/freq_net.xlsx")

#Quais ocupações mais empregam----
ocupacoes_emprego <- df %>% 
  srvyr::as_survey(strata = stype) %>% 
  srvyr::group_by(UF, V4010) %>% 
  srvyr::survey_tally() %>% 
  drop_na() %>% 
  mutate(grupo = case_when(startsWith(as.character(V4010), "00000") ~ "Ocupações maldefinidas",
                           startsWith(as.character(V4010), "0") ~ "Membros das forças armadas, policiais e bombeiros militares",
                           startsWith(as.character(V4010), "1") ~ "Diretores e gerentes",
                           startsWith(as.character(V4010), "2") ~ "Profissionais das ciências e intelectuais",
                           startsWith(as.character(V4010), "3") ~ "Técnicos e profissionais de nível médio",
                           startsWith(as.character(V4010), "4") ~ "Trabalhadores de apoio administrativo",
                           startsWith(as.character(V4010), "5") ~ "Trabalhadores dos serviços, vendedores dos comércios e mercados",
                           startsWith(as.character(V4010), "6") ~ "Trabalhadores qualificados da agropecuária, florestais, da caça e da pesca",
                           startsWith(as.character(V4010), "7") ~ "Trabalhadores qualificados, operários e artesões da construção, das artes mecânicas e outros ofícios",
                           startsWith(as.character(V4010), "8") ~ "Operadores de instalações e máquinas e montadores",
                           startsWith(as.character(V4010), "9") ~ "Ocupações elementares",
                           TRUE ~ "number")) %>% 
  group_by(UF, grupo) %>% 
  summarize(n = sum(n))

ocupacoes_emprego.tabela <- ocupacoes_emprego %>%
  group_by(UF) %>% 
  summarize(sum_n = sum(n)) %>% 
  right_join(ocupacoes_emprego) %>% 
  group_by(UF, grupo) %>% 
  summarize(sum_n = mean(sum_n),
            n = sum(n)) %>% 
  mutate(prop = n/sum_n) %>% 
  left_join(municipios) %>% 
  select(-UF, UF = nome)


ocupacoes_emprego.tabela %>% 
  pivot_wider(id_cols = UF, names_from = grupo, values_from = prop) %>% 
  openxlsx::write.xlsx("tabelas/ocupacoes_emprego.xlsx")

#Quais setores mais empregam ----
setor_emprego <- df %>% 
  srvyr::as_survey(strata = stype) %>% 
  srvyr::group_by(UF, V4013) %>% 
  srvyr::survey_tally() %>% 
  drop_na() %>% 
  mutate(grupo = case_when(startsWith(as.character(V4013), "00000") ~ "Atividades maldefinidas",
                           V4013 > 01101 & V4013 < 03002 ~ "Agricultura, pecuária, produção florestal, pesca e aquicultura",
                           V4013 > 05000 & V4013 < 39000 ~ "Indústria geral",
                           V4013 > 41000 & V4013 < 43000 ~ "Construção",
                           V4013 > 45010 & V4013 < 48100 ~ "Comércio; reparação de veículos automotores e motocicletas",
                           V4013 > 49010 & V4013 < 53002 ~ "Transporte, armazenagem e correio",
                           V4013 > 55000 & V4013 < 56020 ~ "Alojamento e alimentação ",
                           V4013 > 58000 & V4013 < 82009 ~ "Informação, comunicação e atividades financeiras, imobiliárias, profissionais e administrativas ",
                           V4013 > 84011 & V4013 < 88000 ~ "Administração pública, defesa e seguridade social, educação, saúde humana e serviços sociais",
                           V4013 == 97000 ~ "Serviços domésticos",
                           (V4013 > 90000 & V4013 < 96090) | V4013 == 99000 ~ "Outros serviços")) %>% 
  group_by(UF, grupo) %>% 
  summarize(n = sum(n)) %>% 
  drop_na()

setor_emprego.tabela <- setor_emprego %>%
  group_by(UF) %>% 
  summarize(sum_n = sum(n)) %>% 
  right_join(setor_emprego) %>% 
  group_by(UF, grupo) %>% 
  summarize(sum_n = mean(sum_n),
            n = sum(n)) %>% 
  mutate(prop = n/sum_n)  %>% 
  left_join(municipios) %>% 
  select(-UF, UF = nome) 

setor_emprego.tabela %>% 
  pivot_wider(id_cols = UF, names_from = grupo, values_from = prop) %>% 
  openxlsx::write.xlsx("tabelas/setor_emprego.xlsx")




#Por onde pessoas n?o ocupadas buscam emprego, separado BF ----
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
    "Tomou medida para iniciar o pr?prio neg?cio" = c("Tomou medida para iniciar o pr?prio neg?cio (recursos financeiros, local para instala??o, equipamentos, legaliza??o etc.)"),
    "Consultou ou inscreveu-se em ag?ncia de emprego" = c("Consultou ou inscreveu-se em ag?ncia de emprego municipal, estadual ou no Sistema Nacional de Emprego (SINE)",
                                                          "Consultou ou inscreveu-se em ag?ncia de emprego privada ou sindicato"),
    "Entrou em contato com empregador" = c("Entrou em contato com empregador (pessoalmente, por telefone, por email ou pelo portal da empresa, inclusive enviando curr?culo)"),
    "N?o tomou provid?ncia / Tomou outra provid?ncia" = c("Tomou outra provid?ncia, especifique:",
                                                          "N?o tomou provid?ncia efetiva"))) %>% 
  group_by(UF, VI5002A, V4072A) %>% 
  summarize(sum_n = mean(sum_n),
            n = sum(n)) %>% 
  mutate(prop = n/sum_n) %>% 
  filter(VI5002A != "Ignorado") %>% 
  mutate(VI5002A = ifelse(VI5002A == "Sim", "Recebe Bolsa Fam?lia", "N?o Recebe Bolsa Fam?lia"),
         UF = fct_relevel(UF, sort))

busca_emprego.tabela %>% 
  ggplot(aes(x = prop, y = reorder(UF, desc(UF)), fill = reorder(V4072A, desc(V4072A)))) +
  geom_col(position = "stack") +
  facet_wrap(~ VI5002A) +
  labs(title = "Como as pessoas buscam empregos", 
       x = "Maneiras que as pessoas buscam emprego (%)",
       y = "Unidade Federativa", fill = "Op??es de resposta na PNAD") +
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
         VI5002A = ifelse(VI5002A == "Sim", "Recebe Bolsa Fam?lia", "N?o Recebe Bolsa Fam?lia")) %>%
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


## motivos do desalento
motivos_desalentados <- df %>% 
  srvyr::as_survey(strata = stype) %>% 
  srvyr::filter(V4071 == 2, V4073 == 1, V4077 == 1) %>% 
  srvyr::group_by(V4074A) %>% 
  srvyr::survey_tally()

motivos_desalentados %>% 
  select(-n_se) %>% 
  slice(3:6) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = factor(V4074A, labels = c("Não conseguia trabalho adequado", "Não tinha experiência profissional ou qualificação", "Não conseguia trabalho por ser considerado muito jovem ou muito idoso", "Não havia trabalho na localidade")), y = prop)) + 
  geom_col() + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = "motivo", title = "Motivos declarados para o desalento", y = "percentual") + 
  coord_flip()
ggsave("imagens/motivos_declarados.png", height = 6, width = 10, dpi = 600)

## perfil dos desalentados
desalentados_uf <- df %>% 
  srvyr::as_survey(strata = stype) %>% 
  srvyr::filter(V4071 == 2, V4073 == 1, V4077 == 1, V4074A %in% c("03", "04", "05", "06")) %>% 
  srvyr::group_by(UF) %>% 
  srvyr::survey_tally() %>% 
  select(-n_se)

desalentados_uf %>% left_join(municipios) %>% 
  select(-UF, UF = nome) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = reorder(UF, prop), y = prop)) + 
  geom_col() + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = "estado", title = " Distribuição estadual do desalento no 3º trimestre de 2023", y = "percentual") +
  coord_flip()

ggsave("imagens/desalentados_uf.png", height = 8, width = 12, dpi = 600)

desalentados_genero <- df %>% 
  srvyr::as_survey(strata = stype) %>% 
  srvyr::filter(V4071 == 2, V4073 == 1, V4077 == 1, V4074A %in% c("03", "04", "05", "06")) %>% 
  srvyr::group_by(V2007) %>% 
  srvyr::survey_tally() %>% 
  select(-n_se)

desalentados_genero %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = factor(V2007, labels = c("Homem", "Mulher")), y = prop)) + 
  geom_col() + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = "gênero", title = "Distribuição por gênero do desalento", y = "percentual")

ggsave("imagens/desalentados_genero.png", height = 6, width = 10, dpi = 600)

desalentados_raca <- df %>% 
  srvyr::as_survey(strata = stype) %>% 
  srvyr::filter(V4071 == 2, V4073 == 1, V4077 == 1, V4074A %in% c("03", "04", "05", "06")) %>% 
  srvyr::group_by(V2010) %>% 
  srvyr::survey_tally() %>% 
  select(-n_se)

desalentados_raca %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = factor(V2010, labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena")), y = prop)) + 
  geom_col() + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = "raça", title = " Distribuição por cor/raça do desalento", y = "percentual")

ggsave("imagens/desalentados_raca.png", height = 6, width = 10, dpi = 600)


desalentados_educacao <- df %>% 
  srvyr::as_survey(strata = stype) %>% 
  srvyr::filter(V4071 == 2, V4073 == 1, V4077 == 1, V4074A %in% c("03", "04", "05", "06")) %>% 
  srvyr::group_by(V3009A) %>% 
  srvyr::survey_tally() %>% 
  select(-n_se) %>% 
  drop_na()

desalentados_educacao %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = factor(V3009A, labels = c("Pré-escola", "Classe de alfabetização - CA", "Alfabetização de jovens e adultos",
                                           "Antigo primário (elementar)", "Antigo ginásio (médio 1º ciclo)", "Regular do ensino fundamental ou do 1º grau",
                                           "Educação de jovens e adultos (EJA) ou supletivo do 1º grau",
                                           "Antigo científico, clássico, etc. (médio 2º ciclo)", 
                                           "Regular do ensino médio ou do 2º grau", 
                                           "Educação de jovens e adultos (EJA) ou supletivo do 2º grau",
                                           "Superior - graduação", "Especialização de nível superior",
                                           "Mestrado", "Doutorado"))
             , y = prop)) + 
  geom_col() + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = "nível educacional", title = "Distribuição por Escolaridade do desalento", y = "percentual") +
  coord_flip()

ggsave("imagens/desalentados_educacao.png", height = 8, width = 10, dpi = 600)

desalentados_idade <- df %>% 
  srvyr::as_survey(strata = stype) %>% 
  srvyr::filter(V4071 == 2, V4073 == 1, V4077 == 1, V4074A %in% c("03", "04", "05", "06")) %>% 
  srvyr::mutate(faixa_etaria = cut(V2009, breaks = c(14, 23, 33, 43, 53, 63, Inf))) %>% 
  srvyr::group_by(faixa_etaria) %>% 
  srvyr::survey_tally() %>% 
  drop_na() %>% 
  select(-n_se)

desalentados_idade %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = faixa_etaria, y = prop)) + 
  geom_col() + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = "faixa etária", title = "Distribuição etária do desalento", y = "percentual") 

ggsave("imagens/desalentados_idade.png", height = 6, width = 10, dpi = 600)




