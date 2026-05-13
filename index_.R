################################################################################
#     =====       Demanda habitacional no Distrito Federal      ====
################################################################################



# Esses códigos foram elaborados pela equipe da Diretoria de Estudos e
# Políticas Ambientais e Territoriais (DEPAT),
# sob trabalhos diretos da Coordenação de Estudos Territoriais (COET).

# O relatório da pesquisa está disponível em
# https://www.ipe.df.gov.br/deficit-e-demanda-habitacional-do-distrito-federal-2021/

# A DEPAT é parte do IPEDF Instituto de Pesquisa e Estatística do Distrito Federal (IPEDF)

# Os códigos a seguir dizem respeito ao ajuste da base de dados da Pesquisa
# de modo a permitir a expansão e realização de inferências. Há também algumas
# das tabelas utilizadas. Todos os demais agrupamentos
# e cálculos presentes no relatório foram feitos com base na metodologia.

# Pedimos a gentileza de reportarem bugs ou erros nos códigos abaixo.
# E-mail: gab-depat@ipe.df.gov.br

# Todos os dados foram obtidos a partir da Pesquisa Distrital por
# Amostra de Domicílios (PDAD) 
# O dicionário de variáveis e outras informações sobre a PDAD 2021
# podem ser encontrados no site do IPEDF:
# https://www.ipe.df.gov.br/microdados-pdad-2021/



################################################################################
#                     ===== Configurações R ====
################################################################################

# Criando um ambiente virtual

# Para manter a memória dos pacotes utilizados, de modo que possam ser totalmente reproduzidos,
# sugere-se utilizar o pacote [`renv`](https://rstudio.github.io/renv/articles/renv.html). 
# 
# Para isso, o pacote deve ser instalado inicialmente. Este procedimento foi feito 
# com o renv 1.0.2.
# 
# ## Adotando a versão correta dos pacotes.
# 
# Junto com esse script, estão disponíveis os arquivos `renv.lock`, `.Rprofile`,
# `renv/settings.json` e `renv/activate.R`.
#
# Para que os pacotes sejam os mesmos utilizados neste projeto,
# as informações das versões dos pacotes ficarão registradas no arquivo `renv.lock`.
# Com esse arquivo na mesma pasta do projeto, pode-se recuperar as versões adequadas
# em qualquer outra máquina com conexão ao repositório
# ou à internet com a função `renv::restore()`.



# Configurações opcionais
rm(list = ls(all = TRUE))

options(scipen = 999)


################################################################################
# ===== 0. Carregar pacotes e leitura dos dados ====
################################################################################



# Pacotes necessários

  # O pacote `pacman`, por meio de sua função p_load()
  # é utilizado para instalar e carregar os pacotes necessários
  # Primeiro, testa-se se o pacote está instalado. Se não, esta será feita.
if(!require("pacman")) install.packages("pacman")

pacman::p_load(
               tidyverse,     # Pacote para manipulação de dados
               readxl,        # Pacote para leitura de arquivos .xls e .xlsx
               lubridate,     # Pacote para manipulação de datas
               data.table,    # Pacote para manipulação de dados
               survey,        # Pacote para manipulação de dados survey
               srvyr          # Pacote que ajusta a família tidyverse para dados survey        
               )   



pdad_dom_2021 <- read_csv2('dados/01_bruto/PDAD_2021-Domicilios.csv')
pdad_mor_2021 <- read_csv2('dados/01_bruto/PDAD_2021-Moradores.csv')


################################################################################
#                 ===== Ajustes iniciais ====
################################################################################


# O objetivo é calcular a demanda habitacional com a taxa de chefia tradicional
# - que leva em conta todos os chefes daquela faixa etária.


  # Fazer a junção das bases de moradores e domicílios
pdad_2021 <- pdad_dom_2021 %>%
  # Trazer as informações de pessoas para domicílios
  dplyr::left_join(
    pdad_mor_2021 #%>%
    #dplyr::select(-FATOR_PROJ)
    ,
    by = c(
      "A01ra"     = "A01ra",
      "A01nficha" = "A01nficha",
      "A01setor"  = "A01setor"
    )
  ) %>%
  # Ajustar o nome das RAs
  dplyr::mutate(
    RA_nome = factor(
      case_when(
        A01ra == 1 ~ "Plano Piloto",
        A01ra == 2 ~ "Gama",
        A01ra == 3 ~ "Taguatinga",
        A01ra == 4 ~ "Brazlândia",
        A01ra == 5 ~ "Sobradinho",
        A01ra == 6 ~ "Planaltina",
        A01ra == 7 ~ "Paranoá",
        A01ra == 8 ~ "Núcleo Bandeirante",
        A01ra == 9 ~ "Ceilândia",
        A01ra == 10 ~ "Guará",
        A01ra == 11 ~ "Cruzeiro",
        A01ra == 12 ~ "Samambaia",
        A01ra == 13 ~ "Santa Maria",
        A01ra == 14 ~ "São Sebastião",
        A01ra == 15 ~ "Recanto das Emas",
        A01ra == 16 ~ "Lago Sul",
        A01ra == 17 ~ "Riacho Fundo",
        A01ra == 18 ~ "Lago Norte",
        A01ra == 19 ~ "Candangolândia",
        A01ra == 20 ~ "Águas Claras",
        A01ra == 21 ~ "Riacho Fundo II",
        A01ra == 22 ~ "Sudoeste/Octogonal",
        A01ra == 23 ~ "Varjão",
        A01ra == 24 ~ "Park Way",
        A01ra == 25 ~ "SCIA/Estrutural",
        A01ra == 26 ~ "Sobradinho II",
        A01ra == 27 ~ "Jardim Botânico",
        A01ra == 28 ~ "Itapoã",
        A01ra == 29 ~ "SIA",
        A01ra == 30 ~ "Vicente Pires",
        A01ra == 31 ~ "Fercal",
        A01ra == 32 ~ "Sol Nascente/Pôr do Sol",
        A01ra == 33 ~ "Arniqueira"
      )
    ),
    grupos_ped = ifelse(
      RA_nome == 'Plano Piloto' |
        RA_nome == 'Jardim Botânico' |
        RA_nome == 'Lago Norte' |
        RA_nome == 'Lago Sul' |
        RA_nome == 'Park Way' |
        RA_nome == 'Sudoeste/Octogonal',
      "Grupo 1",
      ifelse(
        RA_nome == 'Águas Claras' |
          RA_nome == 'Arniqueira' |
          RA_nome == 'Candangolândia' |
          RA_nome == 'Cruzeiro' |
          RA_nome == 'Gama' |
          RA_nome == 'Guará' |
          RA_nome == 'Núcleo Bandeirante' |
          RA_nome == 'Sobradinho' |
          RA_nome == 'Sobradinho II' |
          RA_nome == 'Taguatinga' |
          RA_nome == 'Vicente Pires',
        "Grupo 2",
        ifelse(
          RA_nome == 'Brazlândia' |
            RA_nome == 'Ceilândia' |
            RA_nome == 'Planaltina' |
            RA_nome == 'Riacho Fundo' |
            RA_nome == 'Riacho Fundo II' |
            RA_nome == 'Samambaia' |
            RA_nome == 'Santa Maria' |
            RA_nome == 'São Sebastião' |
            RA_nome == 'SIA',
          "Grupo 3",
          ifelse(
            RA_nome == 'Fercal' |
              RA_nome == 'Itapoã' |
              RA_nome == 'Paranoá' |
              RA_nome == 'Recanto das Emas' |
              RA_nome == 'SCIA/Estrutural' |
              RA_nome == 'Varjão' |
              RA_nome == 'Sol Nascente/Pôr do Sol',
            "Grupo 4",
            NA
          )
        )
      )
    ),
    # Ajustar os setores de interesse da PDAD 2018
    ra_setor = factor(
      case_when(
        A01setor == 53011 ~ "Asa Norte",
        A01setor == 53012 ~ "Asa Sul",
        A01setor == 53013 ~ "Noroeste",
        A01setor == 53014 ~ "Demais",
        A01setor == 53020 ~ "Gama",
        A01setor == 53030 ~ "Taguatinga",
        A01setor == 53040 ~ "Brazlândia",
        A01setor == 53050 ~ "Sobradinho",
        A01setor == 53060 ~ "Planaltina",
        A01setor == 53070 ~ "Paranoá",
        A01setor == 53080 ~ "Núcleo Bandeirante",
        A01setor == 53090 ~ "Ceilândia",
        A01setor == 53100 ~ "Guará",
        A01setor == 53110 ~ "Cruzeiro",
        A01setor == 53120 ~ "Samambaia",
        A01setor == 53130 ~ "Santa Maria",
        A01setor == 53140 ~ "São Sebastião",
        A01setor == 53150 ~ "Recanto Das Emas",
        A01setor == 53160 ~ "Lago Sul",
        A01setor == 53170 ~ "Riacho Fundo",
        A01setor == 53180 ~ "Lago Norte",
        A01setor == 53190 ~ "Candangolândia",
        A01setor == 53200 ~ "Águas Claras",
        A01setor == 53210 ~ "Riacho Fundo II",
        A01setor == 53220 ~ "Sudoeste/Octogonal",
        A01setor == 53230 ~ "Varjão",
        A01setor == 53240 ~ "Park Way",
        A01setor == 53250 ~ "SCIA-Estrutural",
        A01setor == 53260 ~ "Sobradinho II",
        A01setor == 53271 ~ "Jardim Botânico - Tradicional",
        A01setor == 53272 ~ "Jardim Mangueiral",
        A01setor == 53280 ~ "Itapoã",
        A01setor == 53290 ~ "SIA",
        A01setor == 53300 ~ "Vicente Pires",
        A01setor == 53310 ~ "Fercal",
        A01setor == 53320 ~ "Sol Nascente/Pôr do Sol",
        A01setor == 53330 ~ "Arniqueira"
      )
    )
  )


  # Excluir as bases parciais
rm(pdad_dom_2021, pdad_mor_2021)

  # Deixar apenas os domicílios onde o responsável tem entre 24 e 64 anos.
pdad_2021_original <- pdad_2021

################################################################################
#                         Expansão da amostra 
################################################################################


  # Declarar o desenho incial
sample_pdad <- 
  survey::svydesign(id      = ~A01nficha,        # Identificador único da unidade amostrada
                    strata  = ~A01setor,         # Identificação do estrato
                    weights = ~PESO_MOR,         # Inverso da fração amostral
                    nest    = TRUE,              # Parâmetro de tratamento para os IDs dos estratos
                    data    = pdad_2021_original # Declarar a base a ser utilizada
  )

  # Criar um objeto para pós estrato
post_pop <- pdad_2021_original %>%
  dplyr::group_by(POS_ESTRATO) %>%              # Agrupar por pós-estrato
  dplyr::summarise(Freq=max(POP_AJUSTADA_PROJ)) # Capturar o total da população

  # Declarar o objeto de pós-estrato
  # Estamos dizendo nesse passo qual é a população alvo para cada
  # pós-estrato considerado
amostra <-
  survey::postStratify(sample_pdad,  ~ POS_ESTRATO, post_pop)

  # Ajustar para tratamento de estratos com apenas uma UPA (adjust=centered)
options(survey.lonely.psu = "adjust")

  # Ajustar objeto de amostra, para uso com o pacote srvyr (como tibble)
amostra_mor <- srvyr::as_survey(amostra)

  # Exclui arquivos não mais usados
rm(amostra, sample_pdad, post_pop, pdad_2021_original)


################################################################################
#                         Resultados e inferências
################################################################################


  # Calculando a taxa de chefia por grupo etário.
tx_chefia <- amostra_mor %>% 
  srvyr::mutate(grupo_etario = case_when(idade %in% c(24:29) ~ "idade_24_29",
                                         idade %in% c(30:39) ~ "idade_30_39",
                                         idade %in% c(40:64) ~ "idade_40_64")) %>% 
  srvyr::filter(E05==1) %>% 
  srvyr::group_by(grupo_etario) %>% 
  srvyr::summarise(total_chefes=survey_total(vartype = c("cv", "ci")))

tx_chefia <- subset(tx_chefia, !is.na(grupo_etario))

  # Calculando o total de pessoas na população.
pop_total <- amostra_mor %>%
  srvyr::mutate(
    grupo_etario = case_when(
      idade %in% c(24:29) ~ "idade_24_29",
      idade %in% c(30:39) ~ "idade_30_39",
      idade %in% c(40:64) ~ "idade_40_64"
    )
  ) %>%
  srvyr::group_by(grupo_etario) %>%
  srvyr::summarise(pop_total = survey_total(vartype = c("cv", "ci")))

pop_total <- subset(pop_total,!is.na(grupo_etario))

  # Juntando as tabelas para calcular a taxa de chefia por idade.
tx_chefia_idade <- cbind(tx_chefia, pop_total[, -1])

tx_chefia_idade$tx_chefia <-
  tx_chefia_idade$total_chefes / tx_chefia_idade$pop_total

  # Teste para ver se é possível fazer inferência sobre os dados.
  # Necessário coeficiente de variação (cv) menor do que 25%
tx_chefia_idade <- as.data.frame(tx_chefia_idade)

tx_chefia_idade$tx_chefia <-
  ifelse(tx_chefia_idade$total_chefes_cv > 0.25,
         NA,
         tx_chefia_idade$tx_chefia)

rm(tx_chefia, pop_total)


  # Cálculo a taxa de chefia por idade e RA.
tx_chefia_idade_ra <- amostra_mor %>% 
  srvyr::mutate(grupo_etario = case_when(idade %in% c(24:29) ~ "idade_24_29",
                                         idade %in% c(30:39) ~ "idade_30_39",
                                         idade %in% c(40:64) ~ "idade_40_64")) %>% 
  srvyr::filter(E05==1) %>% 
  srvyr::group_by(grupo_etario, RA_nome) %>% 
  srvyr::summarise(total_chefes=survey_total(vartype = c("cv", "ci")))

tx_chefia_idade_ra <- subset(tx_chefia_idade_ra, !is.na(grupo_etario))

pop_idade_ra <- amostra_mor %>% 
  srvyr::mutate(grupo_etario = case_when(idade %in% c(24:29) ~ "idade_24_29",
                                         idade %in% c(30:39) ~ "idade_30_39",
                                         idade %in% c(40:64) ~ "idade_40_64")) %>% 
  srvyr::group_by(grupo_etario, RA_nome) %>% 
  srvyr::summarise(pop_total=survey_total(vartype = c("cv", "ci")))

pop_idade_ra <- subset(pop_idade_ra, !is.na(grupo_etario))

tx_chefia_idade_raf <-
  cbind(tx_chefia_idade_ra, pop_idade_ra[, -c(1, 2)])

tx_chefia_idade_raf$tx_chefia <-
  tx_chefia_idade_raf$total_chefes / tx_chefia_idade_raf$pop_total

tx_chefia_idade_raf <-
  subset(tx_chefia_idade_raf,!is.na(grupo_etario))

  # Teste para ver se inferências podem ser feitas 
tx_chefia_idade_raf <- as.data.frame(tx_chefia_idade_raf)

tx_chefia_idade_raf$tx_chefia <-
  ifelse(tx_chefia_idade_raf$total_chefes_cv > 0.25,
         NA,
         tx_chefia_idade_raf$tx_chefia)


   # Calcular o número de adultos que têm demanda habitacional por grupo etário.
demanda_idade <- amostra_mor %>%
   srvyr::filter(demanda > 0 & !(E05 %in% c(1:3))) %>%
   srvyr::mutate(demanda_ind = ifelse(idade %in% c(24:64) &
                                        conj_casal == 0, 1, 0)) %>%
   srvyr::mutate(
     grupo_etario = case_when(
       idade %in% c(24:29) ~ "idade_24_29",
       idade %in% c(30:39) ~ "idade_30_39",
       idade %in% c(40:64) ~ "idade_40_64"
     )
   ) %>%
   srvyr::group_by(grupo_etario) %>%
   srvyr::summarise(demanda_ = survey_total(demanda_ind, vartype = c("cv", "ci"), na.rm =
                                              TRUE))
 
 
  # Teste para ver se é possível fazer inferência sobre os dados.
 demanda_idade <- as.data.frame(demanda_idade)
 
 demanda_idade$demanda_ <-
   ifelse(demanda_idade$demanda__cv > 0.25, NA, demanda_idade$demanda_)
 
 demanda_idade <- subset(demanda_idade,!is.na(grupo_etario))
 
 demanda_idade_final  <-
   merge(demanda_idade, tx_chefia_idade, by = c("grupo_etario"))
 
 demanda_idade_final$demanda_final <-
   demanda_idade_final$demanda_ * demanda_idade_final$tx_chefia

  # Tabela final 
demanda_idade_final 
 


  # Calcular o número de adultos que têm demanda habitacional por grupo etário e RA.
demanda_idade_RA <- amostra_mor %>%
  srvyr::filter(demanda > 0 & !(E05 %in% c(1:3))) %>%
  srvyr::mutate(demanda_ind = ifelse(idade %in% c(24:64) &
                                       conj_casal == 0, 1, 0)) %>%
  srvyr::mutate(
    grupo_etario = case_when(
      idade %in% c(24:29) ~ "idade_24_29",
      idade %in% c(30:39) ~ "idade_30_39",
      idade %in% c(40:64) ~ "idade_40_64"
    )
  ) %>%
  srvyr::group_by(grupo_etario, RA_nome) %>%
  srvyr::summarise(demanda_ = survey_total(demanda_ind, vartype = c("cv", "ci"), na.rm =
                                             TRUE))

  # Teste para ver se é possível fazer inferência sobre os dados.
demanda_idade_RA <- as.data.frame(demanda_idade_RA)

demanda_idade_RA$demanda_ <-
  ifelse(demanda_idade_RA$demanda__cv > 0.25,
         NA,
         demanda_idade_RA$demanda_)

demanda_idade_RA <- subset(demanda_idade_RA,!is.na(grupo_etario))

demanda_idade_RA_final  <-
  merge(demanda_idade_RA,
        tx_chefia_idade_raf,
        by = c("grupo_etario", "RA_nome"))

demanda_idade_RA_final$demanda_final <-
  demanda_idade_RA_final$demanda_ * demanda_idade_RA_final$tx_chefia

 # Tabela final
demanda_idade_RA_final

 # Remover tabelas não mais usadas
rm(tx_chefia_idade, tx_chefia_idade_ra, tx_chefia_idade_raf,
   demanda_idade_RA, pop_idade_ra)



  # Comparação grupo etário e rendimento
demanda_ss <- amostra_mor %>% 
  srvyr::filter(demanda>0) %>% 
  srvyr::mutate(demanda_ind=ifelse(idade %in% c(24:64) & conj_casal==0, 1, 0)) %>% 
  srvyr::mutate(grupo_etario = case_when(idade %in% c(24:29) ~ "idade_24_29",
                                         idade %in% c(30:39) ~ "idade_30_39",
                                         idade %in% c(40:64) ~ "idade_40_64"),
                arranjo2 = ifelse(arranjos %in% c(1,3:6),"Uni/Casais",
                                  ifelse(arranjos == 2,"Monoparental Fem",
                                         ifelse(arranjos == 7,"Outro",NA)))) %>%
  srvyr::group_by(grupo_etario, arranjo2) %>% 
  srvyr::summarise(demanda_=survey_total(vartype = c("cv", "ci"), na.rm=TRUE)) %>% 
  dplyr::filter(!is.na(grupo_etario))

tx_chefia <- amostra_mor %>% 
  srvyr::mutate(demanda_ind=ifelse(idade %in% c(24:64) & conj_casal==0, 1, 0)) %>% 
  srvyr::mutate(grupo_etario = case_when(idade %in% c(24:29) ~ "idade_24_29",
                                         idade %in% c(30:39) ~ "idade_30_39",
                                         idade %in% c(40:64) ~ "idade_40_64"),
                arranjo2 = ifelse(arranjos %in% c(1,3:6),"Uni/Casais",
                                  ifelse(arranjos == 2,"Monoparental Fem",
                                         ifelse(arranjos == 7,"Outro",NA)))) %>% 
  srvyr::filter(E05==1) %>% 
  srvyr::group_by(grupo_etario) %>% 
  srvyr::summarise(total_chefes=survey_total(vartype = c("cv", "ci"))) %>% 
  dplyr::filter(!is.na(grupo_etario))

pop_total <- amostra_mor %>% 
  srvyr::mutate(demanda_ind=ifelse(idade %in% c(24:64) & conj_casal==0, 1, 0)) %>% 
  srvyr::mutate(grupo_etario = case_when(idade %in% c(24:29) ~ "idade_24_29",
                                         idade %in% c(30:39) ~ "idade_30_39",
                                         idade %in% c(40:64) ~ "idade_40_64")) %>% 
  srvyr::group_by(grupo_etario) %>% 
  srvyr::summarise(pop_total=survey_total(vartype = c("cv", "ci"))) %>% 
  dplyr::filter(!is.na(grupo_etario))

tb_tx_chefia <- 
demanda_ss %>% 
  inner_join(tx_chefia) %>% 
  inner_join(pop_total) %>% 
  select(-contains("cv"), -contains("low"), -contains("upp")) %>% 
  mutate(demanda_final = demanda_*total_chefes/pop_total) %>% 
  janitor::adorn_totals()

tb_demanda_geral <- 
demanda_idade %>% 
  inner_join(tx_chefia) %>% 
  inner_join(pop_total) %>% 
  select(-contains("cv"), -contains("low"), -contains("upp")) %>% 
  mutate(demanda_final = demanda_*total_chefes/pop_total) %>% 
  janitor::adorn_totals() 

tb_tx_chefia <- 
tb_demanda_geral %>% 
  mutate(tx.chefia = total_chefes/pop_total) %>% 
  select(grupo_etario,tx.chefia) 

  # Tabela de renda domiciliar dos demandantes
tb_renda_demandante <-
  amostra_mor %>%
  srvyr::filter(demanda > 0 & !(E05 %in% c(1:3))) %>%
  srvyr::mutate(demanda_ind = ifelse(idade %in% c(24:64) &
                                       conj_casal == 0, 1, 0)) %>%
  srvyr::mutate(
    grupo_etario = case_when(
      idade %in% c(24:29) ~ "idade_24_29",
      idade %in% c(30:39) ~ "idade_30_39",
      idade %in% c(40:64) ~ "idade_40_64"
    ),
    renda_dom = case_when(
      renda_ind_r <= 1100 ~ "ate01SM",
      renda_ind_r > 1100 &
        renda_ind_r <= 3300 ~ "1a3SM",
      renda_ind_r > 3300 &
        renda_ind_r <= 5500 ~ "3a5SM",
      renda_ind_r > 5500 &
        renda_ind_r <= 13200 ~ "5a12SM",
      renda_ind_r > 13200 ~ "maisde12SM"
    )
  ) %>%
  srvyr::group_by(grupo_etario, renda_dom) %>%
  srvyr::summarise(demanda_ = survey_total(demanda_ind, vartype = c("cv", "ci"), na.rm =
                                             TRUE)) %>%
  dplyr::filter(!is.na(grupo_etario)) %>%
  inner_join(tb_tx_chefia) %>%
  mutate(demanda_final = demanda_ * tx.chefia) %>%
  select(grupo_etario, renda_dom, demanda_final) %>%
  tidyr::spread(grupo_etario, demanda_final) 

  # Tabela final
tb_renda_demandante

  # Remove as tabelas não mais utilizadas
rm(demanda_idade, demanda_ss, tx_chefia, pop_total, tb_tx_chefia, tb_demanda_geral)

################################################################################
#                               FIM 
################################################################################