# Salvador 22 de Julho de 2021
# Autor: Robson Wilson Silva Pessoa


#install.packages('tidyverse')
library(readr)
dados <- read_csv("data/all_data_v00.csv", 
                  col_types = cols(X1 = col_integer(), 
                  id = col_integer(), date = col_date(format = "%Y-%m-%d")), 
                  locale = locale(encoding = "ISO-8859-1"))

library(dplyr)

# Remocao da variavel X1
dados_v01 <- select(dados,-X1)
# forma alternativa dados %>% select(-X1)


# Conhecer o banco 

names(dados_v01)
# listagem dos valores no console
dados_v01$aisp

# Transformar a variavel em fator as.factor() 
# e depois listar  os niveis levels()
levels(as.factor(dados_v01$aisp))

# pipe %>%
# gºf(x) = g(f(x))

# x     operador selecao  
dados_v02 <- dados_v01 %>% group_by(aisp,ano) %>% 
              select(homicidios) %>% 
              summarise(sum(homicidios))

# Breve visualizacao

library(ggplot2)

names(dados_v02) <- c("aisp","ano","homicidio_anual")  
names(dados_v02)
# grafico nao aprovado 
ggplot(dados_v02,aes(x=ano,y=homicidio_anual)) +
   geom_point()
 
# grafico de barras
ggplot(dados_v02,aes(x=ano,y=homicidio_anual,fill=aisp)) +
geom_bar(stat="identity")

# grafico de barras
ggplot(dados_v02) +
  geom_line(aes(x=ano,y=homicidio_anual,color=aisp))

# dplyr para filtrar ano de 2020

typeof(dados_v02$ano)

dados_v02 %>% filter(ano<2020) %>% ggplot() + 
  geom_line(aes(x=ano,y=homicidio_anual,color=aisp))

dados_v02 %>% filter(ano %in% c(2015,2018)) %>% ggplot() + 
  geom_line(aes(x=ano,y=homicidio_anual,color=aisp))

dados_v02 %>% filter(aisp=="AISP 01 - Barris") %>%
  filter(ano %in% c(2015,2018)) %>% 
  select(aisp,ano,homicidio_anual) %>%
  summarise(ano=as.character(ano),homicidio_anual) %>%
  ggplot(aes(x=ano,y=homicidio_anual)) + 
  geom_bar(stat="identity")

############# 4 dia  #########################
dados_v03 <- dados_v01 %>% 
  mutate(total_roub_oni_veic=roubo_onibus+roubo_veiculos)   

# Atividade - calculo dos percentuais de homicidios 
#  por AISP em um determinado mes 

  
  dados_v01 %>% filter(ano<2020) %>%
    group_by(ano,aisp) %>%
    summarise(percentual = 100*homicidios/sum(homicidios)) %>%
    summarise(teste = sum(percentual))
  
## Arrange
  dados_v01 %>% filter(ano==2014) %>%
    group_by(aisp) %>% select(homicidios) %>%
    arrange(homicidios,aisp) 
  
  dados_v01 %>% filter(ano==2014) %>%
    filter(aisp=="AISP 09 - Boca do Rio") %>% 
    select(mes,homicidios) %>% arrange(homicidios) %>%
    mutate(MES = month(ymd(140101) + months(mes - 1), label = TRUE)) %>%
    ggplot(aes(x=MES,y=homicidios)) + geom_bar(stat = "identity")
  
  ## Manipulacao lubridate
  ## month(ymd(080101)+years(5)+ months(2)+minutes(4),label=TRUE)
  
  dados_v01 %>% filter(ano==2014) %>%
    filter(aisp=="AISP 09 - Boca do Rio") %>%  
    summarise(months(mes - 1))
  
  library(lubridate)
  dados_v01 %>% filter(ano==2014) %>%
    filter(aisp=="AISP 09 - Boca do Rio") %>%
    select(mes,homicidios) %>% 
    mutate(MES = month(ymd(080101) + months(mes - 1), label = TRUE)) %>%
    arrange(homicidios) 
  
  
library(dplyr)
library(ggplot2)

  
  
  dados_v01 %>% filter(ano<2015) %>%
    group_by(ano,aisp) %>% select(homicidios) %>% 
    arrange(aisp) %>% arrange(aisp,desc(homicidios))
  
## Across   
  library(dplyr)
  library(tidyr)
  dados_v01 %>% summarise(across(5:13,sum)) %>% 
    gather(key,value) %>% arrange(desc(value)) %>%
    ggplot(aes(x=value,y=key))+
    geom_bar(stat = "identity")
  
  
## where
  
  ### Somatorio
  dados_v01 %>% summarise(across(where(is.numeric),sum))
  ### Media
  dados_v01 %>% summarise(across(where(is.numeric),mean))
  ### Desvio Padrao
  dados_v01 %>% summarise(across(where(is.numeric),sd))
  ### Media e Desvio Padrao 
  
  ###########
  # https://dplyr.tidyverse.org/articles/colwise.html
  library(glue)
  
  min_max <- list(
    min = ~min(.x, na.rm = TRUE), 
    max = ~max(.x, na.rm = TRUE)
  )
  
  
  dados_v01 %>% filter(ano<2020) %>% 
    summarise(across(where(is.numeric),min_max,.names = "{.fn}.{.col}"))
  
  
##   starts_with("roubo")  ends_with()
  dados_v01 %>% group_by(aisp,ano) %>% 
  summarise(across(starts_with("roubo"), list(mean = mean, sd = sd))) %>%
    arrange(aisp)
  
########################################################
# Correlação
  library(corrr)
  dados_v01 %>% 
    summarise(across(starts_with("roubo"))) %>% correlate()
  
  #gather
  #ggplot(data = dados_v04, aes(x=, y=, fill=value)) + geom_tile()
  
  

  
  dados_v01 %>%
    filter(ano >= 2014L & ano <= 2017L) %>%
    filter(aisp %in% c("AISP 07 - Rio Vermelho", "AISP 14 - Barra", 
                       "AISP 16 - Pituba")) %>%
    ggplot() +
    aes(x = date, y = homicidios, fill = aisp, colour = aisp, size = estupro) +
    geom_point(shape = "circle") +
    scale_fill_manual(values = c(`AISP 01 - Barris` = "#F8766D", `AISP 02 - Liberdade` = "#E48432", `AISP 03 - Bonfim` = "#CE9300", 
                                 `AISP 04 - São Caetano` = "#ACA000", `AISP 05 - Periperi` = "#7FAC07", `AISP 06 - Brotas` = "#31B425", 
                                 `AISP 07 - Rio Vermelho` = "#00BB4C", `AISP 08 - CIA` = "#00BF83", `AISP 09 - Boca do Rio` = "#00BEB1", 
                                 `AISP 10 - Pau da Lima` = "#00BAD5", `AISP 11 - Tancredo Neves` = "#20AFEC", `AISP 12 - Itapuã` = "#549FFB", 
                                 `AISP 13 - Cajazeiras` = "#918BFD", `AISP 14 - Barra` = "#D274FB", `AISP 15 - Nordeste` = "#EB6AE0", 
                                 `AISP 16 - Pituba` = "#FF61C3")) +
    scale_color_manual(values = c(`AISP 01 - Barris` = "#F8766D", 
                                  `AISP 02 - Liberdade` = "#E48432", `AISP 03 - Bonfim` = "#CE9300", `AISP 04 - São Caetano` = "#ACA000", 
                                  `AISP 05 - Periperi` = "#7FAC07", `AISP 06 - Brotas` = "#31B425", `AISP 07 - Rio Vermelho` = "#00BB4C", 
                                  `AISP 08 - CIA` = "#00BF83", `AISP 09 - Boca do Rio` = "#00BEB1", `AISP 10 - Pau da Lima` = "#00BAD5", 
                                  `AISP 11 - Tancredo Neves` = "#20AFEC", `AISP 12 - Itapuã` = "#549FFB", `AISP 13 - Cajazeiras` = "#918BFD", 
                                  `AISP 14 - Barra` = "#D274FB", `AISP 15 - Nordeste` = "#EB6AE0", `AISP 16 - Pituba` = "#FF61C3")) +
    theme_linedraw() +
    theme(plot.subtitle = element_text(size = 16L), axis.title.y = element_text(size = 20L), axis.title.x = element_text(size = 20L))
  
  
  