'''
title: "Covid Norte Minas Gerais"
author: "Reinaldo Junio Dias de Abreu"
date: "18/08/2021"
'''

# BASE DE DADOS UTILIZADAS
# BASE DE DADOS PRINCIPAL - COVID BRASIL - https://www.kaggle.com/unanimad/corona-virus-brazil
# BASE DOS MUNICIPIOS BRASILEIROS - https://github.com/kelvins/Municipios-Brasileiros
# BASE POPULAÇÃO BRASILEIRA - http://blog.mds.gov.br/redesuas/lista-de-municipios-brasileiros/

# IMPORTACAO BIBLIOTECAS
library(dplyr)       # Ref: https://livro.curso-r.com/7-2-dplyr.html
library(lubridate)   # Ref: http://sillasgonzaga.com/material/cdr/trabalhando-com-datas.html
library(ggplot2)     # Ref: https://livro.curso-r.com/8-1-o-pacote-ggplot2.html
library(forcats)
library(corrplot)

# IMPORTACAO DA BASE DE DADOS

setwd("/home/reinaldo/Documentos/PROJETOIA") # Formato Linux

# Dados covid 19
dados = read.csv2(file = "brazil_covid19_cities.csv", sep = ",", dec = ".", header = TRUE)
names(dados)
head(dados)

# Lista de municipios do Brasil com coordenadas
municipios = read.csv2(file = "municipios.csv", sep = ",", dec = ".", header = TRUE)
names(municipios)
head(municipios)

# Lista de municipios do Brasil com dados populacionais de 2010
municipios2 = read.csv2(file = "Lista_Municípios_com_IBGE_Brasil_Versao_CSV.csv", sep = ";", dec = ",", header = TRUE)
names(municipios2)
head(municipios2)

# Lista de municipios do Norte de Minas Gerais
municipios_norte = read.csv2(file = "cidades_norte_minas.csv", sep = ",", dec = ".", header = TRUE)
names(municipios_norte)
head(municipios_norte)

# SEPARANDO OS DADOS DE INTERESSE

# Filtrando pelas cidades de Minas Gerais
df = dados %>% filter(state == "MG")
rm(dados) # Removendo da memoria

# Unindo dados dos municipios
municipios = merge(municipios, municipios2, by.x = 'codigo_ibge', by.y = 'IBGE7' )
municipios = municipios %>% select(codigo_ibge, nome, latitude, longitude, Popula..o.2010, Porte)


# Filtrando municipios somente com as cidades do Norte de Minas
cidades = merge(municipios, municipios_norte, by.x = 'nome', by.y = 'cidades_norte', all.y = TRUE)
cidades = cidades %>% select(codigo_ibge, nome, latitude, longitude, Popula..o.2010, Porte)
names(cidades)

# Unindo os dados das cidades com dados de COVID
df2 = merge(df, cidades, by.x = 'name', by.y = 'nome', all.y = TRUE)
df2 = df2 %>% select(codigo_ibge, name, codigo_ibge, latitude, longitude, Popula..o.2010, Porte, date, cases, deaths)
names(df2)

# Armazenando em novo arquivo
write.csv2(df2, file="covid_norte_minas.csv", row.names = FALSE)

# Limpando da memoria
rm(cidades, df, df2, municipios, municipios2, municipios_norte)

# ANALISANDO BASE DE DADOS GERADA
dados = read.csv2(file = "covid_norte_minas.csv", sep = ";", dec = ",", header = TRUE)
dados %>% distinct(name)
head(dados)

# ADAPTANDO DATA (dia/mes/ano)

dados[,'dia'] = day(as.Date(dados[,'date'], format = "%Y-%m-%d"))
dados[,'mes'] = month(as.Date(dados[,'date'], format = "%Y-%m-%d"))
dados[,'ano'] = year(as.Date(dados[,'date'], format = "%Y-%m-%d"))

dados = dados %>% select(codigo_ibge, name, codigo_ibge, latitude, longitude, Popula..o.2010, Porte, dia, mes, ano, cases, deaths)
dados = na.omit(dados)

head(dados)

# DIVIDINDO DADOS POR ANO

df2020 = dados %>% filter(ano == '2020')
df2021 = dados %>% filter(ano == '2021')

# PROCESSANDO DADOS - 2020

# Segregacao em dataframes mensais em uma lista de dataframes
data = df2020

dados <- list()
for(i in 1:12){
  b = data %>% filter(mes == i)
  b = arrange(b, name, dia, mes, ano)
  dados[i] <- list(b)
}

for(i in 3:12){
  t1 = dados[[i]]
  t2 = t1 %>% group_by(name) %>% summarise( 
    porte = first(Porte),
    latitude = max(latitude),
    longitude = max(longitude),
    populacao =  max(Popula..o.2010),
    totcasos = max(cases),
    totobitos = max(deaths)
  )
  t2 = t2 %>% mutate(
    indobito = (totobitos / populacao)*1000,
    indcasos = (totcasos / populacao)*1000
  ) %>% select( name, porte, latitude, longitude, populacao, 
                totcasos, indcasos, totobitos, indobito)
  dados[[i]] = t2
  #write.csv2(t2, file = paste("casos_covid_mes_", i,".csv", sep=""), row.names = FALSE)
}

# Analisando os dados
head(dados[[12]])

df = dados[[12]]

# Municipios com maior população
df %>% 
  top_n(15, populacao) %>% 
  mutate(name = forcats::fct_reorder(name, populacao)) %>%
  ggplot() +
  geom_col(aes(x = name, y = populacao, fill = populacao), show.legend = FALSE) +
  geom_label(aes(x = name, y = populacao/2, label = populacao)) +
  coord_flip()

# Municipios com mais casos
df %>% 
  top_n(15, totcasos) %>% 
  mutate(name = forcats::fct_reorder(name, totcasos)) %>%
  ggplot() +
  geom_col(aes(x = name, y = totcasos, fill = totcasos), show.legend = FALSE) +
  geom_label(aes(x = name, y = totcasos/2, label = totcasos)) +
  coord_flip()

# Municipios com mais obitos
df %>% 
  top_n(15, totobitos) %>% 
  mutate(name = forcats::fct_reorder(name, totobitos)) %>%
  ggplot() +
  geom_col(aes(x = name, y = totobitos, fill = totobitos), show.legend = FALSE) +
  geom_label(aes(x = name, y = totobitos/2, label = totobitos)) +
  coord_flip()

# Indice de casos
# Quanto maior o indice, indica que há muitos casos em relação a populacao
# Quanto menor o indice, indica que há poucos casos em relação a populacao
df %>% 
  top_n(20, indcasos) %>% 
  mutate(name = forcats::fct_reorder(name, indcasos, .desc = FALSE)) %>%
  ggplot() +
  geom_col(aes(x = name, y = indcasos, fill = indcasos), show.legend = FALSE) +
  geom_label(aes(x = name, y = indcasos/2, label = indcasos)) +
  coord_flip()

arrange(df, desc(indcasos)) %>% top_n(20, indcasos) %>% select(name, porte, populacao, totcasos, indcasos) 

# Indice de obitos
# Quanto maior o indice, indica que há muitos obitos em relação a populacao
# Quanto menor o indice, indica que há poucos obitos em relação a populacao
df %>% 
  top_n(20, indobito) %>% 
  mutate(name = forcats::fct_reorder(name, indobito, .desc = FALSE)) %>%
  ggplot() +
  geom_col(aes(x = name, y = indobito, fill = indobito), show.legend = FALSE) +
  geom_label(aes(x = name, y = indobito/2, label = indobito)) +
  coord_flip()

arrange(df, desc(indobito)) %>% top_n(20, indobito) %>% select(name, porte, populacao, totobitos, indobito) 

# Descrição dos dados
summary(df)

# Graficos

# Boxplot relacionando o porte da cidade ao indice de casos
p = df %>% ggplot() + geom_boxplot(aes(x=porte, y=indcasos, fill = porte) )
p + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9","#5dfa14"))
p

# Boxplot relacionando o porte da cidade ao indice de obitos
p = df %>% ggplot() + geom_boxplot(aes(x=porte, y=indobito, fill = porte) )
p + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9","#5dfa14"))
p

summary(df$indcasos)
summary(df$indobito)

# Teste de Normalidade e Correlação
#(Conclusão: Em ambas variáveis existe uma diferencia significativa da média)
ggplot(df) +
  geom_point(mapping = aes(x = indcasos, y = indobito))

cor.test(df$indcasos, df$indobito)

# Correlação das variáveis
correlacao = stats::cor(x = df[,5:9])
corrplot(correlacao, method = 'number', type = 'lower')

# Teste de Normalidade
shapiro.test(df$indcasos)
shapiro.test(df$indobito)

# Histogramas
ggplot(data = df, aes(x = indcasos)) +
  geom_histogram(binwidth = 10)

hist(df$indcasos)

ggplot(data = df, aes(x = indobito)) +
  geom_histogram(binwidth = 0.1)

hist(df$indobito)

