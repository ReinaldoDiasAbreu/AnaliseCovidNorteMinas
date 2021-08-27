'''
title: "Covid Norte Minas Gerais"
author: "Reinaldo Junio Dias de Abreu"
date: "18/08/2021"
'''

# BASE DE DADOS UTILIZADAS
# BASE DE DADOS PRINCIPAL - COVID BRASIL - https://www.kaggle.com/unanimad/corona-virus-brazil
# BASE DOS MUNICIPIOS BRASILEIROS - https://github.com/kelvins/Municipios-Brasileiros
# BASE POPULAÇÃO BRASILEIRA - http://blog.mds.gov.br/redesuas/lista-de-municipios-brasileiros/
# BASE DENSIDADE DEMOGRAFICA - https://sidra.ibge.gov.br/tabela/1301

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

# Densidade demografica IBGE 2010
densidade = read.csv2(file = "densidadebr.csv", sep = ";", dec = ",", header = TRUE)
names(densidade)
head(densidade)

# SEPARANDO OS DADOS DE INTERESSE

# Filtrando pelas cidades de Minas Gerais
df = dados %>% filter(state == "MG")
rm(dados) # Removendo da memoria

# Unindo dados dos municipios
municipios = merge(municipios, municipios2, by.x = 'codigo_ibge', by.y = 'IBGE7' )
municipios = municipios %>% select(codigo_ibge, nome, latitude, longitude, Popula..o.2010, Porte)
head(municipios)

# Inserido dados demograficos
municipios = merge(municipios, densidade, by.x = 'codigo_ibge', by.y = 'Cod' )
head(municipios)

# Filtrando municipios somente com as cidades do Norte de Minas
cidades = merge(municipios, municipios_norte, by.x = 'nome', by.y = 'cidades_norte', all.y = TRUE)
cidades = cidades %>% select(codigo_ibge, nome, latitude, longitude, Popula..o.2010, Porte, Densidade)
names(cidades)

# Unindo os dados das cidades com dados de COVID
df2 = merge(df, cidades, by.x = 'name', by.y = 'nome', all.y = TRUE)
df2 = df2 %>% select(codigo_ibge, name, codigo_ibge, latitude, longitude, Popula..o.2010, Porte, Densidade, date, cases, deaths)
names(df2)

# Armazenando em novo arquivo
write.csv2(df2, file="covid_norte_minas.csv", row.names = FALSE)

# Limpando da memoria
rm(cidades, df, df2, municipios, municipios2, municipios_norte, densidade)

# ANALISANDO BASE DE DADOS GERADA
dados = read.csv2(file = "covid_norte_minas.csv", sep = ";", dec = ",", header = TRUE)
dados %>% distinct(name)
head(dados)

# ADAPTANDO DATA (dia/mes/ano)

dados[,'dia'] = day(as.Date(dados[,'date'], format = "%Y-%m-%d"))
dados[,'mes'] = month(as.Date(dados[,'date'], format = "%Y-%m-%d"))
dados[,'ano'] = year(as.Date(dados[,'date'], format = "%Y-%m-%d"))

dados = dados %>% select(codigo_ibge, name, codigo_ibge, latitude, longitude, Popula..o.2010, Porte, Densidade, dia, mes, ano, cases, deaths)
dados = na.omit(dados)

head(dados)

# DIVIDINDO DADOS POR ANO

df2020 = dados %>% filter(ano == '2020')
df2021 = dados %>% filter(ano == '2021')

# PROCESSANDO DADOS - 2020

# Segregacao em dataframes mensais em uma lista de dataframes
data = df2021

dados <- list()
for(i in 1:12){
  b = data %>% filter(mes == i)
  b = arrange(b, name, dia, mes, ano)
  dados[i] <- list(b)
}

for(i in 1:5){
  t1 = dados[[i]]
  t2 = t1 %>% group_by(name) %>% summarise( 
    porte = first(Porte),
    latitude = max(latitude),
    longitude = max(longitude),
    populacao =  max(Popula..o.2010),
    densidade = max(Densidade),
    totcasos = max(cases),
    totobitos = max(deaths)
  )
  t2 = t2 %>% mutate(
    indobito = (totobitos / populacao)*1000,
    indcasos = (totcasos / populacao)*1000
  ) %>% select( name, porte, latitude, longitude, populacao, densidade,
                totcasos, indcasos, totobitos, indobito)
  dados[[i]] = t2
  #write.csv2(t2, file = paste("casos_covid_mes_", i,".csv", sep=""), row.names = FALSE)
}

# Analisando os dados
head(dados[[5]])

df = dados[[5]]

# Municipios com maior população
df %>% 
  top_n(15, populacao) %>% 
  mutate(name = forcats::fct_reorder(name, populacao)) %>%
  ggplot() +
  geom_col(aes(x = name, y = populacao, fill = populacao), show.legend = FALSE) +
  geom_label(aes(x = name, y = populacao/2, label = populacao)) +
  coord_flip()

# Municipios com maior densidade
df %>% 
  top_n(15, densidade) %>% 
  mutate(name = forcats::fct_reorder(name, densidade)) %>%
  ggplot() +
  geom_col(aes(x = name, y = densidade, fill = densidade), show.legend = FALSE) +
  geom_label(aes(x = name, y = densidade/2, label = densidade)) +
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


# Boxplot relacionando o porte da cidade ao indice de obitos
p = df %>% ggplot() + geom_boxplot(aes(x=porte, y=indobito, fill = porte) )
p + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9","#5dfa14"))


summary(df$indcasos)
summary(df$indobito)

t = df %>% filter(name != c("Montes Claros","Pirapora"))

# Teste de Normalidade e Correlação
#(Conclusão: Em ambas variáveis existe uma diferencia significativa da média)
ggplot(df) +
  geom_point(mapping = aes(x = indcasos, y = indobito))
cor.test(df$indcasos, df$indobito)

ggplot(df) +
  geom_point(mapping = aes(x = densidade, y = totobitos))
cor.test(df$densidade, df$totobitos)

ggplot(df) +
  geom_point(mapping = aes(x = densidade, y = totcasos))
cor.test(df$densidade, df$indobito)

# Correlação das variáveis
correlacao = stats::cor(x = df[,5:10])
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

# Convertendo coordenadas para UTM

library(sp)
library(rgdal)

X = df$longitude
Y = df$latitude
Z = df$indcasos
zone = 23

xy <- data.frame(ID = 1:length(X), X = X, Y = Y)
coordinates(xy) <- c("X", "Y")
proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
res <- spTransform(xy, CRS(paste("+proj=utm +zone=23 ellps=WGS84",sep='')))
coord = as.data.frame(res)
coord[,"Z"] = Z
coord[,"cidade"] = df$name
coord  = coord %>% select(X,Y,Z,cidade)
head(coord)

# Definindo semivariograma

library(geoR)

geodata = as.geodata(coord, coords.col=1:2, data.col=3)
summary(geodata)
labelx="Coord X (m)"
labely="Coord Y (m)"
points(geodata, xlab=labelx, ylab=labely, pt.divide="quartiles")

# Removendo cidades distantes
t = coord %>% filter(Y < -800000)

# Kriando objeto GeoR
geodata = as.geodata(t, coords.col=1:2, data.col=3)
points(geodata, xlab=labelx, ylab=labely, pt.divide="quartiles")

# Definindo função de variograma
Geo = geodata
res = summary(Geo)
dist_max = res$distances.summary[[2]]

par(mfrow=c(3,2))
for(i in 1:6){
  variograma = variog(Geo, max.dist = (dist_max/i), messages = F )
  plot(variograma, main=paste("d/",i))
}

med = mean(Geo$data)
var = var(Geo$data)

print(paste("Med = ", med, " - Var = ", var, sep = ""))

par(mfrow=c(1,1))
variograma <- variog(Geo, max.dist=(dist_max/2), direction="omnidirectional")
plot(variograma, ylim=c(0, 2*(var)),xlab="Distância",ylab="Variancia", main="Variograma")
abline(var(Geo$data),0, col="gray60", lty=2, lwd=2)

# Treinando o modelo
esferico = variofit(variograma, cov.model='sph', max.dist= variograma$max.dist, messages=F)
exponencial = variofit(variograma, cov.model='exponential', max.dist= variograma$max.dist, messages=F)
gaussiano = variofit(variograma, cov.model='gaussian', max.dist = variograma$max.dist, messages=F)

par(mfrow=c(1,1))
plot(variograma, ylim=c(0, 2*(var)),xlab="Distância",ylab="Variancia", main="Variograma")
sentimento = eyefit(variograma)
stp = unlist(sentimento)
sigmasq = as.numeric(stp[2]); phi = as.numeric(stp[3]); tausq = as.numeric(stp[4])

# Exibindo variogramas
par(mfrow=c(2,2),mar=c(5,5,2,2))
plot(variograma, las=1, type='p',pch=19, cex=1.4, col='black' , xlab='',ylab='', xaxs='i', yaxs='i', cex.lab=1.4, cex.axis=1.2, main='Spherical')
lines.variomodel(esferico, col='red' , lwd=2, lty=1)
plot(variograma, las=1, type='p',pch=19, cex=1.4, col='black' , xlab='',ylab='', xaxs='i', yaxs='i', cex.lab=1.4, cex.axis=1.2, main='Exponential')
lines.variomodel(exponencial, col='red' , lwd=2, lty=1)
plot(variograma, las=1, type='p',pch=19, cex=1.4, col='black' , xlab='',ylab='', xaxs='i', yaxs='i', cex.lab=1.4, cex.axis=1.2, main='Gaussian')
lines.variomodel(gaussiano, col='red' , lwd=2, lty=1)
plot(variograma, las=1, type='p',pch=19, cex=1.4, col='black' , xlab='',ylab='', xaxs='i', yaxs='i', cex.lab=1.4, cex.axis=1.2, main='Sentiment')
lines(sentimento, col='red' , lwd=2, lty=1)

# Validação
cv.esf=xvalid(Geo, model=esferico, messages = F); zsco.esf=cv.esf$std.error
cv.exp=xvalid(Geo, model=exponencial, messages = F); zsco.exp=cv.exp$std.error
cv.gau=xvalid(Geo, model=gaussiano, messages = F); zsco.gau=cv.gau$std.error
cv.sent=xvalid(Geo, model=sentimento, messages = F); zsco.sent=cv.sent$std.error

jkmed.esf=round(mean(zsco.esf),5); jkvar.esf=round(var(zsco.esf),5)
jkmed.exp=round(mean(zsco.exp),5); jkvar.exp=round(var(zsco.exp),5)
jkmed.gau=round(mean(zsco.gau),5); jkvar.gau=round(var(zsco.gau),5)
jkmed.sent=round(mean(zsco.sent),5); jkvar.sent=round(var(zsco.sent),5)

r2k.esf = cor(cv.esf$predicted,cv.esf$data)^2
r2k.exp = cor(cv.exp$predicted,cv.exp$data)^2
r2k.gau = cor(cv.gau$predicted,cv.gau$data)^2
r2k.sent = cor(cv.sent$predicted,cv.sent$data)^2

modelos=c('Eph', 'Exp','Gau','Sent')
m.jk=rbind(jkmed.esf,jkmed.exp, jkmed.gau, jkmed.sent)
v.jk=rbind(jkvar.esf, jkvar.exp, jkvar.gau, jkvar.sent)
r2k = rbind(r2k.esf, r2k.exp, r2k.gau, r2k.sent)

resumo = data.frame(row.names=modelos, m.jk, v.jk, r2k)
print(resumo)

# Variograma escolhido
smfit = esferico
par(mfrow=c(1,1))
plot(variograma, las=1, type='p', pch=19, cex=1.4, col='black' , cex.lab=1.4, cex.axis=1.2, main="Variograma")
lines(smfit, col='red', lwd=2, lty=1)

if(length(smfit) == 1){ # Ajuste do sentimento
  sigmasq = smfit[[1]]$cov.pars[1]
  phi = smfit[[1]]$cov.pars[2]
  tausq = smfit[[1]]$nugget
  model = smfit[[1]]$cov.model
}else{
  sigmasq = smfit$cov.pars[1]
  phi = smfit$cov.pars[2]
  tausq = smfit$nugget
  model = smfit$cov.model
}

### AJUSTE POR M?NIMOS QUADRADOS: ols e wls
ols <- variofit(variograma, ini.cov.pars =c(sigmasq, phi), fix.nugget=T,
                nugget =tausq, cov.model = model, weights ="equal", messages = F)
wls <- variofit(variograma, ini.cov.pars = c(sigmasq, phi), fix.nugget=T,
                nugget = tausq, cov.model = model, weights = "npairs", messages = F)

par(mfrow=c(2,1))
plot(variograma, xlab="Distances (m)", ylab="Semivariances", main="OLS")
lines(ols, max.dist= variograma$max.dist, col="brown", lty=4, lwd=2)
abline(var(Geo$data),0, col="gray60", lty=2, lwd=2)

plot(variograma, xlab="Distances (m)", ylab="Semivariances", main="WLS")
lines(wls, max.dist= variograma$max.dist, col="brown", lty=5, lwd=2)
abline(var(Geo$data),0, col="gray60", lty=2, lwd=1)

model = ols # Modelo escolhido

# Validação
autoval = xvalid(Geo, model = model)
rescross = summary(autoval)
print(rescross)
par(mfcol=c(5,2), mar=c(2.3,2.3,.5,.5), mgp=c(1.3, .6, 0))
plot(autoval)

reg.val = lm(autoval$data ~ autoval$predicted)

par(mfrow=c(1,1))
plot(autoval$predicted, autoval$data, xlab="Predicted",
     ylab="Observed", main="Self-Validation Regression")
abline(reg.val)
abline(coef=c(0,1), col="gray60", lty=3, lwd=3)

# Kriando mapa de krigagem

border_kriging = function(Geo, model, width, borderfile=""){
  ## Definir a borda da krigagem
  if(borderfile != ""){
    borda <- read.csv2(borderfile, sep = ";", dec = ",")
    Geo$borders <- with(borda, cbind(x,y))
    par(mfrow=c(1,1))
    points(Geo, pt.div="quartile", xlab="Longitudes", ylab="Latitudes")
  }
  else{
    message("Manually set the border...")
    par(mfrow=c(1,1))
    points(Geo,xlab="Coordinate X (m)", ylab="Coordinate Y (m)", pt.divide="quartiles")
    borda <- as.data.frame(locator(type="p"))
    polygon(borda)
    Geo$borders <- with(borda, cbind(x,y))
    
    par(mfrow=c(1,1))
    points(Geo, pt.div="quartile", xlab="Longitudes", ylab="Latitudes")
    
    message("Do you want to save the file with the border points?")
    message(paste("1 - YES", "2 - NO", sep = "\n"))
    choice = as.numeric(readline("Choose: "))
    if(choice == 1){
      write.csv2(file = "borda.txt",x = borda, row.names = FALSE)
      message("Border file created in the current directory.")
    }
  }
  
  win = summary(model)
  range = win$practicalRange
  ref = summary(Geo)
  
  menor.x = ref$coords.summary[1]
  buf.menor.x = menor.x - range
  maior.x = ref$coords.summary[2]
  buf.maior.x = maior.x + range
  
  menor.y=ref$coords.summary[3]
  buf.menor.y=(menor.y - 3*range)
  maior.y=ref$coords.summary[4]
  buf.maior.y=(maior.y + 3*range)
  
  grid = expand.grid(seq(buf.menor.x,buf.maior.x, l=width),
                     seq(buf.menor.y,buf.maior.y, l=width))
  
  # FAZENDO UMA NOVA KRIGAGEM - DENTRO DA BORDA
  ko <- krige.conv(Geo, locations = grid, borders = Geo$borders,
                   krige=krige.control(type.krige = "ok", beta=ref$data.summary[[4]],
                                       cov.pars=model$cov.pars))
  
  par(mfcol=c(1,1))
  contour(ko,loc=grid,
          col=terrain.colors(11), font.lab=6, xlab="Coordinate X (m)",
          ylab="Coordinate Y (m)")
  
  points(Geo,xlab="Coordinate X (m)",ylab="Coordinate Y (m)",
         pt.divide="data.proportional",add=T)
  
  par(mfcol=c(1,1))
  
  image(ko,loc=grid,
        col=terrain.colors(11), font.lab=6,
        xlab="Coordinate X (m)",
        ylab="Coordinate Y (m)")
  points(Geo, xlab="Coordinate X (m)",ylab="Coordinate Y (m)",
         pt.divide="quartiles",add=T)
  
  
  
  return(list(ko, grid))
}


krige = border_kriging(Geo, model, width = 200, "borda.txt")

# KMEANS

# Número ótimo de clusters
library(factoextra)
df3=df %>% select(populacao, densidade, totcasos, indcasos, totobitos, indobito)
df4=scale(df3)

fviz_nbclust(df4, kmeans, method = "wss")+
  geom_vline(xintercept = 4, linetype = 2)

set.seed(123)
km.res=kmeans(df4, 4, nstart=25)
print(km.res)

# Adicionando no a classificação dos clusters
df5 = cbind(df3, cluster=km.res$cluster)
head(df5)

fviz_cluster(km.res, data=df5,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type="euclid",
             star.plot=TRUE,
             repel=TRUE,
             ggtheme=theme_minimal()
)
km.res$size # Tamanho dos Clusters

# Observando quantidade de cidades por porte
df %>% group_by(porte) %>% count(porte)

df6 = cbind(df, cluster=km.res$cluster)
library("gridExtra")
a=ggplot(data = df6) +
  aes(x = populacao, y = totcasos, color = porte) +
  geom_point() +
  theme_minimal()

b=ggplot(data = df6) +
  aes(x = populacao, y = totcasos, color = cluster) +
  geom_point() +
  theme_minimal()

grid.arrange(a, b, ncol=2)

# Matriz de Confusão
table(df6$porte, df6$cluster)

df6 %>% group_by(cluster) %>% count()

