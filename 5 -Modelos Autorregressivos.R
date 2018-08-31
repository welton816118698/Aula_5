#Aula 5 - Modelos Autorregressivos

library("pwt8")
library("pwt8")
data('pwt8.0')
View(pwt8.0)
library(urca) 
getwd()

setwd("c:/econometria")

 


library(readxl)      #Carregando o pacote Readxl, que efetua leitura dos arquivos do Excel                               
library(urca)        #Carregando pacote URCA para teste de estacionariedade
IPCA <- read_excel("C:/Econometria/IPCA.xls", col_types = c("date","numeric"))   #Carregando o arquivo
Inflacao <- ts(IPCA$IPCA,start = 2008-01, frequency = 12)    #Definindo como Série Temporal
Inflacao <- Inflacao[,-1]
View(Inflacao)

              #Teste de Estacionariedade
TesteDF <- summary(ur.df(Inflacao, type="none", lags=0))
TesteDF

           #Gráficos de Autocorrelação
acf(IPCA$IPCA, main="Inflacao Mensal")   #FAC - Função de Autocorrelação - Esse item está mostrando sazionalidade, a linha azul significa os 5% (pct) de significância
pacf(IPCA$IPCA, main="Inflacao Mensal")  #FACP-Funçao de Autocorrelação Parcial - 

        #Estimando Modelo Autoregressivo

AR1 <- arima(Inflacao,order = c(1,0,0))  #Modelo Autorregressivo de ordem 1
AR1
AR2 <- arima(Inflacao, order=c(2,0,0))   #Modelo Autorregressivo de ordem 2
AR2

                               


                                  #Extras


#plot(decompose(Inflacao)) #Separa Sazonalidade, tendência e efeito aleatório dos dados.

            #Previsão
#install.packages("Forecast")                 #Instala Pacote Forecast
#install.packages("ggplot2")                  #Instala Pacote GGplot2
#library(forecast)                            #Carrega o Pacote Forecast
#library(ggplot2)
#previsao1 <- forecast(AR1, 4)
#previsao1

#previsao2 <- forecast(AR2, 4)
#previsao2

#GráficoS de Previsão
#autoplot(Inflacao)                         #Gráfico dos Dados 
#lines(previsao$pred,col="blue")            #Inseri as previsões em azul
#plot(forecast(AR1, 3), main ="Previsao da Inflacao para 2018 - AR1", xlim=c(2017,2018))  #Gráfico com as previsões com um AR1
#plot(forecast(AR2, 3), main ="Previsao da Inflacao para 2018 -AR2", xlim=c(2017,2018))   #Gráfico com as previsões com um AR2





