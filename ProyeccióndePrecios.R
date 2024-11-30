### Instalar Librerias
install.packages("tseries", dependencies = TRUE)
install.packages("astsa")
install.packages("forecast")
install.packages("tidyverse")
install.packages("readxl")
install.packages("nortest")
install.packages("car")
install.packages("ggfortify")
install.packages("quadprog", dependencies = TRUE)
install.packages("installr")
install.packages("aTSA")
library(installr)
updateR()

### Cargar Librerias
library(tseries)
library(astsa)
library(forecast)
library(tidyverse)


library(readxl)
library(nortest)
library(car)
library(ggfortify)
library(aTSA)


datos<- read_excel("/home/citlaloc/Downloads/Datos.xlsx", sheet = "Datos")

#Convertir datos a series de tiempo 

serie1 = ts(datos$MM_USD, start = c(2005,1), frequency = 12)
serie1
autoplot(serie1)

#Paso 2. Comprobar que tenemos un proceso estacionario.
#Comprobar que se comporta como un ruido blanco. 

#Podemos buscar convertirla a una serie viable (con ruido blanco)
serie1Log = log(serie1)
autoplot(serie1Log) #No parece arreglarse el tema de la estacionalidad. 
stationary.test(serie1Log) #Prueba no paramétriva Dickey-Fuller 

#pvalue > mayor a la significancia 0.5

#Diferencias 
serie1Dif = diff(serie1, differences = 2)
autoplot(serie1Dif)
stationary.test(serie1Dif) # p-value 0.01

#Paso 3 Determinar medias moviles y autoregresores 

par(mfrow = c(2,1)) 
acf(serie1Dif, lag.max=500)# medias moviles
pacf(serie1Dif, lag.max = 500) #autoregresores

#Paso 4. Propuesta del modelo 
mod1 = arima(serie1, order = c(1,1,1)) #ARIMA(p,d,f) -> AR(p), I(d) , MA(q)
mod1

mod2 = arima(serie1, order = c(2,1,1))
mod2

mod3 = arima(serie1, order = c(3,1,1))
mod3

#Paso 5 Selección del modelo 

AIC(mod1, mod2, mod3) #con le menor aic

#Paso 6 Validación al modelo 

Box.test(mod2$residuals, type = "Ljung-Box") #P.values mayores al nivel de significancia 
#para quedarnos con la hipótesis de que los residuakes son ind. dis p-value > alpha 
tsdiag(mod2)


#Paso 7. Pronostics 

pronostico2 = forecast::forecast(mod2,h=9)
autoplot(pronostico2)
par(mfrow = c(1,1))


#Modelo en automático 
mod4 = auto.arima(serie1) #r genera el modelo de manera automatic
mod4

pronostico4 = forecast::forecast(mod4,h=9)
pronostico4
plot(pronostico4)
