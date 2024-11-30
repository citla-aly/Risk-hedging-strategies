################################ Proyección del precio del azúcar 
install.packages("forecast")
install.packages("fpp2")
### Cargar Librerias
library(tseries)
library(astsa)
library(fpp2)
library(tidyverse)


library(readxl)
library(nortest)
library(car)
library(ggfortify)
library(aTSA)

### Cargar los datos 

datos<- read_excel("/home/citlaloc/Downloads/Data.xlsx", sheet = "Sugar")


#Convertir datos a series de tiempo 

SSugar = ts(datos$Prices, start = c(2017,4), frequency = 12)
SSugar
autoplot(SSugar)

#Paso 2. Comprobar que tenemos un proceso estacionario.Comprobar que se comporta como un ruido blanco. 

#Podemos buscar convertirla a una serie viable (con ruido blanco)
serie1Log = log(SSugar)
autoplot(serie1Log)  
stationary.test(serie1Log) #Prueba no paramétriva Dickey-Fuller --> Misma funcionalidad que ADF.test
#pvalue = 0.01 <  a la significancia 0.5

#Paso 3. Propuesta del modelo 
mod1 = arima(SSugar, order = c(1,1,1)) #ARIMA(p,d,f) -> AR(p), I(d) , MA(q)
mod1

mod2 = arima(SSugar, order = c(2,1,1))
mod2

mod3 = arima(SSugar, order = c(3,1,1))
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
mod4 = auto.arima(SSugar) #r genera el modelo de manera automatic
mod4

pronostico4 = forecast::forecast(mod4,h=9)
pronostico4
plot(pronostico4)
