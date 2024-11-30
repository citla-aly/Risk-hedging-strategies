################################ Proyección de Precios ################################ 

### Cargar Librerias
library(quantmod)
library(dplyr)
library(tidyverse)
library(tseries)
library(rugarch)
library(xts)
library(PerformanceAnalytics)
library(astsa)
library(forecast)
library(nortest)
library(car)
library(readxl)
library(xlsx)
library(TTR)
library(tidyquant)
library(fpp2)
library(ggfortify)
library(aTSA)

################################ Cargar los datos 

datos<- read_excel("C:\\Users\\citla\\Downloads\\Data (2).xlsx", sheet = "Sugar")
oro<- read_excel("C:\\Users\\citla\\Downloads\\Data (2).xlsx", sheet = "Gold", range= "D5:E271")
trigo<- read_excel("C:\\Users\\citla\\Downloads\\Data (2).xlsx", sheet = "Wheat", range= "D5:E271")
datosCoffe <- read_excel("C:\\Users\\citla\\Downloads\\Data (2).xlsx", sheet = "Coffee")


################################ Convertir datos a series de tiempo 

###ORO
serie1=ts(oro$`US dollar`[205:266],start = c(2017,1),frequency=12)
serie1 #precios promedio por mes
plot(oro[,1:2],type="l", main ="Precio Ajustado del Oro")

###AZÚCAR
SSugar = ts(datos$Prices, start = c(2017,4), frequency = 12)
SSugar
autoplot(SSugar)

###TRIGO 
precio=ts(trigo$Precio[205:266],start = c(2017,1),frequency=12)
plot(trigo[,1:2],type="l", main ="Precio Ajustado del Trigo")

###CAFÉ
SCoffee = ts(datosCoffe$Prices, start = c(2017,4), frequency = 12)
SCoffee

################################ Pronostics with ARIMA Model

###ORO
modelo=auto.arima(serie1)
modelo #se considera un modelo ARIMA(0,1,1) #Pronosticamos 10 meses para calcular hasta diciembre
pronostico = forecast::forecast(modelo,h=10)
pronostico
plot(pronostico)

###AZÚCAR Modelo en automático 
mod4 = auto.arima(SSugar) #r genera el modelo de manera automatic
mod4

pronostico4 = forecast::forecast(mod4,h=9)
pronostico4
plot(pronostico4)

###TRIGO 
modelo1 = auto.arima(precio)
pronostico1 = forecast::forecast(modelo1,h=10)
pronostico1
plot(pronostico1)

###CAFÉ
#Modelo en automático 
mod2 = auto.arima(SCoffee) #r genera el modelo de manera automatic
mod2

pronostico2 = forecast::forecast(mod2,h=9)
pronostico2

plot(pronostico2)

############################ Algodon ############################
cotton <- read_excel("C:/Users/Nicole SÃ¡nchez/Downloads/octavo/ECR/Datos_c.xlsx",sheet = "Cotton")

serie1=ts(cotton$value,start = c(2005,1),frequency=12)
serie1
autoplot(serie1)

###Estacionariedad
# Diferencias
serie1Dif =diff(serie1,differences = 1)
autoplot(serie1Dif)
adf.test(serie1Dif,alternative = "stationary")

###Medias Moviles y Autoregresores
par(mfrow=c(2,1))
acf(serie1Dif,lag.max = 500)
pacf(serie1Dif,lag.max = 500)

###Propuesta del Modelo

mod1=arima(serie1,order = c(1,1,1))
mod1
mod2=arima(serie1,order = c(2,1,1))
mod2

### Seleccion del Modelo
AIC(mod1,mod2)

###Validacion del modelo 
Box.test(mod1$residuals,type = "Ljung-Box") # nos interesan p-value > alpha
tsdiag(mod1)

###ronosticos
pronostico1 = forecast::forecast(mod1,h=10)
pronostico1
par(mfrow=c(1,1))
autoplot(pronostico1)
plot(pronostico1)

### Modelos en automatico
mod4=auto.arima(serie1)
mod4
pronostico4 = forecast::forecast(mod4,h=10)
pronostico4
plot(pronostico4)
############################ Maiz ############################
corn <- read_excel("C:/Users/Nicole SÃ¡nchez/Downloads/octavo/ECR/Datos_c.xlsx",sheet = "Corn")

serie1=ts(corn$value,start = c(2005,1),frequency=12)
serie1
autoplot(serie1)

###Estacionariedad
# Diferencias
serie1Dif =diff(serie1,differences = 1)
autoplot(serie1Dif)
adf.test(serie1Dif,alternative = "stationary")

###Medias Moviles y Autoregresores
par(mfrow=c(2,1))
acf(serie1Dif,lag.max = 500)
pacf(serie1Dif,lag.max = 500)

###Propuesta del Modelo

mod1=arima(serie1,order = c(1,1,1))
mod1
mod2=arima(serie1,order = c(0,1,1))
mod2

### Seleccion del Modelo
AIC(mod1,mod2)

###Validacion del modelo 
Box.test(mod1$residuals,type = "Ljung-Box") # nos interesan p-value > alpha
tsdiag(mod1)

###ronosticos
pronostico1 = forecast::forecast(mod1,h=10)
pronostico1
par(mfrow=c(1,1))
autoplot(pronostico1)
plot(pronostico1)

### Modelos en automatico
mod4=auto.arima(serie1)
mod4
pronostico4 = forecast::forecast(mod4,h=10)
pronostico4
plot(pronostico4)

