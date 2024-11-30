# Librerías 
install.packages("readr")
install.packages('derivmkts')

library(readr) #leer archivos csv
library(car) #graficar
library(derivmkts)
library(dplyr)


#Lectura de bases de datos usadas 
database <-data.frame(read.csv("C:\\Users\\citla\\Downloads\\AMZN.csv")) #Amazon 
names(database) <- c("Date", "Price","Rendimiento")

database2 <-data.frame(read.csv("C:\\Users\\citla\\Downloads\\FEMSA.csv")) #FEMSA
names(database2) <- c("Día", "Precios","Rendimiento")

database3 <-data.frame(read.csv("C:\\Users\\citla\\Downloads\\BTC.csv")) #Bitcoin 
names(database3) <- c("Fecha", "P","Rendimiento")


#Funciones 

# Modelo de Black and Scholes

BS<-function(S,K,sigma,r,t,op){
  t = t/360 #El tiempo se encuentra expresa en días 
  d1 = (log(S/K)+(r+sigma^2/2)*t)/(sigma*sqrt(t))
  d2 = d1-sigma*sqrt(t)
  
  if (op=="Call"){
    n1 = pnorm(d1)
    n2 = pnorm(d2)
    prima = S*n1 - K*exp(-r*t)*n2
  }
  if(op=="Put"){
    n1 = pnorm(-d1)
    n2 = pnorm(-d2)
    prima = K*exp(-r*t)*n2 - S*n1
  }
  return(prima)
}

# Modelo BS con Dividendos 

BS_div<-function(S,K,sigma,r,t,op,q){
  t = t/360 #El tiempo se encuentra expresa en días 
  d1 = (log(S/K)+(r-q+(sigma^2/2))*t)/(sigma*sqrt(t))
  d2 = d1-sigma*sqrt(t)
  
  if (op=="Call"){
    n1 = pnorm(d1)
    n2 = pnorm(d2)
    prima = S*exp(-q*t)*n1 - K*exp(-r*t)*n2
  }
  if(op=="Put"){
    n1 = pnorm(-d1)
    n2 = pnorm(-d2)
    prima = K*exp(-r*t)*n2 - S*exp(-q*t)*n1
  }
  return(prima)
}

#Valuación de de una opción call y una opción put para la acción de  BITCOIN.

  #Datos
  S1 =  database3[nrow(database3),"P"]  #31,726.39 
  K1 = 35000 #USD
  sigma1 = sd(database3$Rendimiento)*sqrt(252)# Volatilidad anual 
  r1=0.065 # Tasa Libre de Riesgo anual
  tt1=180 #Tiempo expresado en años

  #Valuación
  c1=BS(S1,K1,sigma1,r1,tt1,"Call")
  p1=BS(S1,K1,sigma1,r1,tt1,"Put")

  sprintf(" Precio de la opción de call de BITCOIN: %f", c1)
  sprintf(" Precio de la opción de put de BITCOIN: %f", p1)

#Valuación de de una opción call y una opción put para la acción de AMAZON Inc.

  #Datos
  S =  database[nrow(database),"Price"] #27 de mayo feriado en USA 115.15 
  K = 150 #USD
  sigma = sd(c(database$Rendimiento))*sqrt(252)# Volatilidad anual  
  r=0.065 # Tasa Libre de Riesgo anual
  #d=0.03 # Tasa de dividendos anual
  tt=180 #Tiempo expresado en años

  #Valuación
  c=BS(S,K,sigma,r,tt,"Call")
  p=BS(S,K,sigma,r,tt,"Put")
  
  sprintf(" Precio de la opción de call de AMAZON: %f", c)
  sprintf(" Precio de la opción de put de AMAZON: %f", p)


#Valuación de de una opción call y una opción put para la acción de (FEMSAUBD.MX).

  #tasa de div 
  
  P2021 = median(c(database2[896:1148,"Precios"])) #Precio promedio de la acción en el año 2021
  P2022 = median(c(database2[1149:1250,"Precios"])) #Precio promedio de la acción en el año 2022
  
  d = P2022/P2021-1 #tasa de rendimiento anual : 155.3789595/158.638931 - 1
    
  #Datos
  Spot =  database2[nrow(database2),"Precios"] # 149.79 
  Strike = 160 #MXN
  V = sd(database2$Rendimiento)*sqrt(252) # Volatilidad anual
  rate=0.065 # Tasa Libre de Riesgo anual
  time=180 #Tiempo expresado en años
  
  #Valuación
  c2=BS_div(Spot,Strike,V,rate,time,"Call",d)
  p2=BS_div(Spot,Strike,V,rate,time,"Put",d)
  
  sprintf(" Precio de la opción de call de FEMSA: %f", c2)
  sprintf(" Precio de la opción de put de FEMSA: %f", p2)
  
  
 
  