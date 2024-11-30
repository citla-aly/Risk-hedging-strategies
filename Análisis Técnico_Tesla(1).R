

### Análisis Técnico

# Intalar Librerias }

# Cargar las librerias
library('TTR')
library("tidyquant")
library("quantmod") # Libraria de Finanzas cuantitativas
library("shiny")

################################ Obtener datos

# Trending Tickers -- https://finance.yahoo.com/lookup/

cartera = c("TSLA","NVDA","NFLX","AMZN") #Hay que agregar todas en uno sola cartera
getSymbols(cartera,src = "yahoo",from="2017-03-30", to= "2022-03-30") #Hace 5 años #en quantmod tienen libreiras de datos 

#head(`TSLA`,n=3) #muestra los primeros 3 registros de la info 

################################ Análisis Gráfico 

##TESLA
chartSeries('TSLA',type = "line",subset = "2017-03::2022-03",
            theme = chartTheme('black'))
chartSeries('TSLA',type = "bar",subset = "2017-03::2022-03",
            theme = chartTheme('black'))
chartSeries('TSLA',type = "candlesticks",subset = "2017-03::2022-03",
            theme = chartTheme('black'))

################################  Indicadores técnicos 

##TESLA

chartSeries('TSLA',subset = "2017-03::2022-03",
            theme = chartTheme('white'))
#Medias móviles de 25 días -> ultimos 25 días  
addSMA(n=10,on=1,col = "black")
addSMA(n=50,on=1,col = "red")
#tendencias de cuando te comviene comprar/vender 
#Cuando el PM de corto plazo > lp entonces compramos 
#Cuando lp > cp entonces es señal de venta 


#PROMEDIO MOVIL EXPONENCIAL 
chartSeries('TSLA',subset = "2017-03::2022-03",
            theme = chartTheme('white'))
addEMA(n=25,on=1,col = "blue") 
addEMA(n=50,on=1,col = "red")
#Donde hay  cambio de tendecias de los promedios hay oportunidad de mercado y ventana de venta --> cruce 

#Bandas de Bollinger 
chartSeries('TSLA',subset = "2020-03::2022-03",
            theme = chartTheme('white'))
addBBands(n=25,sd=2)
#casos atipicos cuando se sale de la banda --> que tanto se mueve el precio de la acción
#se toca la banda superior --> venta 
#se toca la banda inferior señal de compra


##### Vólumenes de compra/venta 
chartSeries('TSLA',subset = "2017-03::2022-03",
            theme = chartTheme('white'))
addMomentum(n=1) #ABSOLUTA = que tan rapido cambia el precio de la acción diferencial de precios
addROC(n=1) # RELATIVA = de manera porcentual que tan rapdio cambian los precios 
addRSI(n=14,maType = "EMA") #Fuerza relativa --> volumen de compra o venta


# Cuando el precio toca la banda inferior es una Señal de compra.
# 
# Si el precio toca la banda superior es una Señal de venta.
#observamos que para marzo de 2020 hubo una sobre compra de acciones
# para julio de 2020 hubo una sobreventa de acciones, no hubo mucha 
# volatilidad en los precios

