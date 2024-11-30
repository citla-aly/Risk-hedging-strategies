install.packages("derivmkts")
install.packages("tmvnsim")
library(shiny)
library(derivmkts)
library(shinydashboard)
#Primero corra esto
s=seq(0.5,100,0.5)
#Despues a Shiny
ui <- dashboardPage(skin = "purple",
                    dashboardHeader(title = "Opciones"),
                    dashboardSidebar(sidebarMenu(
                      menuItem("Valuación de Opciones ", tabName = "dashboard", icon = icon("dashboard")),
                      menuItem("Letras Griegas", tabName = "widgets", icon = icon("th"))
                    )),
                    dashboardBody(
                      tabItems(
                        # First tab content
                        tabItem(tabName = "dashboard",
                                fluidRow(
                                  box(plotOutput("g1", height = 350),
                                      fluidRow( box(title="Call",verbatimTextOutput("t1")),
                                                box(title="Put",verbatimTextOutput("t2")))),
                                  box(title="Nota",solidHeader  = TRUE, background = "maroon",
                                  textOutput("nota")),
                                  box(
                                   
                                    title = "Variables",
                                    selectInput(inputId = 'M',label = "Modelo de Valuación",
                                                choices = c("BS sin Dividendos","BS con Dividendos",
                                                            "BS Divisas", "BS Futuros")),
                                    sliderInput(inputId = 'S',label = "Spot",value = 20,
                                                min = 1,max = 100,step = 1),
                                    sliderInput(inputId = 'K',label = "Strike",value = 30,
                                                min = 1,max = 100,step = 1),
                                    sliderInput(inputId = 'sigma',label = "Volatilidad anual",value = 0.20,
                                                min = 0.01,max = 2,step = 0.01),
                                    sliderInput(inputId = 'r',label = "Tasa libre de riesgo anual",value = 0.07,
                                                min = 0.01,max = 0.20,step = 0.01),
                                    sliderInput(inputId = 'q',label = "Tasa de Dividendo",value = 0.03,
                                                min = 0.01,max = 0.30,step = 0.01),
                                    sliderInput(inputId = 'tt',label = "Tiempo expresado en días",value = 180,
                                                min = 1,max = 360,step = 1),
                                    
                                  )
                                )
                        ),
                        
                        # Second tab content
                        tabItem(tabName = "widgets",
                                
                                fluidRow(
                                  box(plotOutput("g3", height = 350),
                                      fluidRow( box(title="Griegas",tableOutput("nombres")),
                                                box(title="Valor",tableOutput("t3"))),
                                      box(title="Volatilidad implicita",tableOutput("t5"))
                                      
                                  ),
                                  
                                  box(
                                    title = "Variables",
                                    selectInput(inputId = 'M1',label = "Modelo de Valuación",
                                                choices = c("BS sin Dividendos","BS con Dividendos",
                                                            "BS Divisas", "BS Futuros")),
                                    selectInput(inputId = 'Op1',label = "Tipo de Opción",
                                                choices = c("Call","Put")),
                                    sliderInput(inputId = 'S1',label = "Spot",value = 20,
                                                min = 1,max = 100,step = 1),
                                    sliderInput(inputId = 'K1',label = "Strike",value = 30,
                                                min = 1,max = 100,step = 1),
                                    sliderInput(inputId = 'sigma1',label = "Volatilidad anual",value = 0.20,
                                                min = 0.01,max = 2,step = 0.01),
                                    sliderInput(inputId = 'r1',label = "Tasa libre de riesgo anual",value = 0.07,
                                                min = 0.01,max = 0.20,step = 0.01),
                                    sliderInput(inputId = 'q1',label = "Tasa de Dividendo",value = 0.03,
                                                min = 0.01,max = 0.30,step = 0.01),
                                    sliderInput(inputId = 'tt1',label = "Tiempo expresado en días",value = 180,
                                                min = 1,max = 360,step = 1)
                                  ))
                        )
                      )
                    )
)


server <- function(input,output){
  output$nota <- renderPrint({
  "Al calcular BS Divisas usa como la tasa local a la tasa libre de riesgo y para la tasa foranea usa la tasa de dividendos, para BS Futuros y BS sin Dividendos: usa la tasa libre de riesgo"
  })
  output$t1 <- renderText({
    if(input$M=="BS sin Dividendos"){
      bscall(input$S,input$K,input$sigma,input$r,input$tt/360,0)
    }
    else if(input$M=="BS con Dividendos"){
      print(bscall(input$S,input$K,input$sigma,input$r,input$tt/360,input$q),digits=4)
    }else if(input$M=="BS Divisas"){
      #r rlocal y q tasa foranea
      print(bscall(input$S,input$K,input$sigma,input$r,input$tt/360,input$q),digits=4)
    }
    else{#bscall(F0,K,sigma,r,tt,r)
      print(bscall(input$S,input$K,input$sigma,input$r,input$tt/360,input$r), digits=4)
    }  
  })
  output$t2 <- renderText({
    if(input$M=="BS sin Dividendos"){
      print(bsput(input$S,input$K,input$sigma,input$r,input$tt/360,0),digits=4)
    }
    else if(input$M=="BS con Dividendos"){
      print(bsput(input$S,input$K,input$sigma,input$r,input$tt/360,input$q),digits=4)
    }else if(input$M=="BS Divisas"){
      #r rlocal y q tasa foranea
      print(bsput(input$S,input$K,input$sigma,input$r,input$tt/360,input$q),digits=4)
    }
    else{#bscall(F0,K,sigma,r,tt,r)
      print(bsput(input$S,input$K,input$sigma,input$r,input$tt/360,input$r), digits=4)
    }  
  })
  output$t5 <- renderText({
    if(input$Op1=="Call"){
      if(input$M1=="BS sin Dividendos"){
        bscallimpvol(input$S1,input$K1,input$r1,input$tt1/360,0,input$S1*(1-input$sigma1))
      }
      else if(input$M=="BS con Dividendos"){
        bscallimpvol(input$S1,input$K1,input$r1,input$tt1/360,input$q1,input$S1*(1-input$sigma1))
      }else if(input$M=="BS Divisas"){
        bscallimpvol(input$S1,input$K1,input$r1,input$tt1/360,input$q1,input$S1*(1-input$sigma1))
      }
      else{
        bscallimpvol(input$S1,input$K1,input$r1,input$tt1/360,input$r1,input$S1*(1-input$sigma1))
      } } 
    else {if(input$M1=="BS sin Dividendos"){
      bsputimpvol(input$S1,input$K1,input$r1,input$tt1/360,0,input$S1*(1+input$sigma1))
    }
      else if(input$M=="BS con Dividendos"){
        bsputimpvol(input$S1,input$K1,input$r1,input$tt1/360,input$q1,input$S1*(1+input$sigma1))
      }else if(input$M=="BS Divisas"){
        bsputimpvol(input$S1,input$K1,input$r1,input$tt1/360,input$q1,input$S1*(1+input$sigma1))
      }
      else{
        bsputimpvol(input$S1,input$K1,input$r1,input$tt1/360,input$r1,input$S1*(1+input$sigma1))
      }}
  })
  
  output$nombres <- renderTable({
    M=matrix(NA,nrow = 8,ncol = 1)
    rownames(M)<- c("Prima","Delta","Gamma","Vega","Rho","Theta","Psi","Elasticidad")})
  
  output$t3 <- renderTable({
    M=matrix(NA,nrow = 8,ncol = 1)
    rownames(M)<- c("Prima","Delta","Gamma","Vega","Rho","Theta","Psi","Elasticidad")
    if(input$Op1=="Call"){
      if(input$M1=="BS sin Dividendos"){
        greeks2(bscall, list(s=input$S1, k=input$K1, v=input$sigma1, r=input$r1, tt=input$tt1/360, d=0))
      }
      else if(input$M=="BS con Dividendos"){
        greeks2(bscall, list(s=input$S1,k=input$K1,v=input$sigma1,r=input$r1,tt=input$tt1/360,d=input$q1))
      }else if(input$M=="BS Divisas"){
        greeks2(bscall, list(s=input$S1,k=input$K1,v=input$sigma1,r=input$r1,tt=input$tt1/360,d=input$q1))
      }
      else{
        greeks2(bscall,list(s=input$S1,k=input$K1,v=input$sigma1,r=input$r1,tt=input$tt1/360,d=input$r1))
      } } 
    else {if(input$M1=="BS sin Dividendos"){
      greeks2(bsput, list(s=input$S1, k=input$K1, v=input$sigma1, r=input$r1, tt=input$tt1/360, d=0))
    }
      else if(input$M=="BS con Dividendos"){
        greeks2(bsput, list(s=input$S1,k=input$K1,v=input$sigma1,r=input$r1,tt=input$tt1/360,d=input$q1))
      }else if(input$M=="BS Divisas"){
        greeks2(bsput, list(s=input$S1,k=input$K1,v=input$sigma1,r=input$r1,tt=input$tt1/360,d=input$q1))
      }
      else{
        greeks2(bsput,list(s=input$S1,k=input$K1,v=input$sigma1,r=input$r1,tt=input$tt1/360,d=input$r1))
      }}
    
  })
  output$g1 <- renderPlot({
    if(input$M=="BS sin Dividendos"){
      s=seq(0.5,100,0.5)
      K <- input$K
      sigma <-input$sigma
      r<-input$r
      tt<-input$tt/360
      call = greeks(bscall(s,K,sigma,r,tt,0))
      par(mar = c(4, 3, 1, 1), mfrow = c(1, 2))
      plot(s,call[1,],main = "Payoff Call",ylab = "Payoff",type = "l",col="purple",lwd = 4)
      put = greeks(bsput(s,K,sigma,r,tt,0))
      plot(s,put[1,],main = "Payoff Put",ylab = "Payoff",type = "l",col="purple",lwd = 4)
      
    }
    else if(input$M=="BS con Dividendos"){
      s=seq(0.5,100,0.5)
      call = greeks2(bscall, list(s=s,k=input$K,v=input$sigma,r=input$r,tt=input$tt/360,d=input$q))
      par(mar = c(4, 3, 1, 1), mfrow = c(1, 2))
      plot(s,call[1,],main = "Payoff Call",ylab = "Payoff",type = "l",col="purple",lwd = 4)
      put =  greeks2(bsput, list(s=s,k=input$K,v=input$sigma,r=input$r,tt=input$tt/360,d=input$q))
      plot(s,put[1,],main = "Payoff Put",ylab = "Payoff",type = "l",col="purple",lwd = 4)
      
    }else if(input$M=="BS Divisas"){
      #r rlocal y q tasa foranea
      s=seq(0.5,100,0.5)
      call = greeks2(bscall, list(s=s,k=input$K,v=input$sigma,r=input$r,tt=input$tt/360,d=input$q))
      par(mar = c(4, 3, 1, 1), mfrow = c(1, 2))
      plot(s,call[1,],main = "Payoff Call",ylab = "Payoff",type = "l",col="maroon",lwd = 4)
      put =  greeks2(bsput, list(s=s,k=input$K,v=input$sigma,r=input$r,tt=input$tt/360,d=input$q))
      plot(s,put[1,],main = "Payoff Put",ylab = "Payoff",type = "l",col="maroon",lwd = 4)
      
    }
    else{#bscall(F0,K,sigma,r,tt,r)
      s=seq(0.5,100,0.5)
      K <- input$K
      sigma <-input$sigma
      r<-input$r
      tt<-input$tt/360
      call = greeks(bscall(s,K,sigma,r,tt,r))
      par(mar = c(4, 3, 1, 1), mfrow = c(1, 2))
      plot(s,call[1,],main = "Payoff Call",ylab = "Payoff",type = "l",col="maroon",lwd = 4)
      put = greeks(bsput(s,K,sigma,r,tt,r))
      plot(s,put[1,],main = "Payoff Put",ylab = "Payoff",type = "l",col="maroon",lwd = 4)
      
    } })
  output$g3 <- renderPlot({
    if(input$M1=="BS sin Dividendos"){
      if(input$Op1=="Call"){
        s=seq(0.5,100,0.5)
        K <- input$K1
        sigma <-input$sigma1
        r<-input$r1
        tt<-input$tt1/360
        q <-input$q1
        call = greeks(bscall(s,K,sigma,r,tt,0))
        par(mar = c(4, 5, 1, 1))
        par(mfrow = c(4, 2))
        for(i in row.names(call)){
          plot(s,call[i,],main = paste(i),ylab = i,type = "l",col="maroon",lwd = 3) 
        }
      } 
      else{ s=seq(0.5,100,0.5)
      K <- input$K1
      sigma <-input$sigma1
      r<-input$r1
      tt<-input$tt1/360
      
        put = greeks(bsput(s,K,sigma,r,tt,0))
        par(mar = c(4, 5, 1, 1))
        par(mfrow = c(4, 2))
        for(i in row.names(put)){
          plot(s,put[i,],main = paste(i),ylab = i,type = "l",col="maroon",lwd = 3) 
        }
        
      }
    } else if(input$M1=="BS con Dividendos"){
      if(input$Op1=="Call"){
        s=seq(0.5,100,0.5)
        call = greeks2(bscall, list(s=s,k=input$K1,v=input$sigma1,r=input$r1,tt=input$tt1/360,d=input$q1))
        par(mar = c(4, 5, 1, 1))
        par(mfrow = c(4, 2))
        for(i in row.names(call)){
          plot(s,call[i,],main = paste(i),ylab = i,type = "l",col="maroon",lwd = 3) 
        }
      } 
      else{ s=seq(0.5,100,0.5)
      put = greeks2(bsput, list(s=s,k=input$K1,v=input$sigma1,r=input$r1,tt=input$tt1/360,d=input$q1))
      par(mar = c(4, 5, 1, 1))
      par(mfrow = c(4, 2))
      for(i in row.names(put)){
        plot(s,put[i,],main = paste(i),ylab = i,type = "l",col="maroon",lwd = 3) 
      }}} 
    else if(input$M1=="BS Divisas"){
      if(input$Op1=="Call"){
        s=seq(0.5,100,0.5)
        call = greeks2(bscall, list(s=s,k=input$K1,v=input$sigma1,r=input$r1,tt=input$tt1/360,d=input$q1))
        par(mar = c(4, 5, 1, 1))
        par(mfrow = c(4, 2))
        for(i in row.names(call)){
          plot(s,call[i,],main = paste(i),ylab = i,type = "l",col="maroon",lwd = 3) 
        }
      } 
      else{ s=seq(0.5,100,0.5)
      put = greeks2(bsput, list(s=s,k=input$K1,v=input$sigma1,r=input$r1,tt=input$tt1/360,d=input$q1))
      par(mar = c(4, 5, 1, 1))
      par(mfrow = c(4, 2))
      for(i in row.names(put)){
        plot(s,put[i,],main = paste(i),ylab = i,type = "l",col="maroon",lwd = 3) 
      }}} 
    else {
      if(input$Op1=="Call"){
        s=seq(0.5,100,0.5)
        call = greeks2(bscall, list(s=s,k=input$K1,v=input$sigma1,r=input$r1,tt=input$tt1/360,d=input$r1))
        par(mar = c(4, 5, 1, 1))
        par(mfrow = c(4, 2))
        for(i in row.names(call)){
          plot(s,call[i,],main = paste(i),ylab = i,type = "l",col="maroon",lwd = 3) 
        }
      } 
      else{ s=seq(0.5,100,0.5)
      put = greeks2(bsput, list(s=s,k=input$K1,v=input$sigma1,r=input$r1,tt=input$tt1/360,d=input$r1))
      par(mar = c(4, 5, 1, 1))
      par(mfrow = c(4, 2))
      for(i in row.names(put)){
        plot(s,put[i,],main = paste(i),ylab = i,type = "l",col="maroon",lwd = 3) 
      }}} 
  })
  
}

shinyApp(ui, server)
