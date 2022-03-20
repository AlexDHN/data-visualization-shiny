library ( shiny )
library(plotly)
library(quantmod)
library(tidyverse)
library(tidyquant)
library(ggplot2)
library(dplyr)
library(priceR)
library(reshape2)

# Server logic
server <- function(input, output) {
  dataInput <- reactive({
    
  })
  
  output$cours <- renderPlot({
    
    if(input$type=="AIRE"){
      AAPL <- tq_get(input$symb,
                     get = "stock.prices",
                     from = input$dates[1],
                     to = input$dates[2])
      data = data.frame(x = AAPL$date, y1 = AAPL$close)#, y2 = IND$close)
      ggplot(data, aes(x)) +
        geom_area(aes(y=y1),color=ifelse(AAPL$close[1] < tail(AAPL$close,n=1),"#006400","#8B0000"),
                  fill=ifelse(AAPL$close[1] < tail(AAPL$close,n=1),"#7FFF00","#FFA07A")) +
        #geom_line(aes(y=y2),color="red") +
        labs(title = paste("COURS de l'action",input$symb), y = "Prix a la fermeture ($)", x = "") + 
        theme_tq()
    }
    else {
      if(input$type=="LIGNE"){
        AAPL <- tq_get(input$symb,
                       get = "stock.prices",
                       from = input$dates[1],
                       to = input$dates[2])
        data = data.frame(x = AAPL$date, y1 = AAPL$close)#, y2 = IND$close)
        ggplot(data, aes(x)) +
          geom_line(aes(y=y1),color="darkblue") +
          #geom_line(aes(y=y2),color="red") +
          labs(title = paste("COURS de l'action",input$symb), y = "Prix a la fermeture ($)", x = "") + 
          theme_tq()
      }
      else{
        if(input$type=="PERFORMANCE"){
          AAPL <- tq_get(input$symb,
                         get = "stock.prices",
                         from = input$dates[1],
                         to = input$dates[2])
          AAPL$perf = ((AAPL$close-AAPL$open[1])/AAPL$open[1])*100
          data = data.frame(x = AAPL$date, y1 = AAPL$perf)#, y2 = IND$close)
          ggplot(data, aes(x)) +
            geom_area(aes(y=y1),color="darkblue",fill="lightblue") +
            #geom_line(aes(y=y2),color="red") +
            labs(title = paste("PERFORMANCE de l'action",input$symb), y = "Performance de L'action (%)", x = "") + 
            theme_tq()
          
        }
        else{
          if(input$type=="BOUGIE"){
            getSymbols(input$symb, 
                       from = input$dates[1],
                       to = input$dates[2],
                       src = "yahoo")
            chartSeries(get(input$symb),theme=chartTheme('white'))
            
          }
        }
      }
    }
    
  })
  
  output$volume <- renderPlot({
    if(input$type!="BOUGIE"){
      AAPL <- tq_get(input$symb,
                     get = "stock.prices",
                     from = input$dates[1],
                     to = input$dates[2])
      filll <- if_else(((AAPL$close-AAPL$open)/AAPL$open)*100 >=0,"#00FF00","#FF0000")
      #data = data.frame(x = AAPL$date, y = AAPL$volume, fill = if_else(((AAPL$close-AAPL$open)/AAPL$open)*100 >=0,"#00FF00","#FF0000"))
      ggplot(AAPL, aes(x=date, y=volume, fill = filll)) +
        geom_bar(stat="identity",show.legend = FALSE) +
        scale_fill_manual(values=c("#00FF00","#FF0000")) +
        labs(title = "VOLUME", y = "", x = "") + 
        theme_tq()
    }
  })
  
  output$resume <- renderTable({
    AAPL <- tq_get(input$symb,
                   get = "stock.prices",
                   from = input$dates[2]-1,
                   to = input$dates[2])
    AAPL <- AAPL[,-2]
    AAPL$adjusted <- round(((AAPL$close-AAPL$open)/AAPL$open)*100,digits = 3)
    names(AAPL) <- c("Symbole","Ouverture","Maximum","Minimum","Fermeture","Volume","Performance")
    tail(AAPL,n=1)
  })
  
  output$histo <- DT::renderDataTable({
    AAPL <- tq_get(input$symb,
                   get = "stock.prices",
                   from = input$dates[1],
                   to = input$dates[2])
    AAPL$adjusted <- round(((AAPL$close-AAPL$open)/AAPL$open)*100,digits = 3)
    names(AAPL) <- c("Symbole","Date","Ouverture","Maximum","Minimum","Fermeture","Volume","Performance")
    DT::datatable(AAPL, options = list(pageLength = 5, searching = FALSE))
  })
  
  
  output$comp <- renderPlot({
    
    
    
    AAPL <- tq_get(input$symb,
                   get = "stock.prices",
                   from = input$dates[1],
                   to = input$dates[2])
    
    index <- data.frame(indice = c("DAX 30","FTSE 100","CAC 40",
                                   "Dow Jones","S&P 500","Nasdaq","Nikkei 225"),
                        symb = c("^GDAXI","^FTSE","^FCHI","^DJI","^GSPC","^IXIC","^N225"))
    
    IND <- tq_get(index$symb[index$indice==input$bourse],
                  get = "stock.prices",
                  from = input$dates[1],
                  to = input$dates[2])
    
    AAPL$perf = ((AAPL$close-AAPL$open[1])/AAPL$open[1])*100
    IND$perf = ((IND$close-IND$open[1])/IND$open[1])*100
    data = data.frame(x = AAPL$date, y1 = AAPL$perf,y2 = IND$perf[1:length(AAPL$perf)])
    ggplot(data, aes(x)) +
      geom_area(aes(y=y1),color="darkblue",fill="lightblue") +
      geom_line(aes(y=y2),color="red") +
      labs(title = paste("Correlation :",cor(AAPL$perf,IND$perf[1:length(AAPL$perf)])), y = "Performance de L'action (%)", x = "") + 
      theme_tq()
  })
  
  output$dividende <- renderTable({
    
    chart_Series(getDividends(getDividends(input$symb)))
    
  })
  
  output$ex1 <- DT::renderDataTable({
    from <- c("USD","USD","USD","USD","USD","USD","USD","USD","USD","USD","USD","EUR","EUR","EUR","EUR","EUR","EUR","EUR","EUR","EUR","EUR","EUR","JPY","JPY","JPY","JPY","JPY","JPY","JPY","JPY","JPY","JPY","JPY","GBP","GBP","GBP","GBP","GBP","GBP","GBP","GBP","GBP","GBP","GBP","CHF","CHF","CHF","CHF","CHF","CHF","CHF","CHF","CHF","CHF","CHF","CAD","CAD","CAD","CAD","CAD","CAD","CAD","CAD","CAD","CAD","CAD","MAD","MAD","MAD","MAD","MAD","MAD","MAD","MAD","MAD","MAD","MAD","CNY","CNY","CNY","CNY","CNY","CNY","CNY","CNY","CNY","CNY","CNY","THB","THB","THB","THB","THB","THB","THB","THB","THB","THB","THB","ILS","ILS","ILS","ILS","ILS","ILS","ILS","ILS","ILS","ILS","ILS","RUB","RUB","RUB","RUB","RUB","RUB","RUB","RUB","RUB","RUB","RUB","RUB")
    to <- c("USD", "JPY", "GBP", "CHF","CAD","MAD","CNY","THB","ILS","RUB","EUR","USD", "JPY", "GBP", "CHF","CAD","MAD","CNY","THB","ILS","RUB","EUR","USD", "JPY", "GBP", "CHF","CAD","MAD","CNY","THB","ILS","RUB","EUR","USD", "JPY", "GBP", "CHF","CAD","MAD","CNY","THB","ILS","RUB","EUR","USD", "JPY", "GBP", "CHF","CAD","MAD","CNY","THB","ILS","RUB","EUR","USD", "JPY", "GBP", "CHF","CAD","MAD","CNY","THB","ILS","RUB","EUR","USD", "JPY", "GBP", "CHF","CAD","MAD","CNY","THB","ILS","RUB","EUR","USD", "JPY", "GBP", "CHF","CAD","MAD","CNY","THB","ILS","RUB","EUR","USD", "JPY", "GBP", "CHF","CAD","MAD","CNY","THB","ILS","RUB","EUR","USD", "JPY", "GBP", "CHF","CAD","MAD","CNY","THB","ILS","RUB","EUR","USD", "JPY", "GBP", "CHF","CAD","MAD","CNY","THB","ILS","RUB","EUR")
    aapl <- getQuote(paste0(from, to, "=X"))
    names(aapl) <- c("Heure", "Taux_de_change", "Evolution", "Taux de change(%)", "Ouverture", "Au plus haut", "Au plus bas", "Volume")
    DT::datatable(aapl, options = list(pageLength = 5))
  })
  
  output$conversion  <- renderText({
    n <- input$obs
    aapl_bis <- getQuote(paste0(input$type1, input$type2, "=X"))
    res <- n*aapl_bis[1,2]
    if(input$obs<=1){
      paste(as.character(input$obs), input$type1 ,"vaut", res, input$type2)
    }else{
      paste(as.character(input$obs), input$type1 ,"valent", res, input$type2)
    }
    
    
  })
  
  output$change <- renderPlot({
    
    monnaie1 <- input$type1
    monnaie2 <- input$type2
    from = input$dates2[1]
    to = input$dates2[2]
    
    graph <- historical_exchange_rates(as.character(monnaie1), as.character(monnaie2), start_date = as.character(from), end_date = as.character(to))
    #name <- names(graph)[1]
    graph <- melt(graph ,  id.vars = 'date', variable.name = "one_USD_equivalent_to_x_EUR")
    
    
    
    ggplot(graph, mapping = aes(date,value)) + geom_line(aes(colour = one_USD_equivalent_to_x_EUR))
    
  })
  
 output$teste <- DT::renderDataTable({   DT::datatable(RV, options = list(pageLength = 5, searching = FALSE))
  })
  
   output$contents <- renderTable({
     
     # input$file1 will be NULL initially. After the user selects
     # and uploads a file, head of that data file by default,
     # or all rows if selected, will be shown.
     
     req(input$file1)
     
     # when reading semicolon separated files,
     # having a comma separator causes `read.csv` to error
     tryCatch(
       {
         df <- read.csv(input$file1$datapath,
                        header = FALSE,
                        sep = ",",
                        quote = '"')
       },
       error = function(e) {
         # return a safeError if a parsing error occurs
         stop(safeError(e))
       }
     )
     names(df) <- c("Symbole","Quantite","Cout d'achat")
     return(df)
  })
 
   
   output$com <- renderPlot({
     
     req(input$file1)
     
     # when reading semicolon separated files,
     # having a comma separator causes `read.csv` to error
     tryCatch(
       {
         df <- read.csv(input$file1$datapath,
                        header = FALSE,
                        sep = ",",
                        quote = '"')
       },
       error = function(e) {
         # return a safeError if a parsing error occurs
         stop(safeError(e))
       }
     )
     
     names(df) <- c("Symbole","Quantite","Cout d'achat")
     
     n <- length(df$Symbole)
     
     AAPL <- tq_get(df$Symbole[1],
                    get = "stock.prices",
                    from = input$dat[1],
                    to = input$dat[2])
     
     l <- length(AAPL$date)
     au <- data.frame(date = AAPL$date)
     au$close = rep(0,l)
     au$open = rep(0,l)
     for (i in 1:n){
       AAPL <- tq_get(df$Symbole[i],
                      get = "stock.prices",
                      from = input$dat[1],
                      to = input$dat[2])
       au$close =  AAPL$close*df$Quantite[i] + au$close
       au$open =   AAPL$open*df$Quantite[i] + au$open
     }
     au$perf = ((au$close-au$open[1])/au$open[1])*100
     
     index <- data.frame(indice = c("DAX 30","FTSE 100","CAC 40",
                                    "Dow Jones","S&P 500","Nasdaq","Nikkei 225"),
                         symb = c("^GDAXI","^FTSE","^FCHI","^DJI","^GSPC","^IXIC","^N225"))
     
     IND <- tq_get(index$symb[index$indice==input$bours],
                   get = "stock.prices",
                   from = input$dat[1],
                   to = input$dat[2])
     
     IND$perf = ((IND$close-IND$open[1])/IND$open[1])*100
     data = data.frame(x = au$date, y1 = au$perf,y2 = IND$perf[1:length(au$perf)])
     ggplot(data, aes(x)) +
       geom_area(aes(y=y1),color="darkblue",fill="lightblue") +
       geom_line(aes(y=y2),color="red") +
       labs(title = paste("Correlation :",cor(au$perf,IND$perf[1:length(au$perf)])), y = "Performance de L'action (%)", x = "") + 
       theme_tq()
   })  
   
   output$compPlot <- renderPlot({
     liste_devise <- list(usd="USD", jpy="JPY", gbp="GBP", chf="CHF",cad="CAD",mad="MAD",cny="CNY",thb="THB",ils="ILS",rub="RUB",eur="EUR")
     liste_devise <- liste_devise[liste_devise!=input$comp]
     liste_price <- list()
     for(i in 1:length(liste_devise)){
       aapl_bis <- getQuote(paste0(input$comp, liste_devise[[i]], "=X"))
       res <- aapl_bis[1,2]
       liste_price[[i]] <- res
     }
     
     df <- data.frame(Devises=unlist(liste_devise),
                      Devise_selectionnee = unlist(liste_price))
     
     
     ggplot(df, aes(x=Devises, y=Devise_selectionnee))+geom_bar(stat = "identity") + geom_text(aes(label=Devise_selectionnee), vjust=-0.3, size = 3.5)
     
   })
   
   
   
}
