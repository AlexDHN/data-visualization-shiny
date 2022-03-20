# Load packages ----
library(shiny)
library(plotly)
library(quantmod)
library(tidyverse)
library(tidyquant)
library(ggplot2)
library(dplyr)
library(priceR)
library(reshape2)


# User interface ----
ui <- fluidPage(
  #shinythemes::themeSelector(),
  theme = "flatly",
  navbarPage("Finance by DHEZACK Corp", tabPanel("ACTIONS",
                                                 
                                                 titlePanel("COTATION"),
                                                 
                                                 sidebarLayout(
                                                   sidebarPanel(width = 3,
                                                                helpText("Selectionne une cotation. Les informations sont collectees depuis Yahoo finance."),
                                                                textInput("symb", "Symbole", "AAPL"),
                                                                selectInput("type", 
                                                                            label = "Styles de cours",
                                                                            choices = list("AIRE", 
                                                                                           "LIGNE",
                                                                                           "BOUGIE",
                                                                                           "PERFORMANCE"),
                                                                            selected = "AIRE"),
                                                                dateRangeInput("dates",
                                                                               "Echelle de temps",
                                                                               start = "2021-01-01",
                                                                               end = as.character(Sys.Date()-1)),
                                                                
                                                   ),
                                                   
                                                   mainPanel(
                                                     tabsetPanel(
                                                       tabPanel("COURS",
                                                                plotOutput("cours",height = 300),
                                                                plotOutput("volume",height = 150),
                                                                tableOutput("resume")
                                                       ),
                                                       tabPanel("DIVIDENDE",br(),
                                                                plotOutput("dividende",height = 300)
                                                       ),
                                                       tabPanel("COMPARAISON",
                                                                br(),
                                                                selectInput('bourse', 'Indice Boursier', c("DAX 30","FTSE 100","CAC 40",
                                                                                                           "Dow Jones","S&P 500","Nasdaq","Nikkei 225")),
                                                                br(),
                                                                plotOutput("comp",height = 500)
                                                       ),
                                                       tabPanel("HISTORIQUE",
                                                                titlePanel("DONNEES HISTORIQUES"),
                                                                br(),
                                                                DT::dataTableOutput("histo")    
                                                       )
                                                       
                                                     )
                                                   )
                                                   
                                                 )
                                                 
  ),
             
  tabPanel("MON PORTEFEUILLE", 
           
             sidebarPanel(
               # Input: Select a file ----
               fileInput("file1", "Choose CSV File",
                         multiple = FALSE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
             dateRangeInput("dat",
                            "Echelle de temps",
                            start = "2022-01-01",
                            end = as.character(Sys.Date()))
           ),
           mainPanel(
             tabsetPanel(
               tabPanel("CONTENU",
                          tableOutput("contents")
  
                        ),
               tabPanel("PERFORMANCE",
                        br(),
                        selectInput('bours', 'Indice Boursier', c("DAX 30","FTSE 100","CAC 40",
                                                                   "Dow Jones","S&P 500","Nasdaq","Nikkei 225")),
                        br(),
                        plotOutput("com",height = 500)
               ),
               tabPanel("AJOUTER UN TITRE",
                br(),
                sliderInput("nb_action","Combien d'action souhaitez vous acheter",5,min = 0, max = 100)
              )
             )
           )
      
  ),
  
  tabPanel("DEVISES", 
           
           navbarPage(
             title = 'Les principales monnaies',
             tabPanel('Les devises',     DT::dataTableOutput('ex1')),
             tabPanel('Convertir les devises',   
                      sidebarPanel(width = 6,
                                   
                                   helpText("Choisissez la conversion que vous souhaitez effectuer."),
                                   selectInput("type1", 
                                               label = "Convertir des ",
                                               choices = list("USD", "JPY", "GBP", "CHF","CAD","MAD","CNY","THB","ILS","RUB","EUR")
                                   ),
                                   selectInput("type2", 
                                               label = "en ",
                                               choices = list("USD", "JPY", "GBP", "CHF","CAD","MAD","CNY","THB","ILS","RUB","EUR")
                                   ),
                                   numericInput("obs", "Montant", 10),
                                   
                                   dateRangeInput("dates2",
                                                  "Choisir des dates pour voir les courbes",
                                                  start = "2020-01-01",
                                                  end = as.character(Sys.Date())),
                                   
                                   
                                   
                      ),
                      
                      
                      column(5,
                             verbatimTextOutput("conversion")
                             
                      ),
                      
                      plotOutput("change", width = "100%", height = "300px"),
                      
             ),
             
             tabPanel('Puissances',
                      sidebarPanel(width= 4,
                                   
                                   helpText("Choisissez la devise que vous souhaitez comparer aux autres."),
                                   selectInput("comp", 
                                               label = "Comparer des ",
                                               choices = list("USD", "JPY", "GBP", "CHF","CAD","MAD","CNY","THB","ILS","RUB","EUR")
                                   ),
                                   helpText("Les devises dont la bare est plus petite que la valeur 1 sont celles qui sont plus puissantes que la devise choisie"),
                                   
                      ),
                      
                      mainPanel(
                        plotOutput("compPlot")  
                      )
             )
             
             
             
             
           )
           
           
           
           
  ),
  
  
  )
  
)
