# example from http://shiny.rstudio.com/gallery/kmeans-example.html

library(shiny)
library(data.table)
library(flexdashboard)
library(shinydashboard)
library(shinythemes)
library(DT)

data <- fread("/Users/joseizammontt/Desktop/Universidad/master/thesis/codigo/DATA_UNV_2.csv", header=TRUE, stringsAsFactors=FALSE)
data <- data[F_UNIDADESVENTA>0.000001,]
data <- data[,WEEK:=strftime(as.POSIXlt(data$ID_DIAANALISIS, format = "%Y-%d-%m"), "%W")]
data <- data[,.(WEEK,DESC_LOCALFISICO,COD_ZONAL,DIVISION,DEPARTAMENTO,SUBDEPARTAMENTO,CLASE,SUBCLASE,COD_SKU,DESC_SKU,F_UNIDADESVENTA,F_MONTOVENTA,F_COSTOVENTA)]

fluidPage(theme = shinytheme("united"),
          tags$head(
            tags$style(HTML('.navbar-static-top {background-color: green; border-color: green;}'))
          ),
          navbarPage("Tottus Dashboard",
            tabPanel("Gráfico",
              # Sidebar layout with input and output definitions ----
              sidebarLayout(
                
                # Sidebar panel for inputs ----
                sidebarPanel(
                  
                  # Input: Slider for the number of bins ----
                  selectInput("selectInput1",
                              "Selecionar categoria:",choices = unique(data$CLASE), 
                              selected = "J03010421 - POLLO ENTERO"
                              ),
                  selectInput("selectInput2",
                              "Selecionar local:",choices = unique(data$DESC_LOCALFISICO),
                              selected = "La Marina"
                  ),
                  selectInput("selectInput3",
                              "Selecionar DESC_SKU:",choices = unique(data$DESC_SKU),
                              selected = "POLLO FRESCO TTN GR X KG"
                  ),
                  selectInput("from_date",
                              "Desde la semana:",choices = sort(unique(data$WEEK)),
                              selected = min(unique(data$WEEK))
                  ),
                  selectInput("to_date",
                              "Hasta la semana:",choices = sort(unique(data$WEEK)),
                              selected = max(unique(data$WEEK))
                  )
                  
                ),
                
                # Main panel for displaying outputs ----
                mainPanel(
                  # Output: Histogram ----
                  plotOutput(outputId = "distPlot",height = "300px"),
                  tags$hr(),
                  plotOutput(outputId = "distPlot2",height = "300px", width = "97%"),
                  h2("Statistical summary of demands by SKU"),
                  DT::dataTableOutput("Demand_summary"),
                  h2("Statistical summary of prices by SKU"),
                  DT::dataTableOutput("price_summary")
                )
                
              )
            ),
            tabPanel("Ventas por local", 
                     titlePanel("Ventas por local"),
                     
                     sidebarLayout(
                       
                       # Sidebar panel for inputs ----
                       sidebarPanel(
                         # Input: Slider for the number of bins ----
                         selectInput("qzonal",
                                     "Nivel de agregación:",choices = c("Zonal","Local"),
                                     selected ="Local"
                         ),
                         conditionalPanel(
                           condition = "input.qzonal == 'Zonal'",
                           selectInput("cod_zonal",
                                       "Zona:",choices = unique(data$COD_ZONAL),
                                       selected = "LM"
                           )
                         ),
                         conditionalPanel(
                           condition = "input.qzonal == 'Local'",
                           selectInput("selectInput4",
                                       "Top:",choices = seq(1:84),
                                       selected =20
                           )
                        
                         )
                         
                       ),
                       
                       # Main panel for displaying outputs ----
                       mainPanel(
                         # Output
                         conditionalPanel(
                           condition = "input.qzonal == 'Zonal'",
                           plotOutput(outputId = "Zone_Ranking")
                           ),
                         tags$hr(),
                         plotOutput(outputId = "distPlot3")
                         
                       )
                       
                     )
            ),
            tabPanel("Ventas por nivel", 
                     titlePanel("Ventas por nivel"),
                     
                     sidebarLayout(
                       
                       # Sidebar panel for inputs ----
                       sidebarPanel(
                         
                         selectInput("selectInput5",
                                     "Nivel:",choices = c("DIVISION","DEPARTAMENTO","SUBDEPARTAMENTO","CLASE","SUBCLASE"),
                                     selected ="CLASE"
                         ),
                         conditionalPanel(
                           condition = "input.selectInput5 == 'CLASE' || input.selectInput5 == 'SUBCLASE'",
                           selectInput("selectInput6",
                                       "Top:",choices = seq(1:79),
                                       selected =20
                           )
                         )
                       ),
                       
                       # Main panel for displaying outputs ----
                       mainPanel(
                         # Output
                         plotOutput(outputId = "distPlot4")
                         
                       )
                       
                     )
            ),
            tabPanel("Detalle", 
                     titlePanel("Detalle Ventas"),
                     
                     sidebarLayout(
                       
                       # Sidebar panel for inputs ----
                       sidebarPanel(
                         selectInput("selectInput7",
                                     "Local:",choices = unique(data$DESC_LOCALFISICO),
                                     selected = "La Marina"
                                     ),
                         selectInput("selectInput8",
                                     "Clase:",choices = unique(data$CLASE),
                                     selected = "J03010421 - POLLO ENTERO"
                                    )
                       ),
                       # Main panel for displaying outputs ----
                       mainPanel(
                         # Output
                         plotOutput(outputId = "distPlot5", height = "550px")
                         
                       )
                       
                     )
            )
          )
)
        
