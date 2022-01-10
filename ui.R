# example from http://shiny.rstudio.com/gallery/kmeans-example.html

library(shiny)
library(data.table)
library(flexdashboard)
library(shinydashboard)
library(shinythemes)

data <- fread("/Users/joseizammontt/Desktop/Universidad/master/thesis/codigo/DATA_UNV_2.csv", header=TRUE, stringsAsFactors=FALSE)
data <- data[F_UNIDADESVENTA>0.000001,]
data <- data[,WEEK:=strftime(as.POSIXlt(data$ID_DIAANALISIS, format = "%Y-%d-%m"), "%W")]
data <- data[,.(WEEK,DESC_LOCALFISICO,COD_ZONAL,DIVISION,DEPARTAMENTO,SUBDEPARTAMENTO,CLASE,SUBCLASE,COD_SKU,DESC_SKU,F_UNIDADESVENTA,F_MONTOVENTA,F_COSTOVENTA)]

fluidPage(theme = shinytheme("united"),
          navbarPage("Tottus Dashboard",
           tags$head(tags$style(HTML('.navbar-static-top {background-color: green; border-color: green;}',
                                     '.navbar-default .navbar-nav>.active>a {background-color: green;}'))),
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
                  )
                  
                ),
                
                # Main panel for displaying outputs ----
                mainPanel(
                  # Output: Histogram ----
                  plotOutput(outputId = "distPlot",height = "300px"),
                  plotOutput(outputId = "distPlot2",height = "300px", width = "97%")
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
                                     "Analisis por zona:",choices = c("Si","No"),
                                     selected ="No"
                         ),
                         conditionalPanel(
                           condition = "input.qzonal == 'Si'",
                           selectInput("cod_zonal",
                                       "Zona:",choices = unique(data$COD_ZONAL)
                           )
                         ),
                         conditionalPanel(
                           condition = "input.qzonal == 'No'",
                           selectInput("selectInput4",
                                       "Top:",choices = seq(1:84),
                                       selected =20
                           )
                        
                         )
                         
                       ),
                       
                       # Main panel for displaying outputs ----
                       mainPanel(
                         # Output
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
        
