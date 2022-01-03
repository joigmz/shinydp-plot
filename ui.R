# example from http://shiny.rstudio.com/gallery/kmeans-example.html

library(shiny)
library(data.table)
library(flexdashboard)
library(shinydashboard)
library(shinythemes)

data <- fread("/Users/joseizammontt/Desktop/Universidad/master/thesis/codigo/DATA_UNV_2.csv", header=TRUE, stringsAsFactors=FALSE)
data <- data[F_UNIDADESVENTA>0.000001,]
data <- data[,WEEK:=strftime(as.POSIXlt(data$ID_DIAANALISIS, format = "%Y-%d-%m"), "%W")]
data <- data[,.(WEEK,DESC_LOCALFISICO,CLASE,COD_SKU,DESC_SKU,F_UNIDADESVENTA,F_MONTOVENTA,F_COSTOVENTA)]

fluidPage(theme = shinytheme("united"),
          navbarPage("Tottus Dashboard:",
            tabPanel("Gráfico",
              # Sidebar layout with input and output definitions ----
              sidebarLayout(
                
                # Sidebar panel for inputs ----
                sidebarPanel(
                  
                  # Input: Slider for the number of bins ----
                  selectInput("selectInput1",
                              "Selecionar categoria:",choices = unique(data$CLASE)
                              ),
                  selectInput("selectInput2",
                              "Selecionar local:",choices = unique(data$DESC_LOCALFISICO)
                  ),
                  selectInput("selectInput3",
                              "Selecionar DESC_SKU:",choices = unique(data$DESC_SKU)
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
                         selectInput("selectInput4",
                                     "Top:",choices = seq(1:84)

                         )
                       ),
                       
                       # Main panel for displaying outputs ----
                       mainPanel(
                         # Output
                         plotOutput(outputId = "distPlot3")
                         
                       )
                       
                     )
            ),
            tabPanel("Ventas por clase", 
                     titlePanel("Ventas por clase"),
                     
                     sidebarLayout(
                       
                       # Sidebar panel for inputs ----
                       sidebarPanel(
                         
                         # Input: Slider for the number of bins ----
                         selectInput("selectInput5",
                                     "Top:",choices = seq(1:79)
                                     
                         )
                       ),
                       
                       # Main panel for displaying outputs ----
                       mainPanel(
                         # Output
                         plotOutput(outputId = "distPlot4")
                         
                       )
                       
                     )
            )
          )
)
        
