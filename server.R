# example from http://shiny.rstudio.com/gallery/kmeans-example.html

library(shiny)
library(ggplot2)
library(dplyr)
library(data.table)
library(scales)

data <- fread("/Users/joseizammontt/Desktop/Universidad/master/thesis/codigo/DATA_UNV_2.csv", header=TRUE, stringsAsFactors=FALSE)
data <- data[F_UNIDADESVENTA>0.000001,]
data <- data[,WEEK:=strftime(as.POSIXlt(data$ID_DIAANALISIS, format = "%Y-%d-%m"), "%W")]
data <- data[,.(WEEK,DESC_LOCALFISICO,CLASE,COD_SKU,DESC_SKU,F_UNIDADESVENTA,F_MONTOVENTA,F_COSTOVENTA)]

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {
  
  output$distPlot <- renderPlot({
    
    data.grafico1 <-data[DESC_LOCALFISICO==input$selectInput2 & CLASE==input$selectInput1,]
    data.grafico1 <- data.grafico1[,.(MONTOVENTAS = sum(F_MONTOVENTA), DEMANDA = sum(F_UNIDADESVENTA)), keyby = .(WEEK,DESC_LOCALFISICO,CLASE,COD_SKU,DESC_SKU)]
    data.grafico1 <- data.grafico1[,PRECIOPROMEDIO:=data.grafico1$MONTOVENTAS/data.grafico1$DEMANDA]
    data.grafico1 <- data.grafico1[DESC_SKU==input$selectInput3,]
    
    x <- data.grafico1$WEEK
    y1 <- data.grafico1$PRECIOPROMEDIO
    y2 <- data.grafico1$DEMANDA
    
    p <- ggplot(data.grafico1, aes(x = x, group = 1))
    p <- p + geom_line(aes(y = y1, colour = "Precio"))

        # adding the relative humidity data, transformed to match roughly the range of the temperature
    p <- p + geom_line(aes(y = y2, colour = "Demanda"))
    
    # now adding the secondary axis, following the example in the help file ?scale_y_continuous
    # and, very important, reverting the above transformation
    p <- p + scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Precio",labels=scales::dollar_format()))
    
    # modifying colours and theme options
    p <- p + scale_colour_manual(values = c("blue", "red"))
    p <- p + labs(y = "Demanda",
                  x = "Week",
                  colour = "Curvas")
    p <- p + theme(legend.position = c(0.85, 0.2))
    p
    
  })
  
  output$distPlot2 <- renderPlot({
    
    data.grafico2 <-data[DESC_LOCALFISICO==input$selectInput2 & CLASE==input$selectInput1,]
    data.grafico2 <- data.grafico2[DESC_SKU==input$selectInput3,]
    data.grafico2 <- data.grafico2[,PRECIOVENTA:=data.grafico2$F_MONTOVENTA/data.grafico2$F_UNIDADESVENTA]
    
    p2 <- ggplot(data.grafico2, aes(x=WEEK, y=PRECIOVENTA)) + 
      geom_boxplot()+
      scale_y_continuous(labels=scales::dollar_format())
    p2
  })
  
  output$distPlot3 <- renderPlot({
    
    data.grafico3 <- data[,.(MONTOVENTAS = sum(F_MONTOVENTA)), keyby = .(DESC_LOCALFISICO)]
    data.grafico3 <- setorder(data.grafico3,-MONTOVENTAS)
    
    data.grafico3 <- data.grafico3[1:input$selectInput4,]
    
    ggplot(data.grafico3, aes(x = reorder(DESC_LOCALFISICO, MONTOVENTAS), y = MONTOVENTAS)) +
      geom_bar(stat = "identity",
               show.legend = FALSE,
               fill= "lightblue",
               color = "white") +
      geom_text(aes(label = dollar(round(MONTOVENTAS, 0)),
                    hjust = 1,
                    vjust = 0.5),
                size = 5)+
      xlab("LOCAL") +
      ylab("MONTOVENTAS") +
      coord_flip()+
      scale_y_continuous(labels=scales::dollar_format())
  })
  
  output$distPlot4 <- renderPlot({
    
    data.grafico4 <- data[,.(MONTOVENTAS = sum(F_MONTOVENTA)), keyby = .(CLASE)]
    data.grafico4 <- setorder(data.grafico4,-MONTOVENTAS)
    
    data.grafico4 <- data.grafico4[1:input$selectInput5,]
    
    ggplot(data.grafico4, aes(x = reorder(CLASE, MONTOVENTAS), y = MONTOVENTAS)) +
      geom_bar(stat = "identity",
               show.legend = FALSE,
               fill= "lightblue",
               color = "white") +
      geom_text(aes(label = dollar(round(MONTOVENTAS, 0)),
                    hjust = 1,
                    vjust = 0.5),
                size = 5)+
      xlab("CLASE") +
      ylab("MONTOVENTAS") +
      coord_flip()+
      scale_y_continuous(labels=scales::dollar_format())
  })
  
  output$distPlot5 <- renderPlot({
    
    data.grafico5 <- data[DESC_LOCALFISICO=="La Marina" & CLASE=="J03010421 - POLLO ENTERO",]
    data.grafico5 <- data.grafico5[,.(MONTOVENTAS = sum(F_MONTOVENTA)), keyby = .(DESC_SKU)]
    data.grafico5 <- setorder(data.grafico5,-MONTOVENTAS)
    
    ggplot(data.grafico5, aes(x = reorder(DESC_SKU, MONTOVENTAS), y = MONTOVENTAS)) +
      geom_bar(stat = "identity",
               show.legend = FALSE,
               fill= "lightblue",
               color = "white") +
      geom_text(aes(label = dollar(round(MONTOVENTAS, 0)),
                    hjust = 1,
                    vjust = 0.5),
                size = 5)+
      xlab("DESC_SKU") +
      ylab("MONTOVENTAS") +
      coord_flip()+
      scale_y_continuous(labels=scales::dollar_format())
  })
})
