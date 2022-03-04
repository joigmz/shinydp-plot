# example from http://shiny.rstudio.com/gallery/kmeans-example.html

library(shiny)
library(ggplot2)
library(dplyr)
library(data.table)
library(scales)
library(DT)


data <- fread("/Users/joseizammontt/Desktop/Universidad/master/thesis/codigo/DATA_UNV_2.csv", header=TRUE, stringsAsFactors=FALSE)
data <- data[F_UNIDADESVENTA>0.000001,]
data <- data[,WEEK:=strftime(as.POSIXlt(data$ID_DIAANALISIS, format = "%Y-%d-%m"), "%W")]
data <- data[,.(WEEK,DESC_LOCALFISICO,COD_ZONAL,DIVISION,DEPARTAMENTO,SUBDEPARTAMENTO,CLASE,SUBCLASE,COD_SKU,DESC_SKU,F_UNIDADESVENTA,F_MONTOVENTA,F_COSTOVENTA)]

data_pap <- fread("data/paso_a_paso.csv", header=TRUE, stringsAsFactors=FALSE)

theme_clean <- function() {
  theme_minimal(base_family = "Barlow Semi Condensed") +
    theme(panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(face = "bold", size=24),
          axis.title = element_text(face = "bold", size=20),
          text = element_text(size=20),
          strip.text = element_text(face = "bold", size = rel(0.8), hjust = 0),
          strip.background = element_rect(fill = "grey80", color = NA),
          legend.title = element_text(face = "bold"))
}

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {
  
  output$distPlot <- renderPlot({
    
    data.grafico1 <-data[DESC_LOCALFISICO==input$selectInput2 & CLASE==input$selectInput1,]
    data.grafico1 <-data.grafico1[WEEK>=input$from_date & WEEK<=input$to_date,]
    data.grafico1 <- data.grafico1[,.(MONTOVENTAS = sum(F_MONTOVENTA), DEMANDA = sum(F_UNIDADESVENTA)), keyby = .(WEEK,DESC_LOCALFISICO,CLASE,COD_SKU,DESC_SKU)]
    data.grafico1 <- data.grafico1[,PRECIOPROMEDIO:=data.grafico1$MONTOVENTAS/data.grafico1$DEMANDA]
    data.grafico1 <- data.grafico1[DESC_SKU==input$selectInput3,]
    
    x <- data.grafico1$WEEK
    y1 <- data.grafico1$PRECIOPROMEDIO
    y2 <- data.grafico1$DEMANDA
    
    p <- ggplot(data.grafico1, aes(x = x, group = 1))
    p <- p + geom_line(aes(y = y1*mean(y2)/mean(y1), colour = "Precio"))

        # adding the relative humidity data, transformed to match roughly the range of the temperature
    p <- p + geom_line(aes(y = y2, colour = "Demanda"))
    
    # now adding the secondary axis, following the example in the help file ?scale_y_continuous
    # and, very important, reverting the above transformation
    p <- p + scale_y_continuous(sec.axis = sec_axis(~.*mean(y1)/mean(y2), name = "Precio",labels=scales::dollar_format(prefix="S/ ")))
    
    # modifying colours and theme options
    p <- p + scale_colour_manual(values = c("blue", "red"))
    p <- p + labs(y = "Demanda",
                  x = " Nº Semana",
                  colour = "Curvas")
    p <- p + ggtitle('Variación del precio y demanda por semana')
    p <- p + theme_clean()
    p <- p + theme(legend.position = c(0.85, 0.2))
    p
  })
  
  output$distPlot2 <- renderPlot({
    
    data.grafico2 <-data[DESC_LOCALFISICO==input$selectInput2 & CLASE==input$selectInput1,]
    data.grafico2 <-data.grafico2[WEEK>=input$from_date & WEEK<=input$to_date,]
    data.grafico2 <- data.grafico2[DESC_SKU==input$selectInput3,]
    data.grafico2 <- data.grafico2[,PRECIOVENTA:=data.grafico2$F_MONTOVENTA/data.grafico2$F_UNIDADESVENTA]
    
    p2 <- ggplot(data.grafico2, aes(x=WEEK, y=PRECIOVENTA)) + 
      geom_boxplot()+
      scale_y_continuous(labels=scales::dollar_format(prefix="S/ "))
    p2 <- p2 + labs(y = "Precio Venta",
                  x = " Nº Semana",
                  colour = "Curvas")
    p2 <- p2 + ggtitle('Variación del precio por semana')
    
    p2 <- p2 + theme_clean()
    p2
  })
  
  output$Demand_summary = DT::renderDataTable({
    
    data.grafico2 <-data[DESC_LOCALFISICO==input$selectInput2 & CLASE==input$selectInput1,]
    data.grafico2 <-data.grafico2[WEEK>=input$from_date & WEEK<=input$to_date,]
    data.grafico2 <- data.grafico2[DESC_SKU==input$selectInput3,]
    data.grafico2 <- data.grafico2[,PRECIOVENTA:=data.grafico2$F_MONTOVENTA/data.grafico2$F_UNIDADESVENTA]
    
    df_grafico2 <- data_frame(data.grafico2)
    
    price_summary <- c()
    weeks_list <- sort(unique(df_grafico2$WEEK))
    for (week_number in weeks_list){
      sold_units <-df_grafico2[df_grafico2$WEEK==as.character(week_number),c('F_UNIDADESVENTA')]
      price_summary_aux <- transpose(data.table(do.call(cbind, lapply(sold_units, summary))))
      price_summary <- rbind(price_summary,price_summary_aux)
    }
    
    price_summary <- cbind(weeks_list,price_summary)
    colnames(price_summary) <- c('Week','Min','1st Qu','Median','Mean','3rd Qu','Max')
    price_summary[,c('Min','1st Qu','Median','Mean','3rd Qu','Max')]<- round(price_summary[,c('Min','1st Qu','Median','Mean','3rd Qu','Max')] ,digit=2)
    price_summary
  })

  output$price_summary = DT::renderDataTable({
    
    data.grafico2 <-data[DESC_LOCALFISICO==input$selectInput2 & CLASE==input$selectInput1,]
    data.grafico2 <-data.grafico2[WEEK>=input$from_date & WEEK<=input$to_date,]
    data.grafico2 <- data.grafico2[DESC_SKU==input$selectInput3,]
    data.grafico2 <- data.grafico2[,PRECIOVENTA:=data.grafico2$F_MONTOVENTA/data.grafico2$F_UNIDADESVENTA]
    
    df_grafico2 <- data_frame(data.grafico2)
    
    price_summary <- c()
    weeks_list <- sort(unique(df_grafico2$WEEK))
    for (week_number in weeks_list){
      sold_units <-df_grafico2[df_grafico2$WEEK==as.character(week_number),c('PRECIOVENTA')]
      price_summary_aux <- transpose(data.table(do.call(cbind, lapply(sold_units, summary))))
      price_summary <- rbind(price_summary,price_summary_aux)
    }
    
    price_summary <- cbind(weeks_list,price_summary)
    colnames(price_summary) <- c('Week','Min','1st Qu','Median','Mean','3rd Qu','Max')
    price_summary[,c('Min','1st Qu','Median','Mean','3rd Qu','Max')]<- round(price_summary[,c('Min','1st Qu','Median','Mean','3rd Qu','Max')] ,digit=2)
    price_summary
  })
  output$Zone_Ranking <- renderPlot({
    if (input$qzonal=="Zonal"){
      ranking_zonal <- data[,.(MONTOVENTAS = sum(F_MONTOVENTA)), keyby = .(COD_ZONAL)]
      ranking_zonal <- setorder(ranking_zonal,-MONTOVENTAS)
      
      ggplot(ranking_zonal, aes(x = reorder(COD_ZONAL, MONTOVENTAS), y = MONTOVENTAS)) +
        geom_bar(stat = "identity",
                 show.legend = FALSE,
                 fill= "lightblue",
                 color = "white") +
        geom_text(aes(label = dollar(round(MONTOVENTAS, 0), prefix = "S/ "),
                      hjust = 0,
                      vjust = 0.5),
                  size = 5)+
        xlab("Zona") +
        ylab("Ventas totales") +
        ggtitle('Ranking de ventas por zona')+
        coord_flip()+
        scale_y_continuous(labels=scales::dollar_format(prefix="S/ "), limits = c(0, max(ranking_zonal$MONTOVENTAS)*1.3))+
        theme_clean()
    }
  })
  
  output$distPlot3 <- renderPlot({
    if (input$qzonal=="Zonal"){
    data.grafico3 <- data[COD_ZONAL == input$cod_zonal,]
    data.grafico3 <- data.grafico3[,.(MONTOVENTAS = sum(F_MONTOVENTA)), keyby = .(DESC_LOCALFISICO)]
    data.grafico3 <- setorder(data.grafico3,-MONTOVENTAS)
    
    ggplot(data.grafico3, aes(x = reorder(DESC_LOCALFISICO, MONTOVENTAS), y = MONTOVENTAS)) +
      geom_bar(stat = "identity",
               show.legend = FALSE,
               fill= "lightblue",
               color = "white") +
      geom_text(aes(label = dollar(round(MONTOVENTAS, 0), prefix = "S/ "),
                    hjust = 0,
                    vjust = 0.5),
                size = 5)+
      xlab("Local") +
      ylab("Ventas totales") +
      ggtitle(paste('Detalle ventas zona',input$cod_zonal))+
      coord_flip()+
      scale_y_continuous(labels=scales::dollar_format(prefix="S/ "), limits = c(0, max(data.grafico3$MONTOVENTAS)*1.3))+
      theme_clean()
    } else {
    data.grafico3 <- data[,.(MONTOVENTAS = sum(F_MONTOVENTA)), keyby = .(DESC_LOCALFISICO)]
    data.grafico3 <- setorder(data.grafico3,-MONTOVENTAS)
    
    data.grafico3 <- data.grafico3[1:input$selectInput4,]
    
    ggplot(data.grafico3, aes(x = reorder(DESC_LOCALFISICO, MONTOVENTAS), y = MONTOVENTAS)) +
      geom_bar(stat = "identity",
               show.legend = FALSE,
               fill= "lightblue",
               color = "white") +
      geom_text(aes(label = dollar(round(MONTOVENTAS, 0), prefix = "S/ "),
                    hjust = 0,
                    vjust = 0.5),
                size = 5)+
      xlab("Local") +
      ylab("Ventas totales") +
      ggtitle(paste('Detalle ventas por local'))+
      coord_flip()+
      scale_y_continuous(labels=scales::dollar_format(prefix="S/ "), limits = c(0, max(data.grafico3$MONTOVENTAS)*1.3))+
      theme_clean()
    }
  })
  
  output$distPlot4 <- renderPlot({
    if(input$selectInput5=="DIVISION") { 
      data.grafico4 <- data[,.(MONTOVENTAS = sum(F_MONTOVENTA)), keyby = .(DIVISION)]
      data.grafico4 <- setorder(data.grafico4,-MONTOVENTAS)

      ggplot(data.grafico4, aes(x = reorder(DIVISION, MONTOVENTAS), y = MONTOVENTAS)) +
        geom_bar(stat = "identity",
                 show.legend = FALSE,
                 fill= "lightblue",
                 color = "white") +
        geom_text(aes(label = dollar(round(MONTOVENTAS, 0), prefix = "S/ "),
                      hjust = 0,
                      vjust = 0.5),
                  size = 5)+
        ggtitle(paste('Ventas toales a nivel',input$selectInput5))+
        xlab("DIVISION") +
        ylab("Ventas Totales") +
        coord_flip()+
        scale_y_continuous(labels=scales::dollar_format(prefix="S/ "), limits = c(0, max(data.grafico4$MONTOVENTAS)*1.3))+
        theme_clean()
    }
    else if(input$selectInput5=="DEPARTAMENTO"){
      data.grafico4 <- data[,.(MONTOVENTAS = sum(F_MONTOVENTA)), keyby = .(DEPARTAMENTO)]
      data.grafico4 <- setorder(data.grafico4,-MONTOVENTAS)

      ggplot(data.grafico4, aes(x = reorder(DEPARTAMENTO, MONTOVENTAS), y = MONTOVENTAS)) +
        geom_bar(stat = "identity",
                 show.legend = FALSE,
                 fill= "lightblue",
                 color = "white") +
        geom_text(aes(label = dollar(round(MONTOVENTAS, 0), prefix = "S/ "),
                      hjust = 0,
                      vjust = 0.5),
                  size = 5)+
        ggtitle(paste('Ventas toales a nivel',input$selectInput5))+
        xlab("DEPARTAMENTO") +
        ylab("Ventas Totales") +
        coord_flip()+
        scale_y_continuous(labels=scales::dollar_format(prefix="S/ "), limits = c(0, max(data.grafico4$MONTOVENTAS)*1.3))+
        theme_clean()
    }
    else if(input$selectInput5=="SUBDEPARTAMENTO"){
      data.grafico4 <- data[,.(MONTOVENTAS = sum(F_MONTOVENTA)), keyby = .(SUBDEPARTAMENTO)]
      data.grafico4 <- setorder(data.grafico4,-MONTOVENTAS)

      ggplot(data.grafico4, aes(x = reorder(SUBDEPARTAMENTO, MONTOVENTAS), y = MONTOVENTAS)) +
        geom_bar(stat = "identity",
                 show.legend = FALSE,
                 fill= "lightblue",
                 color = "white") +
        geom_text(aes(label = dollar(round(MONTOVENTAS, 0), prefix = "S/ "),
                      hjust = 0,
                      vjust = 0.5),
                  size = 5)+
        ggtitle(paste('Ventas toales a nivel',input$selectInput5))+
        xlab("SUBDEPARTAMENTO") +
        ylab("Ventas Totales") +
        coord_flip()+
        scale_y_continuous(labels=scales::dollar_format(prefix="S/ "), limits = c(0, max(data.grafico4$MONTOVENTAS)*1.3))+
        theme_clean()
    }
    else if(input$selectInput5=="CLASE"){
      data.grafico4 <- data[,.(MONTOVENTAS = sum(F_MONTOVENTA)), keyby = .(CLASE)]
      data.grafico4 <- setorder(data.grafico4,-MONTOVENTAS)
      data.grafico4 <- data.grafico4[1:input$selectInput6,]
      
      ggplot(data.grafico4, aes(x = reorder(CLASE, MONTOVENTAS), y = MONTOVENTAS)) +
        geom_bar(stat = "identity",
                 show.legend = FALSE,
                 fill= "lightblue",
                 color = "white") +
        geom_text(aes(label = dollar(round(MONTOVENTAS, 0), prefix = "S/ "),
                      hjust = 0,
                      vjust = 0.5),
                  size = 5)+
        ggtitle(paste('Ventas toales a nivel',input$selectInput5))+
        xlab("CLASE") +
        ylab("Ventas Totales") +
        coord_flip()+
        scale_y_continuous(labels=scales::dollar_format(prefix="S/ "), limits = c(0, max(data.grafico4$MONTOVENTAS)*1.3))+
        theme_clean()
    }
    else if(input$selectInput5=="SUBCLASE"){
      data.grafico4 <- data[,.(MONTOVENTAS = sum(F_MONTOVENTA)), keyby = .(SUBCLASE)]
      data.grafico4 <- setorder(data.grafico4,-MONTOVENTAS)
      data.grafico4 <- data.grafico4[1:input$selectInput6,]
      
      ggplot(data.grafico4, aes(x = reorder(SUBCLASE, MONTOVENTAS), y = MONTOVENTAS)) +
        geom_bar(stat = "identity",
                 show.legend = FALSE,
                 fill= "lightblue",
                 color = "white") +
        geom_text(aes(label = dollar(round(MONTOVENTAS, 0), prefix = "S/ "),
                      hjust = 0,
                      vjust = 0.5),
                  size = 5)+
        ggtitle(paste('Ventas toales a nivel',input$selectInput5))+
        xlab("SUBCLASE") +
        ylab("Ventas Totales") +
        coord_flip()+
        scale_y_continuous(labels=scales::dollar_format(prefix="S/ "), limits = c(0, max(data.grafico4$MONTOVENTAS)*1.3))+
        theme_clean()
    }
  })
  
  output$distPlot5 <- renderPlot({
    
    data.grafico5 <- data[DESC_LOCALFISICO==input$selectInput7 & CLASE==input$selectInput8,]
    data.grafico5 <- data.grafico5[,.(MONTOVENTAS = sum(F_MONTOVENTA)), keyby = .(DESC_SKU)]
    data.grafico5 <- setorder(data.grafico5,-MONTOVENTAS)
    
    ggplot(data.grafico5, aes(x = reorder(DESC_SKU, MONTOVENTAS), y = MONTOVENTAS)) +
      geom_bar(stat = "identity",
               show.legend = FALSE,
               fill= "lightblue",
               color = "white") +
      geom_text(aes(label = dollar(round(MONTOVENTAS, 0), prefix = "S/ "),
                    hjust = 0,
                    vjust = 0.5),
                size = 5)+
      xlab("DESC_SKU") +
      ylab("MONTOVENTAS") +
      coord_flip()+
      scale_y_continuous(labels=scales::dollar_format(prefix="S/ "), limits = c(0, max(data.grafico5$MONTOVENTAS)*1.3))+
      theme_clean()
  })
  
  output$cuarentena_opciones = DT::renderDataTable({
    data_pap[codigo_region==input$cod_region,c(1:5)]
  })
  
  output$lineplot_pap <- renderPlot({
    
    aux <-  data_pap[codigo_region==as.integer(input$cod_region2)& 
                       region_residencia==as.character(input$region_residencia) &
                       codigo_comuna==as.integer(input$codigo_comuna) &
                       comuna_residencia==as.character(input$comuna_residencia) &
                       zona==as.character(input$zona),c(6:584)]
    
    x <- as.Date(colnames(data_pap[codigo_region==input$cod_region2,c(6:584)]))
    y <- t(aux[1,])[,1]
    
    df = data.frame(x,y)
    p <- ggplot(df, aes(x = x, group = 1))
    p <- p+scale_x_date(date_labels = "%Y %b %d")
    p <- p + geom_line(aes(y = y, colour = "Etapa"))
    

    # modifying colours and theme options
    p <- p + scale_colour_manual(values = c("blue", "red"))
    p <- p + labs(y = "Etapa",
                  x = " Fecha",
                  colour = "Curva")
    str <- paste('Variación de plan paso a paso por Fecha para comuna de',input$comuna_residencia,sep = " ")
    p <- p + ggtitle(str)
    p <- p + theme_clean()
    p <- p + theme(legend.position = c(0.85, 0.2))
    p
  })
  
})


