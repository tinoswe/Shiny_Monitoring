library(shiny)
#install.packages("lubridate")
library(lubridate)
library(dygraphs)
library(xts)

df <- read.csv(file="../RH_T/CSV_Files/all_data.csv",
               header=TRUE,
               sep=",",
               as.is=TRUE)
#df$XT2
df$XT2 <- strptime(df$XT2,
                   format="%Y-%m-%d %H:%M:%S")
names(df) <- c("DateTime",
               "2 - T",
               "3 - T",
               "4 - T",
               "5 - T",
               "2 - HR",
               "3 - HR",
               "4 - HR",
               "5 - HR")


#year(df$XT2[2000])
#month(df$XT2[2000])
#day(df$XT2[2000])
#hour(df$XT2[2000])
#minute(df$XT2[2000])
#second(df$XT2[2000])
#d_years <- unique(year(df$XT2))
#d_months <- unique(month(df$XT2))
#d_days <- unique(day(df$XT2))
#d_hours <- unique(hour(df$XT2))

#df[, grepl( "- T" , names( df ) )]


#jan16
df_jan16 <- df[month(df$DateTime)=="1",] 
df_jan16$Day <- as.character(date(df_jan16$DateTime))
df_jan16$Time <- paste(sprintf("%02d", hour(df_jan16$DateTime)),
                 sprintf("%02d", minute(df_jan16$DateTime)),
                 sprintf("%02d", second(df_jan16$DateTime)),
                 sep=":")
df_jan16$DateTime <- c()
df_jan16 <- df_jan16[,c(9,10,1,2,3,4,5,6,7,8)]
#df_jan16<-df_jan16[2:10,]
#tail(df_jan16,
#     n=20)
df_jan16 <- na.omit(df_jan16)

#feb16
df_feb16 <- df[month(df$DateTime)=="2",] 
df_feb16$Day <- as.character(date(df_feb16$DateTime))
df_feb16$Time <- paste(sprintf("%02d", hour(df_feb16$DateTime)),
                       sprintf("%02d", minute(df_feb16$DateTime)),
                       sprintf("%02d", second(df_feb16$DateTime)),
                       sep=":")
df_feb16$DateTime <- c()
df_feb16 <- df_feb16[,c(9,10,1,2,3,4,5,6,7,8)]
df_feb16 <- na.omit(df_feb16)

#mar16
df_mar16 <- df[month(df$DateTime)=="3",] 
df_mar16$Day <- as.character(date(df_mar16$DateTime))
df_mar16$Time <- paste(sprintf("%02d", hour(df_mar16$DateTime)),
                       sprintf("%02d", minute(df_mar16$DateTime)),
                       sprintf("%02d", second(df_mar16$DateTime)),
                       sep=":")
df_mar16$DateTime <- c()
df_mar16 <- df_mar16[,c(9,10,1,2,3,4,5,6,7,8)]
df_mar16 <- na.omit(df_mar16)

#apr16
df_apr16 <- df[month(df$DateTime)=="4",] 
df_apr16$Day <- as.character(date(df_apr16$DateTime))
df_apr16$Time <- paste(sprintf("%02d", hour(df_apr16$DateTime)),
                       sprintf("%02d", minute(df_apr16$DateTime)),
                       sprintf("%02d", second(df_apr16$DateTime)),
                       sep=":")
df_apr16$DateTime <- c()
df_apr16 <- df_apr16[,c(9,10,1,2,3,4,5,6,7,8)]
df_apr16 <- na.omit(df_apr16)

function(input, output) {
  
  datasetInput <- reactive({
    
    switch(input$dataset,
           "Jan 16" = df_jan16,
           "Feb 16" = df_feb16,
           "Mar 16" = df_mar16,
           "Apr 16" = df_apr16)
    
  })
  
  output$plot <- renderPlot({

    library(ggplot2)
    library(reshape)

    data <- datasetInput()
    
    data_T <- data[, !grepl( "- HR" , names( data ) )]
    data_HR <- data[, !grepl( "- T" , names( data ) )]

    data_T$dtime <- strptime(paste(data_T$Day, data_T$Time, sep = " "),
                            format="%Y-%m-%d %H:%M:%S")
    
    data_T$Day <- c()
    data_T$Time <- c()

    d_t_t <- melt(data_T,  
                  id = "dtime")

    #tt <- d_t_t[,1]
    #xx <- d_t_t[,3]
    #fact <- factor(d_t_t[,2])
    
    colnames(d_t_t) <- c("tt", "fact", "xx")
    
    ggplot(data = d_t_t, 
           aes(x = tt,
               y = xx)) + geom_line(aes(colour = fact)) + theme_bw()
           

  })
  
  output$table <- renderDataTable( #{
    dataset <- datasetInput(),
    options = list(pageLength = 25,
                   dom  = 'tip'),
    
    
  output$tgraph <- renderDygraph({
    #https://faidherbard.shinyapps.io/joburgdygraph/
    
    library(reshape)
    
    data <- datasetInput()
    data_T <- data[, !grepl( "- HR" , names( data ) )]
    data_HR <- data[, !grepl( "- T" , names( data ) )]
    data_T$dtime <- strptime(paste(data_T$Day, data_T$Time, sep = " "),
                             format="%Y-%m-%d %H:%M:%S")
    data_T$Day <- c()
    data_T$Time <- c()
    
    #d_t_t <- melt(data_T,  
    #              id = "dtime")
    #colnames(d_t_t) <- c("T", "LABEL", "Y")
    
    
    #filtered <- filter(d_t_t,
    #                   Variety == input$productname,
    #                   Count == input$count )
    
    #dygraph(d_t_t, main="") %>%
    #  dySeries(c("T", "LABEL", "Y"), label="" ) 
    
    xts(data_T[,names(data_T)!="dtime"], 
        strptime(data_T$dtime, format = "%Y-%m-%d %H:%M:%S")) %>%
    dygraph() %>%
      dyAxis("y", valueRange = c(10, 30), label="Temp [°C]") %>%
      dyLimit(as.numeric(15), color = "red") %>%
      dyLimit(as.numeric(25), color = "red")
    
    #dygraph(data_T, main = "test") %>%
    #  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2"))
    
    }
  )
    
#  }
)
  
  # output$downloadData <- downloadHandler(
  #   filename = function() { 
  #     paste(input$dataset, '.csv', sep='') 
  #   },
  #   content = function(file) {
  #     write.csv(datasetInput(), file)
  #   }
  # )
}
