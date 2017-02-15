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

#df_jan16 <- df_jan16[1:100,]
#df_feb16 <- df_feb16[1:100,]

function(input, output) {
  
  datasetInput <- reactive({
    
    switch(input$dataset,
           "Jan 16" = df_jan16,
           "Feb 16" = df_feb16,
           "Mar 16" = df_mar16)
    
  })
  
  output$plot <- renderPlot({

    library(ggplot2)
    library(reshape)
    library(dygraphs)
    
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
    
    
  output$dygraph <- renderDygraph({
    
    data <- datasetInput()
    filtered <- filter(data,
                       Variety == input$productname,
                       Count == input$count )
    
    #dygraph(datasetInput(), main="") %>%
    #  dySeries(c("lwr", "fit", "upr"), label="" ) 
    
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
