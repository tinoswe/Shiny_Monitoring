library(shiny)
#install.packages("lubridate")
library(lubridate)

df <- read.csv(file="../RH_T/CSV_Files/all_data.csv",
               header=TRUE,
               sep=",",
               as.is=TRUE)
#df$XT2
df$XT2 <- strptime(df$XT2,
                   format="%Y-%m-%d %H:%M:%S")
names(df) <- c("DateTime",
               "MOB 2 - T",
               "MOB 3 - T",
               "MOB 4 - T",
               "MOB 5 - T",
               "MOB 2 - HR",
               "MOB 3 - HR",
               "MOB 4 - HR",
               "MOB 5 - HR")

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

#df_jan16 <- df_jan16[1:100,]
#df_feb16 <- df_feb16[1:100,]

function(input, output) {
  
  datasetInput <- reactive({
    switch(input$dataset,
           "Jan 16" = head(df_jan16, n=15),
           "Feb 16" = head(df_feb16, n=20))
  })
  
  
  
  output$table <- renderTable({
    datasetInput()
  })
  
  # output$downloadData <- downloadHandler(
  #   filename = function() { 
  #     paste(input$dataset, '.csv', sep='') 
  #   },
  #   content = function(file) {
  #     write.csv(datasetInput(), file)
  #   }
  # )
}
