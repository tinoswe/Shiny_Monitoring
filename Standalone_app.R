library(shiny)
dat <- data.frame(date=seq.Date(Sys.Date(),by=1,length.out=5),
                  temp = runif(5))

library(lubridate)
df <- read.csv(file="../RH_T/CSV_Files/all_data.csv",
               header=TRUE,
               sep=",",
               as.is=TRUE)
#df$XT2
df$XT2 <- strptime(df$XT2,
                   format="%Y-%m-%d %H:%M:%S")
names(df) <- c("Time",
               "MOB 2 - T",
               "MOB 3 - T",
               "MOB 4 - T",
               "MOB 5 - T",
               "MOB 2 - HR",
               "MOB 3 - HR",
               "MOB 4 - HR",
               "MOB 5 - HR")

ui <- fluidPage(
  fluidRow(column(12,#12 for full page width
                  dataTableOutput('dto')))
)

server <- function(input,output){
  
  output$dto <- renderDataTable(df,
                                options = list(
                                  pageLength = 20))

}

runApp(list(ui=ui,
            server=server))