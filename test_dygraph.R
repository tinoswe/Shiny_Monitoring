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


#---

library(reshape)

data <- df_jan16
data_T <- data[, !grepl( "- HR" , names( data ) )]
data_HR <- data[, !grepl( "- T" , names( data ) )]
data_T$dtime <- strptime(paste(data_T$Day, data_T$Time, sep = " "),
                         format="%Y-%m-%d %H:%M:%S")
data_T$Day <- c()
data_T$Time <- c()

colnames(data_T)

d_t_t <- melt(data_T,  
              id = "dtime")

colnames(d_t_t) <- c("T", "LABEL", "Y")
df <- d_t_t
df$T

df

xts(df[df$LABEL == "2 - T",]$Y, 
    strptime(df[df$LABEL == "2 - T",]$T, 
             format = "%Y-%m-%d %H:%M:%S")) %>%
  dygraph()

class(ldeaths)
lungDeaths <- cbind(ldeaths, mdeaths, fdeaths)