setwd("C:/Users/Lanrete/Google Drive/StoIndFut/IndPro1")

options(scipen=999)

pre <- "http://quotes.money.163.com/service/chddata.html?code=0"
suff <- "&start=20150323&end=20151120&fields=TCLOSE;HIGH;LOW;TOPEN;LCLOSE;CHG;PCHG;TURNOVER;VOTURNOVER;VATURNOVER;TCAP;MCAP"

list <- c(600000,600010,600015,600016,600018,600028,600030,600036,600048,600050,
          600089,600104,600109,600111,600150,600256,600406,600518,600519,600583,
          600585,600637,600690,600837,600887,600893,600958,600999,601006,601088,
          601166,601169,601186,601288,601318,601328,601390,601398,601601,601628,
          601668,601688,601766,601800,601818,601857,601901,601988,601989,601998)

FileUrl <- paste0(pre,list,suff)
DestiFile <- paste0(list,".csv")

for (i in 1:50) {
  download.file(FileUrl[i], destfile = DestiFile[i])
}

cleandata <- function(number){
    temp <- read.csv(paste0(getwd(),"/",number,".csv"))
    temp <- subset.data.frame(temp,select = c("日期","收盘价","前收盘"))
    temp$日期 <- as.POSIXct(temp$日期)
    temp <- subset.data.frame(temp,months(temp$日期)=="十一月" | months(temp$日期)=="十月")
    write.csv(temp, file=paste0("./clean",number,".csv"),row.names = FALSE)
}

for (i in 1:50) {cleandata(list[i])}
