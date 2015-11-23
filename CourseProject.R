setwd("C:/Users/Lanrete/Google Drive/StoIndFut/IndPro1")
options(scipen=999)

cleandata <- function(number){
  temp <- read.csv(paste0(getwd(),"/",number,".csv"))
  temp <- subset.data.frame(temp,select = c("日期","收盘价","前收盘"))
  temp$日期 <- as.POSIXct(temp$日期)
  temp <- subset.data.frame(temp, months(temp$日期)=="十一月" | 
                                  months(temp$日期)=="十月" |
                                  months(temp$日期)=="九月" |
                                  months(temp$日期)=="八月")
  write.csv(temp, file=paste0("./clean",number,".csv"),row.names = FALSE)
}

clacrate <- function(number) {
  temp <- read.csv(paste0(getwd(),"/clean",number,".csv"))
  temp$Rate = (temp$收盘价/temp$前收盘)-1
  write.csv(temp, file=paste0("./clean",number,".csv"),row.names = FALSE)
}

grab <- function(a,b,c) {
  temp <- read.csv(paste0(getwd(),"/clean",a,".csv"))
  ta <- subset.data.frame(temp,select = c("日期","Rate"))
  temp <- read.csv(paste0(getwd(),"/clean",b,".csv"))
  tb <- subset.data.frame(temp,select = c("日期","Rate"))
  temp <- read.csv(paste0(getwd(),"/clean",c,".csv"))
  tc <- subset.data.frame(temp,select = c("日期","Rate"))
  In50 <- read.csv(paste0(getwd(),"/clean000016.csv"))
  In50 <- In50$Rate
  Combine <- ta
  Combine$Rate <- ta$Rate+tb$Rate+tc$Rate
  temp <- cbind(Combine,In50)
  temp$日期 <- as.POSIXct(temp$日期)
  temp <- subset.matrix(temp, months(temp$日期) != "十一月")
  temp
}


list <- c(600000,600010,600015,600016,600018,600028,600030,600036,600048,600050,
          600089,600104,600109,600111,600150,600256,600406,600518,600519,600583,
          600585,600637,600690,600837,600887,600893,600958,600999,601006,601088,
          601166,601169,601186,601288,601318,601328,601390,601398,601601,601628,
          601668,601688,601766,601800,601818,601857,601901,601988,601989,601998,
          000016)
list <- as.character(list)
list[51] <- "000016"

for (i in 1:51) {cleandata(list[i])}
for (i in 1:51) {clacrate(list[i])}

ans <- NULL

for (a in 1:50) { 
  for (b in a:50) { if (a==b) next
    for (c in b:50) { if (b==c | a==c) next
      temp <- grab(list[a],list[b],list[c])
      fit <- lm(temp$Rate ~ temp$In50)
      beta <- summary(fit)$coefficients[2,1]
      r <- summary(fit)$r.squared
      vec <- cbind(a,b,c,beta,r)
      ans <- rbind(ans,vec)
    }
  }
}

ans <- as.data.frame(ans)
ans <- ans[order(ans$beta,decreasing = TRUE),]

new <- NULL

for (i in 1:117600) {
  if (i%%6==0) {new <- rbind(new,ans[i,])}
}

MAX <- 0
mark <- NULL

for (Try in 1:19600) {

beta <- new[Try,]$beta
choose <- as.matrix(new[Try,c(1:3)])
choose <- cbind(choose,51)

clean <- NULL

for (i in 1:4) {
  temp <- read.csv(paste0(getwd(),"/clean",list[choose[i]],".csv"))
  temp$日期 <- as.POSIXct(temp$日期)
  temp <- subset.data.frame(temp,months(temp$日期)=="十一月" )
  clean <- cbind(clean, temp$收盘价, temp$前收盘)
}

temp$日期 <- as.character(temp$日期)

clean <- cbind(temp$日期,clean)
colnames(clean) <- c("日期","股票1收盘","股票1前收盘","股票2收盘","股票2前收盘","股票3收盘","股票3前收盘","上证50收盘","上证50前收盘") 

len <- length(clean[,1])
clean <- as.data.frame(clean)
for (i in 2:9) {clean[,i]<-as.character(clean[,i])}
for (i in 2:9) {clean[,i]<-as.numeric(clean[,i])}
Ft <- clean$上证50收盘 * 300
Ft0 <- clean$上证50前收盘 * 300 
Vt <- (clean$`股票1收盘` + clean$`股票2收盘` + clean$`股票3收盘`)* 10000000
Vt0 <- (clean$`股票1前收盘` + clean$`股票2前收盘` + clean$`股票3前收盘`) * 10000000

N <- beta * Vt / Ft
Diff <- (N/N[1]) - 1 

Vr <- ((clean$`股票1收盘`/clean$`股票1前收盘`)-1+
       (clean$`股票2收盘`/clean$`股票2前收盘`)-1+
       (clean$`股票3收盘`/clean$`股票3前收盘`)-1 ) / 3

Hr <- (Vt-Vt0) / (Vt0) - N[1] * (Ft-Ft0) / (Vt0) 

result <- (var(Vr)-var(Hr))/ var(Vr) 
if (result > MAX) {
  MAX <- result
  mark <- Try
}
}
