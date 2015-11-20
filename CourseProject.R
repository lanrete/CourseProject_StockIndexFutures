trans <- function(x) {
  return(paste0("F:/HW1/",x,".csv"))
}

list <- c(600000,600010,600015,600016,600018,600028,600030,600036,600048,600050,
          600089,600104,600109,600111,600150,600256,600406,600518,600519,600583,
          600585,600637,600690,600837,600887,600893,600958,600999,601006,601088,
          601166,601169,601186,601288,601318,601328,601390,601398,601601,601628,
          601668,601688,601766,601800,601818,601857,601901,601988,601989,601998)
beta <- NULL
risk <- NULL

for (i in 1:50){
    temp <- read.csv(trans(list[i]))
    temp$date = as.POSIXct(temp$date)
    temp <- subset.data.frame(temp, 
                              (date >= as.POSIXct("2015-01-01") &
                               (date <= as.POSIXct("2015-09-30"))),
                              select = c(date,Code,Close,PreClose))
    temp$Rate <- (temp$Close-temp$PreClose) / temp$PreClose 
    cal <- merge(sha000016,temp,by="date")
    fit <- summary(lm(cal$Rate.y~cal$Rate.x))
    beta <- c(beta,fit$coef[2,1])
    risk <- c(risk,fit$r.squared)
}

result <- cbind(beta,risk)
row.names(result) <- as.character(list)
result <- round(result, digits = 5)
