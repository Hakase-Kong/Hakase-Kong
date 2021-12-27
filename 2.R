install.packages("lubridate")
install.packages("tidyr")
install.packages("xts")
install.packages("PerformanceAnalytics")
install.packages("psych")
install.packages("lmtest")
install.packages("rmarkdown")
install.packages("sandwich")
install.packages("quantmod")

rm(list=ls())
setwd("C:/Users/WiTHUS/OneDrive - 성균관대학교/바탕 화면/Coding Practice/Quant Portfolio")

library(lubridate)
library(tidyr)
library(xts)
library(PerformanceAnalytics)
library(psych)
library(lmtest)
library(rmarkdown)
library(sandwich)
library(quantmod)



fun5 <- read.csv("fundamental5.csv")
fun5$cusip <- substr(fun5$cusip,1,8)
prc5 <- read.csv("price5.csv")

fun5$intersect = ifelse(fun5$cusip %in% prc5$CUSIP,'yes','no')
fun5 <- fun5[!(fun5$intersect == "no"),]

prc5$intersect = ifelse(prc5$CUSIP %in% fun5$cusip, 'yes','no')
prc5 <- prc5[!(prc5$intersect == "no"),]



fun10 <- read.csv("fundamental10.csv")
fun10$cusip <- substr(fun10$cusip,1,8)
prc10 <- read.csv("price10.csv")

fun10$intersect = ifelse(fun10$cusip %in% prc10$CUSIP,'yes','no')
fun10 <- fun10[!(fun10$intersect == "no"),]

prc10$intersect = ifelse(prc10$CUSIP %in% fun10$cusip, 'yes','no')
prc10 <- prc10[!(prc10$intersect == "no"),]



fun20 <- read.csv("fundamental20.csv")
fun20$cusip <- substr(fun20$cusip,1,8)
prc20 <- read.csv("price20.csv")

fun20$intersect = ifelse(fun20$cusip %in% prc20$CUSIP,'yes','no')
fun20 <- fun20[!(fun20$intersect == "no"),]

prc20$intersect = ifelse(prc20$CUSIP %in% fun20$cusip, 'yes','no')
prc20 <- prc20[!(prc20$intersect == "no"),]

#Calculating Returns and Market Capitalization
ret5 <- as.data.frame(cbind(prc5$date,prc5$CUSIP,prc5$ALTPRC))
names(ret5) <- c("Date","CUSIP","ALTPRC")
ret5$Date <- as.Date(as.character(prc5$date), format = "%Y%m%d")
ret5$ALTPRC <- as.numeric(ret5$ALTPRC)
ret5 <- ret5 %>% spread(CUSIP,ALTPRC) 
ret5 <- ret5 %>% xts(x = ret5[,-1], order.by = ret5$Date)
ret5 <- Return.calculate(ret5)

prc5$mktcap <- abs(prc5$ALTPRC) * prc5$SHROUT
mktcap5 <- as.data.frame(cbind(prc5$date, prc5$CUSIP, prc5$mktcap))
names(mktcap5) <- c("DATE", "CUSIP", "mktcap")
mktcap5$DATE <- as.Date(as.character(mktcap5$DATE), format = '%Y%m%d')
mktcap5$mktcap <- as.numeric(mktcap5$mktcap)
mktcap5 <- mktcap5 %>% spread(CUSIP, mktcap)
mktcap5 <- xts(x = mktcap5[, -1], order.by = mktcap5$DATE)



ret10 <- as.data.frame(cbind(prc10$date,prc10$CUSIP,prc10$ALTPRC))
names(ret10) <- c("Date","CUSIP","ALTPRC")
ret10$Date <- as.Date(as.character(prc10$date), format = "%Y%m%d")
ret10$ALTPRC <- as.numeric(ret10$ALTPRC)
ret10 <- ret10 %>% spread(CUSIP,ALTPRC) 
ret10 <- ret10 %>% xts(x = ret10[,-1], order.by = ret10$Date)
ret10 <- Return.calculate(ret10)

prc10$mktcap <- abs(prc10$ALTPRC) * prc10$SHROUT
mktcap10 <- as.data.frame(cbind(prc10$date, prc10$CUSIP, prc10$mktcap))
names(mktcap10) <- c("DATE", "CUSIP", "mktcap")
mktcap10$DATE <- as.Date(as.character(mktcap10$DATE), format = '%Y%m%d')
mktcap10$mktcap <- as.numeric(mktcap10$mktcap)
mktcap10 <- mktcap10 %>% spread(CUSIP, mktcap)
mktcap10 <- xts(x = mktcap10[, -1], order.by = mktcap10$DATE)



ret20 <- as.data.frame(cbind(prc20$date,prc20$CUSIP,prc20$ALTPRC))
names(ret20) <- c("Date","CUSIP","ALTPRC")
ret20$Date <- as.Date(as.character(prc20$date), format = "%Y%m%d")
ret20$ALTPRC <- as.numeric(ret20$ALTPRC)
ret20 <- ret20 %>% spread(CUSIP,ALTPRC) 
ret20 <- ret20 %>% xts(x = ret20[,-1], order.by = ret20$Date)
ret20 <- Return.calculate(ret20)

prc20$mktcap <- abs(prc20$ALTPRC) * prc20$SHROUT
mktcap20 <- as.data.frame(cbind(prc20$date, prc20$CUSIP, prc20$mktcap))
names(mktcap20) <- c("DATE", "CUSIP", "mktcap")
mktcap20$DATE <- as.Date(as.character(mktcap20$DATE), format = '%Y%m%d')
mktcap20$mktcap <- as.numeric(mktcap20$mktcap)
mktcap20 <- mktcap20 %>% spread(CUSIP, mktcap)
mktcap20 <- xts(x = mktcap20[, -1], order.by = mktcap20$DATE)

#Ranking System Using Fundamental Value
NCAV5 <- as.data.frame(cbind(fun5$datadate,fun5$cusip,fun5$act-fun5$lt))
names(NCAV5) <- c("Date","CUSIP","NCAV")
NCAV5$Date <- as.Date(as.character(NCAV5$Date), format = "%Y%m%d")
NCAV5$NCAV <- as.numeric(NCAV5$NCAV)
NCAV5 <- NCAV5[!duplicated(NCAV5[,c("Date","CUSIP")]),]
NCAV5 <- NCAV5 %>% spread(CUSIP,NCAV)
NCAV5 <- NCAV5 %>% xts(x = NCAV5[,-1], order.by = NCAV5$Date)
NCAV5 <- NCAV5[endpoints(NCAV5, on = 'years'),]
NCAV5 <- NCAV5[-7,]
NCAV5 <- NCAV5[-1,]

NetIc5 <- as.data.frame(cbind(fun5$datadate,fun5$cusip,fun5$ni))
names(NetIc5) <- c("Date","CUSIP","Net Income")
NetIc5$Date <- as.Date(as.character(NetIc5$Date), format = "%Y%m%d")
NetIc5$`Net Income` <- as.numeric(NetIc5$`Net Income`)
NetIc5 <- NetIc5[!duplicated(NetIc5[,c("Date","CUSIP")]),]
NetIc5 <- NetIc5 %>% spread(CUSIP,`Net Income`)
NetIc5 <- NetIc5 %>% xts(x = NetIc5[,-1], order.by = NetIc5$Date)
NetIc5 <- NetIc5[endpoints(NetIc5, on = 'years'),]
NetIc5 <- NetIc5[-7,]
NetIc5 <- NetIc5[-1,]

GPA5 <- as.data.frame(cbind(fun5$datadate,fun5$cusip,(fun5$revt-fun5$cogs)/fun5$at))
names(GPA5) <- c("Date","CUSIP","GP/A")
GPA5$Date <- as.Date(as.character(GPA5$Date), format = "%Y%m%d")
GPA5$`GP/A` <- as.numeric(GPA5$`GP/A`)
GPA5 <- GPA5[!duplicated(GPA5[,c("Date","CUSIP")]),]
GPA5 <- GPA5 %>% spread(CUSIP,`GP/A`)
GPA5 <- GPA5 %>% xts(x = GPA5[,-1], order.by = GPA5$Date)
GPA5 <- GPA5[endpoints(GPA5, on = 'years'),]
GPA5 <- GPA5[-7,]
GPA5 <- GPA5[-1,]

Date5 <- as.data.frame(fun5$datadate)
names(Date5) <- "Date"
Date5$Date <- as.Date(as.character(Date5$Date), format = "%Y%m%d")
Date5 <- Date5[!duplicated(Date5$Date),]
Date5 <- as.data.frame(Date5)
Date5 <- Date5 %>% xts(x = Date5, order.by = Date5$Date)
Date5 <- Date5[.indexmon(Date5) == 11]
Date5 <- Date5[-1,]


NCAV10 <- as.data.frame(cbind(fun10$datadate,fun10$cusip,fun10$act-fun10$lt))
names(NCAV10) <- c("Date","CUSIP","NCAV")
NCAV10$Date <- as.Date(as.character(NCAV10$Date), format = "%Y%m%d")
NCAV10$NCAV <- as.numeric(NCAV10$NCAV)
NCAV10 <- NCAV10[!duplicated(NCAV10[,c("Date","CUSIP")]),]
NCAV10 <- NCAV10 %>% spread(CUSIP,NCAV)
NCAV10 <- NCAV10 %>% xts(x = NCAV10[,-1], order.by = NCAV10$Date)
NCAV10 <- NCAV10[endpoints(NCAV10, on = 'years'),]
NCAV10 <- NCAV10[-12,]

NetIc10 <- as.data.frame(cbind(fun10$datadate,fun10$cusip,fun10$ni))
names(NetIc10) <- c("Date","CUSIP","Net Income")
NetIc10$Date <- as.Date(as.character(NetIc10$Date), format = "%Y%m%d")
NetIc10$`Net Income` <- as.numeric(NetIc10$`Net Income`)
NetIc10 <- NetIc10[!duplicated(NetIc10[,c("Date","CUSIP")]),]
NetIc10 <- NetIc10 %>% spread(CUSIP,`Net Income`)
NetIc10 <- NetIc10 %>% xts(x = NetIc10[,-1], order.by = NetIc10$Date)
NetIc10 <- NetIc10[endpoints(NetIc10, on = 'years'),]
NetIc10 <- NetIc10[-12,]

GPA10 <- as.data.frame(cbind(fun10$datadate,fun10$cusip,(fun10$revt-fun10$cogs)/fun10$at))
names(GPA10) <- c("Date","CUSIP","GP/A")
GPA10$Date <- as.Date(as.character(GPA10$Date), format = "%Y%m%d")
GPA10$`GP/A` <- as.numeric(GPA10$`GP/A`)
GPA10 <- GPA10[!duplicated(GPA10[,c("Date","CUSIP")]),]
GPA10 <- GPA10 %>% spread(CUSIP,`GP/A`)
GPA10 <- GPA10 %>% xts(x = GPA10[,-1], order.by = GPA10$Date)
GPA10 <- GPA10[endpoints(GPA10, on = 'years'),]
GPA10 <- GPA10[-12,]

Date10 <- as.data.frame(fun10$datadate)
names(Date10) <- "Date"
Date10$Date <- as.Date(as.character(Date10$Date), format = "%Y%m%d")
Date10 <- Date10[!duplicated(Date10$Date),]
Date10 <- as.data.frame(Date10)
Date10 <- Date10 %>% xts(x = Date10, order.by = Date10$Date)
Date10 <- Date10[.indexmon(Date10) == 11]


NCAV20 <- as.data.frame(cbind(fun20$datadate,fun20$cusip,fun20$act-fun20$lt))
names(NCAV20) <- c("Date","CUSIP","NCAV")
NCAV20$Date <- as.Date(as.character(NCAV20$Date), format = "%Y%m%d")
NCAV20$NCAV <- as.numeric(NCAV20$NCAV)
NCAV20 <- NCAV20[!duplicated(NCAV20[,c("Date","CUSIP")]),]
NCAV20 <- NCAV20 %>% spread(CUSIP,NCAV)
NCAV20 <- NCAV20 %>% xts(x = NCAV20[,-1], order.by = NCAV20$Date)
NCAV20 <- NCAV20[endpoints(NCAV20, on = 'years'),]
NCAV20 <- NCAV20[-22,]

NetIc20 <- as.data.frame(cbind(fun20$datadate,fun20$cusip,fun20$ni))
names(NetIc20) <- c("Date","CUSIP","Net Income")
NetIc20$Date <- as.Date(as.character(NetIc20$Date), format = "%Y%m%d")
NetIc20$`Net Income` <- as.numeric(NetIc20$`Net Income`)
NetIc20 <- NetIc20[!duplicated(NetIc20[,c("Date","CUSIP")]),]
NetIc20 <- NetIc20 %>% spread(CUSIP,`Net Income`)
NetIc20 <- NetIc20 %>% xts(x = NetIc20[,-1], order.by = NetIc20$Date)
NetIc20 <- NetIc20[endpoints(NetIc20, on = 'years'),]
NetIc20 <- NetIc20[-22,]

GPA20 <- as.data.frame(cbind(fun20$datadate,fun20$cusip,(fun20$revt-fun20$cogs)/fun20$at))
names(GPA20) <- c("Date","CUSIP","GP/A")
GPA20$Date <- as.Date(as.character(GPA20$Date), format = "%Y%m%d")
GPA20$`GP/A` <- as.numeric(GPA20$`GP/A`)
GPA20 <- GPA20[!duplicated(GPA20[,c("Date","CUSIP")]),]
GPA20 <- GPA20 %>% spread(CUSIP,`GP/A`)
GPA20 <- GPA20 %>% xts(x = GPA20[,-1], order.by = GPA20$Date)
GPA20 <- GPA20[endpoints(GPA20, on = 'years'),]
GPA20 <- GPA20[-22,]

Date20 <- as.data.frame(fun20$datadate)
names(Date20) <- "Date"
Date20$Date <- as.Date(as.character(Date20$Date), format = "%Y%m%d")
Date20 <- Date20[!duplicated(Date20$Date),]
Date20 <- as.data.frame(Date20)
Date20 <- Date %>% xts(x = Date20, order.by = Date20$Date)
Date20 <- Date20[.indexmon(Date20) == 11]


#For market capitalization, subset data of June and December (notice: jan=0, feb=1, mar=2, ... , dec=11)
mktcap5_jun <- mktcap5[.indexmon(mktcap5) == 5]
mktcap5_dec <- mktcap5[.indexmon(mktcap5) == 11]

NCAV5_r <- as.data.frame(coredata(NCAV5)/coredata(mktcap5_dec))
NCAV5_r <- NCAV5_r %>% xts(x = NCAV5_r, order.by = index(Date5))

NetIc5_r <- as.data.frame(coredata(NetIc5)/coredata(mktcap5_dec))
NetIc5_r <- NetIc5_r %>% xts(x = NetIc5_r, order.by = index(Date5))



mktcap10_jun <- mktcap10[.indexmon(mktcap10) == 5]
mktcap10_dec <- mktcap10[.indexmon(mktcap10) == 11]

NCAV10_r <- as.data.frame(coredata(NCAV10)/coredata(mktcap10_dec))
NCAV10_r <- NCAV10_r %>% xts(x = NCAV10_r, order.by = index(Date10))

NetIc10_r <- as.data.frame(coredata(NetIc10)/coredata(mktcap10_dec))
NetIc10_r <- NetIc10_r %>% xts(x = NetIc10_r, order.by = index(Date10))



mktcap20_jun <- mktcap20[.indexmon(mktcap20) == 5]
mktcap20_dec <- mktcap20[.indexmon(mktcap20) == 11]

NCAV20_r <- as.data.frame(coredata(NCAV20)/coredata(mktcap20_dec))
NCAV20_r <- NCAV20_r %>% xts(x = NCAV20_r, order.by = index(Date20))

NetIc20_r <- as.data.frame(coredata(NetIc20)/coredata(mktcap20_dec))
NetIc20_r <- NetIc20_r %>% xts(x = NetIc20_r, order.by = index(Date20))


#Cross-sectionally winsorize monthly return in 0.5% level
ret5 <- xts(x = t(winsor(t(ret5), trim = 0.005, na.rm =TRUE)), order.by = index(ret5))
ret10 <- xts(x = t(winsor(t(ret10), trim = 0.005, na.rm =TRUE)), order.by = index(ret10))
ret20 <- xts(x = t(winsor(t(ret20), trim = 0.005, na.rm =TRUE)), order.by = index(ret20))

#Take negative value to be NA
#NCAV <- as.data.frame(ifelse(NCAV < 0, NA, NCAV))
#NetIc <- as.data.frame(ifelse(NetIc < 0, NA, NetIc))
#GPA <- as.data.frame(ifelse(GPA < 0, NA, GPA))

#Align the calculated BM with the last trading day of next June
NCAV5 <- xts(x = NCAV5, order.by = index(mktcap5_jun))
NetIc5 <- xts(x = NetIc5, order.by = index(mktcap5_jun))
GPA5 <- xts(x = GPA5, order.by = index(mktcap5_jun))


NCAV10 <- xts(x = NCAV10, order.by = index(mktcap10_jun))
NetIc10 <- xts(x = NetIc10, order.by = index(mktcap10_jun))
GPA10 <- xts(x = GPA10, order.by = index(mktcap10_jun))


NCAV20 <- xts(x = NCAV20, order.by = index(mktcap20_jun))
NetIc20 <- xts(x = NetIc20, order.by = index(mktcap20_jun))
GPA20 <- xts(x = GPA20, order.by = index(mktcap20_jun))


#Calculate monthly breakpoints by prior 12-1 month return: Top 10% & Bottom 10%
NCAV5_q <- as.data.frame(1:(length(endpoints(NCAV5_r, on = 'months'))-1))
for (i in 1:(length(endpoints(NCAV5_r, on = 'months'))-1)){ 
  NCAV5_q[i,1] <- quantile(NCAV5_r[i,], na.rm = TRUE, 0.1)
  NCAV5_q[i,2] <- quantile(NCAV5_r[i,], na.rm = TRUE, 0.9)}
NCAV5_q <- cbind(index(NCAV5_r),NCAV5_q)
names(NCAV5_q) <- c("Date","Q1","Q2")

NetIc5_q <- as.data.frame(1:(length(endpoints(NetIc5_r, on = 'months'))-1))
for (i in 1:(length(endpoints(NetIc5_r, on = 'months'))-1)){ 
  NetIc5_q[i,1] <- quantile(NetIc5_r[i,], na.rm = TRUE, 0.1)
  NetIc5_q[i,2] <- quantile(NetIc5_r[i,], na.rm = TRUE, 0.9)}
NetIc5_q <- cbind(index(NetIc5_r),NetIc5_q)
names(NetIc5_q) <- c("Date","Q1","Q2")

GPA5_q <- as.data.frame(1:(length(endpoints(GPA5, on = 'months'))-1))
for (i in 1:(length(endpoints(GPA5, on = 'months'))-1)){ 
  GPA5_q[i,1] <- quantile(GPA5[i,], na.rm = TRUE, 0.1)
  GPA5_q[i,2] <- quantile(GPA5[i,], na.rm = TRUE, 0.9)}
GPA5_q <- cbind(index(GPA5),GPA5_q)
names(GPA5_q) <- c("Date","Q1","Q2")


NCAV10_q <- as.data.frame(1:(length(endpoints(NCAV10_r, on = 'months'))-1))
for (i in 1:(length(endpoints(NCAV10_r, on = 'months'))-1)){ 
  NCAV10_q[i,1] <- quantile(NCAV10_r[i,], na.rm = TRUE, 0.1)
  NCAV10_q[i,2] <- quantile(NCAV10_r[i,], na.rm = TRUE, 0.9)}
NCAV10_q <- cbind(index(NCAV10_r),NCAV10_q)
names(NCAV10_q) <- c("Date","Q1","Q2")

NetIc10_q <- as.data.frame(1:(length(endpoints(NetIc10_r, on = 'months'))-1))
for (i in 1:(length(endpoints(NetIc10_r, on = 'months'))-1)){ 
  NetIc10_q[i,1] <- quantile(NetIc10_r[i,], na.rm = TRUE, 0.1)
  NetIc10_q[i,2] <- quantile(NetIc10_r[i,], na.rm = TRUE, 0.9)}
NetIc10_q <- cbind(index(NetIc10_r),NetIc10_q)
names(NetIc10_q) <- c("Date","Q1","Q2")

GPA10_q <- as.data.frame(1:(length(endpoints(GPA10, on = 'months'))-1))
for (i in 1:(length(endpoints(GPA10, on = 'months'))-1)){ 
  GPA10_q[i,1] <- quantile(GPA10[i,], na.rm = TRUE, 0.1)
  GPA10_q[i,2] <- quantile(GPA10[i,], na.rm = TRUE, 0.9)}
GPA10_q <- cbind(index(GPA10),GPA10_q)
names(GPA10_q) <- c("Date","Q1","Q2")


NCAV20_q <- as.data.frame(1:(length(endpoints(NCAV20_r, on = 'months'))-1))
for (i in 1:(length(endpoints(NCAV20_r, on = 'months'))-1)){ 
  NCAV20_q[i,1] <- quantile(NCAV20_r[i,], na.rm = TRUE, 0.1)
  NCAV20_q[i,2] <- quantile(NCAV20_r[i,], na.rm = TRUE, 0.9)}
NCAV20_q <- cbind(index(NCAV20_r),NCAV20_q)
names(NCAV20_q) <- c("Date","Q1","Q2")

NetIc20_q <- as.data.frame(1:(length(endpoints(NetIc20_r, on = 'months'))-1))
for (i in 1:(length(endpoints(NetIc20_r, on = 'months'))-1)){ 
  NetIc20_q[i,1] <- quantile(NetIc20_r[i,], na.rm = TRUE, 0.1)
  NetIc20_q[i,2] <- quantile(NetIc20_r[i,], na.rm = TRUE, 0.9)}
NetIc20_q <- cbind(index(NetIc20_r),NetIc20_q)
names(NetIc20_q) <- c("Date","Q1","Q2")

GPA20_q <- as.data.frame(1:(length(endpoints(GPA20, on = 'months'))-1))
for (i in 1:(length(endpoints(GPA20, on = 'months'))-1)){ 
  GPA20_q[i,1] <- quantile(GPA20[i,], na.rm = TRUE, 0.1)
  GPA20_q[i,2] <- quantile(GPA20[i,], na.rm = TRUE, 0.9)}
GPA20_q <- cbind(index(GPA20),GPA20_q)
names(GPA20_q) <- c("Date","Q1","Q2")


#Calculate monthly breakpoints by Market Capitalization: Median
mktcap5_q <- as.data.frame(1:(length(endpoints(mktcap5, on = 'months'))-1))
for (i in 1:(length(endpoints(mktcap5, on = 'months'))-1)){ 
  mktcap5_q[i,1] <- quantile(mktcap5[i,], na.rm = TRUE, 0.5)}
mktcap5_q <- cbind(index(mktcap5),mktcap5_q)
names(mktcap5_q) <- c("Date","Q1")


mktcap10_q <- as.data.frame(1:(length(endpoints(mktcap10, on = 'months'))-1))
for (i in 1:(length(endpoints(mktcap10, on = 'months'))-1)){ 
  mktcap10_q[i,1] <- quantile(mktcap10[i,], na.rm = TRUE, 0.5)}
mktcap10_q <- cbind(index(mktcap10),mktcap10_q)
names(mktcap10_q) <- c("Date","Q1")


mktcap20_q <- as.data.frame(1:(length(endpoints(mktcap20, on = 'months'))-1))
for (i in 1:(length(endpoints(mktcap20, on = 'months'))-1)){ 
  mktcap20_q[i,1] <- quantile(mktcap20[i,], na.rm = TRUE, 0.5)}
mktcap20_q <- cbind(index(mktcap20),mktcap20_q)
names(mktcap20_q) <- c("Date","Q1")


#To calculate timeseries weight for each asset, distribute 1 for corresponding conditions: size, NCAV, Net Income, GP/A

high5_1 <- as.data.frame(ifelse(coredata(NCAV5) >= NCAV5_q$Q2, 1, NA))
low5_1 <- as.data.frame(ifelse(coredata(NCAV5) <= NCAV5_q$Q1, 1, NA))

high5_2 <- as.data.frame(ifelse(coredata(NetIc5) >= NetIc5_q$Q2, 1, NA))
low5_2 <- as.data.frame(ifelse(coredata(NetIc5) <= NetIc5_q$Q1, 1, NA))

high5_3 <- as.data.frame(ifelse(coredata(GPA5) >= GPA5_q$Q2, 1, NA))
low5_3 <- as.data.frame(ifelse(coredata(GPA5) <= GPA5_q$Q1, 1, NA))


high10_1 <- as.data.frame(ifelse(coredata(NCAV10) >= NCAV10_q$Q2, 1, NA))
low10_1 <- as.data.frame(ifelse(coredata(NCAV10) <= NCAV10_q$Q1, 1, NA))

high10_2 <- as.data.frame(ifelse(coredata(NetIc10) >= NetIc10_q$Q2, 1, NA))
low10_2 <- as.data.frame(ifelse(coredata(NetIc10) <= NetIc10_q$Q1, 1, NA))

high10_3 <- as.data.frame(ifelse(coredata(GPA10) >= GPA10_q$Q2, 1, NA))
low10_3 <- as.data.frame(ifelse(coredata(GPA10) <= GPA10_q$Q1, 1, NA))


high20_1 <- as.data.frame(ifelse(coredata(NCAV20) >= NCAV20_q$Q2, 1, NA))
low20_1 <- as.data.frame(ifelse(coredata(NCAV20) <= NCAV20_q$Q1, 1, NA))

high20_2 <- as.data.frame(ifelse(coredata(NetIc20) >= NetIc20_q$Q2, 1, NA))
low20_2 <- as.data.frame(ifelse(coredata(NetIc20) <= NetIc20_q$Q1, 1, NA))

high20_3 <- as.data.frame(ifelse(coredata(GPA20) >= GPA20_q$Q2, 1, NA))
low20_3 <- as.data.frame(ifelse(coredata(GPA20) <= GPA20_q$Q1, 1, NA))


hh12_5 <- high5_1*high5_2
lh12_5 <- low5_1*high5_2
hl12_5 <- high5_1*low5_2
ll12_5 <- low5_1*low5_2

hh13_5 <- high5_1*high5_3
lh13_5 <- low5_1*high5_3
hl13_5 <- high5_1*high5_3
ll13_5 <- low5_1*low5_3

hh23_5 <- high5_2*high5_3
lh23_5 <- low5_2*high5_3
hl23_5 <- high5_2*low5_3
ll23_5 <- low5_2*low5_3


hh12_10 <- high10_1*high10_2
lh12_10 <- low10_1*high10_2
hl12_10 <- high10_1*low10_2
ll12_10 <- low10_1*low10_2

hh13_10 <- high10_1*high10_3
lh13_10 <- low10_1*high10_3
hl13_10 <- high10_1*high10_3
ll13_10 <- low10_1*low10_3

hh23_10 <- high10_2*high10_3
lh23_10 <- low10_2*high10_3
hl23_10 <- high10_2*low10_3
ll23_10 <- low10_2*low10_3


hh12_20 <- high20_1*high20_2
lh12_20 <- low20_1*high20_2
hl12_20 <- high20_1*low20_2
ll12_20 <- low20_1*low20_2

hh13_20 <- high20_1*high20_3
lh13_20 <- low20_1*high20_3
hl13_20 <- high20_1*high20_3
ll13_20 <- low20_1*low20_3

hh23_20 <- high20_2*high20_3
lh23_20 <- low20_2*high20_3
hl23_20 <- high20_2*low20_3
ll23_20 <- low20_2*low20_3


hhh_5 <- high5_1*high5_2*high5_3
lhh_5 <- low5_1*high5_2*high5_3
hlh_5 <- high5_1*low5_2*high5_3
hhl_5 <- high5_1*high5_2*low5_3
llh_5 <- low5_1*low5_2*high5_3
lhl_5 <- low5_1*high5_2*low5_3
hll_5 <- high5_1*low5_2*low5_3
lll_5 <- low5_1*low5_2*low5_3


hhh_10 <- high10_1*high10_2*high10_3
lhh_10 <- low10_1*high10_2*high10_3
hlh_10 <- high10_1*low10_2*high10_3
hhl_10 <- high10_1*high10_2*low10_3
llh_10 <- low10_1*low10_2*high10_3
lhl_10 <- low10_1*high10_2*low10_3
hll_10 <- high10_1*low10_2*low10_3
lll_10 <- low10_1*low10_2*low10_3


hhh_20 <- high20_1*high20_2*high20_3
lhh_20 <- low20_1*high20_2*high20_3
hlh_20 <- high20_1*low20_2*high20_3
hhl_20 <- high20_1*high20_2*low20_3
llh_20 <- low20_1*low20_2*high20_3
lhl_20 <- low20_1*high20_2*low20_3
hll_20 <- high20_1*low20_2*low20_3
lll_20 <- low20_1*low20_2*low20_3


#Value weighted portfolios

hh12_5 <- hh12_5 * coredata(mktcap5_jun)
lh12_5 <- lh12_5 * coredata(mktcap5_jun)
hl12_5 <- hl12_5 * coredata(mktcap5_jun)
ll12_5 <- ll12_5 * coredata(mktcap5_jun)

hh13_5 <- hh13_5 * coredata(mktcap5_jun)
lh13_5 <- lh13_5 * coredata(mktcap5_jun)
hl13_5 <- hl13_5 * coredata(mktcap5_jun)
ll13_5 <- ll13_5 * coredata(mktcap5_jun)

hh23_5 <- hh23_5 * coredata(mktcap5_jun)
lh23_5 <- lh23_5 * coredata(mktcap5_jun)
hl23_5 <- hl23_5 * coredata(mktcap5_jun)
ll23_5 <- ll23_5 * coredata(mktcap5_jun)


hh12_10 <- hh12_10 * coredata(mktcap10_jun)
lh12_10 <- lh12_10 * coredata(mktcap10_jun)
hl12_10 <- hl12_10 * coredata(mktcap10_jun)
ll12_10 <- ll12_10 * coredata(mktcap10_jun)

hh13_10 <- hh13_10 * coredata(mktcap10_jun)
lh13_10 <- lh13_10 * coredata(mktcap10_jun)
hl13_10 <- hl13_10 * coredata(mktcap10_jun)
ll13_10 <- ll13_10 * coredata(mktcap10_jun)

hh23_10 <- hh23_10 * coredata(mktcap10_jun)
lh23_10 <- lh23_10 * coredata(mktcap10_jun)
hl23_10 <- hl23_10 * coredata(mktcap10_jun)
ll23_10 <- ll23_10 * coredata(mktcap10_jun)


hh12_20 <- hh12_20 * coredata(mktcap20_jun)
lh12_20 <- lh12_20 * coredata(mktcap20_jun)
hl12_20 <- hl12_20 * coredata(mktcap20_jun)
ll12_20 <- ll12_20 * coredata(mktcap20_jun)

hh13_20 <- hh13_20 * coredata(mktcap20_jun)
lh13_20 <- lh13_20 * coredata(mktcap20_jun)
hl13_20 <- hl13_20 * coredata(mktcap20_jun)
ll13_20 <- ll13_20 * coredata(mktcap20_jun)

hh23_20 <- hh23_20 * coredata(mktcap20_jun)
lh23_20 <- lh23_20 * coredata(mktcap20_jun)
hl23_20 <- hl23_20 * coredata(mktcap20_jun)
ll23_20 <- ll23_20 * coredata(mktcap20_jun)


hh12_5 <- hh12_5/rowSums(hh12_5, na.rm=TRUE)
lh12_5 <- lh12_5/rowSums(lh12_5, na.rm=TRUE)
hl12_5 <- hl12_5/rowSums(hl12_5, na.rm=TRUE)
ll12_5 <- ll12_5/rowSums(ll12_5, na.rm=TRUE)

hh13_5 <- hh13_5/rowSums(hh13_5, na.rm=TRUE)
lh13_5 <- lh13_5/rowSums(lh13_5, na.rm=TRUE)
hl13_5 <- hl13_5/rowSums(hl13_5, na.rm=TRUE)
ll13_5 <- ll13_5/rowSums(ll13_5, na.rm=TRUE)

hh23_5 <- hh23_5/rowSums(hh23_5, na.rm=TRUE)
lh23_5 <- lh23_5/rowSums(lh23_5, na.rm=TRUE)
hl23_5 <- hl23_5/rowSums(hl23_5, na.rm=TRUE)
ll23_5 <- ll23_5/rowSums(ll23_5, na.rm=TRUE)


hh12_10 <- hh12_10/rowSums(hh12_10, na.rm=TRUE)
lh12_10 <- lh12_10/rowSums(lh12_10, na.rm=TRUE)
hl12_10 <- hl12_10/rowSums(hl12_10, na.rm=TRUE)
ll12_10 <- ll12_10/rowSums(ll12_10, na.rm=TRUE)

hh13_10 <- hh13_10/rowSums(hh13_10, na.rm=TRUE)
lh13_10 <- lh13_10/rowSums(lh13_10, na.rm=TRUE)
hl13_10 <- hl13_10/rowSums(hl13_10, na.rm=TRUE)
ll13_10 <- ll13_10/rowSums(ll13_10, na.rm=TRUE)

hh23_10 <- hh23_10/rowSums(hh23_10, na.rm=TRUE)
lh23_10 <- lh23_10/rowSums(lh23_10, na.rm=TRUE)
hl23_10 <- hl23_10/rowSums(hl23_10, na.rm=TRUE)
ll23_10 <- ll23_10/rowSums(ll23_10, na.rm=TRUE)


hh12_20 <- hh12_20/rowSums(hh12_20, na.rm=TRUE)
lh12_20 <- lh12_20/rowSums(lh12_20, na.rm=TRUE)
hl12_20 <- hl12_20/rowSums(hl12_20, na.rm=TRUE)
ll12_20 <- ll12_20/rowSums(ll12_20, na.rm=TRUE)

hh13_20 <- hh13_20/rowSums(hh13_20, na.rm=TRUE)
lh13_20 <- lh13_20/rowSums(lh13_20, na.rm=TRUE)
hl13_20 <- hl13_20/rowSums(hl13_20, na.rm=TRUE)
ll13_20 <- ll13_20/rowSums(ll13_20, na.rm=TRUE)

hh23_20 <- hh23_20/rowSums(hh23_20, na.rm=TRUE)
lh23_20 <- lh23_20/rowSums(lh23_20, na.rm=TRUE)
hl23_20 <- hl23_20/rowSums(hl23_20, na.rm=TRUE)
ll23_20 <- ll23_20/rowSums(ll23_20, na.rm=TRUE)


hhh_5 <- hhh_5 * coredata(mktcap5_jun)
lhh_5 <- lhh_5 * coredata(mktcap5_jun)
hlh_5 <- hlh_5 * coredata(mktcap5_jun)
hhl_5 <- hhl_5 * coredata(mktcap5_jun)
llh_5 <- llh_5 * coredata(mktcap5_jun)
lhl_5 <- lhl_5 * coredata(mktcap5_jun)
hll_5 <- hll_5 * coredata(mktcap5_jun)
lll_5 <- lll_5 * coredata(mktcap5_jun)


hhh_10 <- hhh_10 * coredata(mktcap10_jun)
lhh_10 <- lhh_10 * coredata(mktcap10_jun)
hlh_10 <- hlh_10 * coredata(mktcap10_jun)
hhl_10 <- hhl_10 * coredata(mktcap10_jun)
llh_10 <- llh_10 * coredata(mktcap10_jun)
lhl_10 <- lhl_10 * coredata(mktcap10_jun)
hll_10 <- hll_10 * coredata(mktcap10_jun)
lll_10 <- lll_10 * coredata(mktcap10_jun)


hhh_20 <- hhh_20 * coredata(mktcap20_jun)
lhh_20 <- lhh_20 * coredata(mktcap20_jun)
hlh_20 <- hlh_20 * coredata(mktcap20_jun)
hhl_20 <- hhl_20 * coredata(mktcap20_jun)
llh_20 <- llh_20 * coredata(mktcap20_jun)
lhl_20 <- lhl_20 * coredata(mktcap20_jun)
hll_20 <- hll_20 * coredata(mktcap20_jun)
lll_20 <- lll_20 * coredata(mktcap20_jun)


hhh_5 <- hhh_5/rowSums(hhh_5, na.rm=TRUE)
lhh_5 <- lhh_5/rowSums(lhh_5, na.rm=TRUE)
hlh_5 <- hlh_5/rowSums(hlh_5, na.rm=TRUE)
hhl_5 <- hhl_5/rowSums(hhl_5, na.rm=TRUE)
llh_5 <- llh_5/rowSums(llh_5, na.rm=TRUE)
lhl_5 <- lhl_5/rowSums(lhl_5, na.rm=TRUE)
hll_5 <- hll_5/rowSums(hll_5, na.rm=TRUE)
lll_5 <- lll_5/rowSums(lll_5, na.rm=TRUE)


hhh_10 <- hhh_10/rowSums(hhh_10, na.rm=TRUE)
lhh_10 <- lhh_10/rowSums(lhh_10, na.rm=TRUE)
hlh_10 <- hlh_10/rowSums(hlh_10, na.rm=TRUE)
hhl_10 <- hhl_10/rowSums(hhl_10, na.rm=TRUE)
llh_10 <- llh_10/rowSums(llh_10, na.rm=TRUE)
lhl_10 <- lhl_10/rowSums(lhl_10, na.rm=TRUE)
hll_10 <- hll_10/rowSums(hll_10, na.rm=TRUE)
lll_10 <- lll_10/rowSums(lll_10, na.rm=TRUE)


hhh_20 <- hhh_20/rowSums(hhh_20, na.rm=TRUE)
lhh_20 <- lhh_20/rowSums(lhh_20, na.rm=TRUE)
hlh_20 <- hlh_20/rowSums(hlh_20, na.rm=TRUE)
hhl_20 <- hhl_20/rowSums(hhl_20, na.rm=TRUE)
llh_20 <- llh_20/rowSums(llh_20, na.rm=TRUE)
lhl_20 <- lhl_20/rowSums(lhl_20, na.rm=TRUE)
hll_20 <- hll_20/rowSums(hll_20, na.rm=TRUE)
lll_20 <- lll_20/rowSums(lll_20, na.rm=TRUE)


#change into xts format

hh12_5 <- xts(x = hh12_5, order.by = index(NCAV5))
lh12_5 <- xts(x = lh12_5, order.by = index(NCAV5))
hl12_5 <- xts(x = hl12_5, order.by = index(NCAV5))
ll12_5 <- xts(x = ll12_5, order.by = index(NCAV5))

hh13_5 <- xts(x = hh13_5, order.by = index(NCAV5))
lh13_5 <- xts(x = lh13_5, order.by = index(NCAV5))
hl13_5 <- xts(x = hl13_5, order.by = index(NCAV5))
ll13_5 <- xts(x = ll13_5, order.by = index(NCAV5))

hh23_5 <- xts(x = hh23_5, order.by = index(NCAV5))
lh23_5 <- xts(x = lh23_5, order.by = index(NCAV5))
hl23_5 <- xts(x = hl23_5, order.by = index(NCAV5))
ll23_5 <- xts(x = ll23_5, order.by = index(NCAV5))


hh12_10 <- xts(x = hh12_10, order.by = index(NCAV10))
lh12_10 <- xts(x = lh12_10, order.by = index(NCAV10))
hl12_10 <- xts(x = hl12_10, order.by = index(NCAV10))
ll12_10 <- xts(x = ll12_10, order.by = index(NCAV10))

hh13_10 <- xts(x = hh13_10, order.by = index(NCAV10))
lh13_10 <- xts(x = lh13_10, order.by = index(NCAV10))
hl13_10 <- xts(x = hl13_10, order.by = index(NCAV10))
ll13_10 <- xts(x = ll13_10, order.by = index(NCAV10))

hh23_10 <- xts(x = hh23_10, order.by = index(NCAV10))
lh23_10 <- xts(x = lh23_10, order.by = index(NCAV10))
hl23_10 <- xts(x = hl23_10, order.by = index(NCAV10))
ll23_10 <- xts(x = ll23_10, order.by = index(NCAV10))


hh12_20 <- xts(x = hh12_20, order.by = index(NCAV20))
lh12_20 <- xts(x = lh12_20, order.by = index(NCAV20))
hl12_20 <- xts(x = hl12_20, order.by = index(NCAV20))
ll12_20 <- xts(x = ll12_20, order.by = index(NCAV20))

hh13_20 <- xts(x = hh13_20, order.by = index(NCAV20))
lh13_20 <- xts(x = lh13_20, order.by = index(NCAV20))
hl13_20 <- xts(x = hl13_20, order.by = index(NCAV20))
ll13_20 <- xts(x = ll13_20, order.by = index(NCAV20))

hh23_20 <- xts(x = hh23_20, order.by = index(NCAV20))
lh23_20 <- xts(x = lh23_20, order.by = index(NCAV20))
hl23_20 <- xts(x = hl23_20, order.by = index(NCAV20))
ll23_20 <- xts(x = ll23_20, order.by = index(NCAV20))


hhh_5 <- xts(x = hhh_5, order.by = index(NCAV5))
lhh_5 <- xts(x = lhh_5, order.by = index(NCAV5))
hlh_5 <- xts(x = hlh_5, order.by = index(NCAV5))
hhl_5 <- xts(x = hhl_5, order.by = index(NCAV5))
llh_5 <- xts(x = llh_5, order.by = index(NCAV5))
lhl_5 <- xts(x = lhl_5, order.by = index(NCAV5))
hll_5 <- xts(x = hll_5, order.by = index(NCAV5))
lll_5 <- xts(x = lll_5, order.by = index(NCAV5))


hhh_10 <- xts(x = hhh_10, order.by = index(NCAV10))
lhh_10 <- xts(x = lhh_10, order.by = index(NCAV10))
hlh_10 <- xts(x = hlh_10, order.by = index(NCAV10))
hhl_10 <- xts(x = hhl_10, order.by = index(NCAV10))
llh_10 <- xts(x = llh_10, order.by = index(NCAV10))
lhl_10 <- xts(x = lhl_10, order.by = index(NCAV10))
hll_10 <- xts(x = hll_10, order.by = index(NCAV10))
lll_10 <- xts(x = lll_10, order.by = index(NCAV10))


hhh_20 <- xts(x = hhh_20, order.by = index(NCAV20))
lhh_20 <- xts(x = lhh_20, order.by = index(NCAV20))
hlh_20 <- xts(x = hlh_20, order.by = index(NCAV20))
hhl_20 <- xts(x = hhl_20, order.by = index(NCAV20))
llh_20 <- xts(x = llh_20, order.by = index(NCAV20))
lhl_20 <- xts(x = lhl_20, order.by = index(NCAV20))
hll_20 <- xts(x = hll_20, order.by = index(NCAV20))
lll_20 <- xts(x = lll_20, order.by = index(NCAV20))


#replace NA with 0

hh12_5[is.na(hh12_5)] <- 0
lh12_5[is.na(lh12_5)] <- 0
hl12_5[is.na(hl12_5)] <- 0
ll12_5[is.na(ll12_5)] <- 0

hh13_5[is.na(hh13_5)] <- 0
lh13_5[is.na(lh13_5)] <- 0
hl13_5[is.na(hl13_5)] <- 0
ll13_5[is.na(ll13_5)] <- 0

hh23_5[is.na(hh23_5)] <- 0
lh23_5[is.na(lh23_5)] <- 0
hl23_5[is.na(hl23_5)] <- 0
ll23_5[is.na(ll23_5)] <- 0


hh12_10[is.na(hh12_10)] <- 0
lh12_10[is.na(lh12_10)] <- 0
hl12_10[is.na(hl12_10)] <- 0
ll12_10[is.na(ll12_10)] <- 0

hh13_10[is.na(hh13_10)] <- 0
lh13_10[is.na(lh13_10)] <- 0
hl13_10[is.na(hl13_10)] <- 0
ll13_10[is.na(ll13_10)] <- 0

hh23_10[is.na(hh23_10)] <- 0
lh23_10[is.na(lh23_10)] <- 0
hl23_10[is.na(hl23_10)] <- 0
ll23_10[is.na(ll23_10)] <- 0


hh12_20[is.na(hh12_20)] <- 0
lh12_20[is.na(lh12_20)] <- 0
hl12_20[is.na(hl12_20)] <- 0
ll12_20[is.na(ll12_20)] <- 0

hh13_20[is.na(hh13_20)] <- 0
lh13_20[is.na(lh13_20)] <- 0
hl13_20[is.na(hl13_20)] <- 0
ll13_20[is.na(ll13_20)] <- 0

hh23_20[is.na(hh23_20)] <- 0
lh23_20[is.na(lh23_20)] <- 0
hl23_20[is.na(hl23_20)] <- 0
ll23_20[is.na(ll23_20)] <- 0


hhh_5[is.na(hhh_5)] <- 0
lhh_5[is.na(lhh_5)] <- 0
hlh_5[is.na(hlh_5)] <- 0
hhl_5[is.na(hhl_5)] <- 0
llh_5[is.na(llh_5)] <- 0
lhl_5[is.na(lhl_5)] <- 0
hll_5[is.na(hll_5)] <- 0
lll_5[is.na(lll_5)] <- 0


hhh_10[is.na(hhh_10)] <- 0
lhh_10[is.na(lhh_10)] <- 0
hlh_10[is.na(hlh_10)] <- 0
hhl_10[is.na(hhl_10)] <- 0
llh_10[is.na(llh_10)] <- 0
lhl_10[is.na(lhl_10)] <- 0
hll_10[is.na(hll_10)] <- 0
lll_10[is.na(lll_10)] <- 0


hhh_20[is.na(hhh_20)] <- 0
lhh_20[is.na(lhh_20)] <- 0
hlh_20[is.na(hlh_20)] <- 0
hhl_20[is.na(hhl_20)] <- 0
llh_20[is.na(llh_20)] <- 0
lhl_20[is.na(lhl_20)] <- 0
hll_20[is.na(hll_20)] <- 0
lll_20[is.na(lll_20)] <- 0


#Calculate monthly portfolio returns

portfolio_hh12_5 = Return.portfolio(ret5, hh12_5, verbose = TRUE)
portfolio_lh12_5 = Return.portfolio(ret5, lh12_5, verbose = TRUE)
portfolio_hl12_5 = Return.portfolio(ret5, hl12_5, verbose = TRUE)
portfolio_ll12_5 = Return.portfolio(ret5, ll12_5, verbose = TRUE)

portfolio_hh13_5 = Return.portfolio(ret5, hh13_5, verbose = TRUE)
portfolio_lh13_5 = Return.portfolio(ret5, lh13_5, verbose = TRUE)
portfolio_hl13_5 = Return.portfolio(ret5, hl13_5, verbose = TRUE)
portfolio_ll13_5 = Return.portfolio(ret5, ll13_5, verbose = TRUE)

portfolio_hh23_5 = Return.portfolio(ret5, hh23_5, verbose = TRUE)
portfolio_lh23_5 = Return.portfolio(ret5, lh23_5, verbose = TRUE)
portfolio_hl23_5 = Return.portfolio(ret5, hl23_5, verbose = TRUE)
portfolio_ll23_5 = Return.portfolio(ret5, ll23_5, verbose = TRUE)


portfolio_hh12_10 = Return.portfolio(ret10, hh12_10, verbose = TRUE)
portfolio_lh12_10 = Return.portfolio(ret10, lh12_10, verbose = TRUE)
portfolio_hl12_10 = Return.portfolio(ret10, hl12_10, verbose = TRUE)
portfolio_ll12_10 = Return.portfolio(ret10, ll12_10, verbose = TRUE)

portfolio_hh13_10 = Return.portfolio(ret10, hh13_10, verbose = TRUE)
portfolio_lh13_10 = Return.portfolio(ret10, lh13_10, verbose = TRUE)
portfolio_hl13_10 = Return.portfolio(ret10, hl13_10, verbose = TRUE)
portfolio_ll13_10 = Return.portfolio(ret10, ll13_10, verbose = TRUE)

portfolio_hh23_10 = Return.portfolio(ret10, hh23_10, verbose = TRUE)
portfolio_lh23_10 = Return.portfolio(ret10, lh23_10, verbose = TRUE)
portfolio_hl23_10 = Return.portfolio(ret10, hl23_10, verbose = TRUE)
portfolio_ll23_10 = Return.portfolio(ret10, ll23_10, verbose = TRUE)


portfolio_hh12_20 = Return.portfolio(ret20, hh12_20, verbose = TRUE)
portfolio_lh12_20 = Return.portfolio(ret20, lh12_20, verbose = TRUE)
portfolio_hl12_20 = Return.portfolio(ret20, hl12_20, verbose = TRUE)
portfolio_ll12_20 = Return.portfolio(ret20, ll12_20, verbose = TRUE)

portfolio_hh13_20 = Return.portfolio(ret20, hh13_20, verbose = TRUE)
portfolio_lh13_20 = Return.portfolio(ret20, lh13_20, verbose = TRUE)
portfolio_hl13_20 = Return.portfolio(ret20, hl13_20, verbose = TRUE)
portfolio_ll13_20 = Return.portfolio(ret20, ll13_20, verbose = TRUE)

portfolio_hh23_20 = Return.portfolio(ret20, hh23_20, verbose = TRUE)
portfolio_lh23_20 = Return.portfolio(ret20, lh23_20, verbose = TRUE)
portfolio_hl23_20 = Return.portfolio(ret20, hl23_20, verbose = TRUE)
portfolio_ll23_20 = Return.portfolio(ret20, ll23_20, verbose = TRUE)


portfolio_hhh_5 = Return.portfolio(ret5, hhh_5, verbose = TRUE)
portfolio_lhh_5 = Return.portfolio(ret5, lhh_5, verbose = TRUE)
portfolio_hlh_5 = Return.portfolio(ret5, hlh_5, verbose = TRUE)
portfolio_hhl_5 = Return.portfolio(ret5, hhl_5, verbose = TRUE)
portfolio_llh_5 = Return.portfolio(ret5, llh_5, verbose = TRUE)
portfolio_lhl_5 = Return.portfolio(ret5, lhl_5, verbose = TRUE)
portfolio_hll_5 = Return.portfolio(ret5, hll_5, verbose = TRUE)
portfolio_lll_5 = Return.portfolio(ret5, lll_5, verbose = TRUE)


portfolio_hhh_10 = Return.portfolio(ret10, hhh_10, verbose = TRUE)
portfolio_lhh_10 = Return.portfolio(ret10, lhh_10, verbose = TRUE)
portfolio_hlh_10 = Return.portfolio(ret10, hlh_10, verbose = TRUE)
portfolio_hhl_10 = Return.portfolio(ret10, hhl_10, verbose = TRUE)
portfolio_llh_10 = Return.portfolio(ret10, llh_10, verbose = TRUE)
portfolio_lhl_10 = Return.portfolio(ret10, lhl_10, verbose = TRUE)
portfolio_hll_10 = Return.portfolio(ret10, hll_10, verbose = TRUE)
portfolio_lll_10 = Return.portfolio(ret10, lll_10, verbose = TRUE)


portfolio_hhh_20 = Return.portfolio(ret20, hhh_20, verbose = TRUE)
portfolio_lhh_20 = Return.portfolio(ret20, lhh_20, verbose = TRUE)
portfolio_hlh_20 = Return.portfolio(ret20, hlh_20, verbose = TRUE)
portfolio_hhl_20 = Return.portfolio(ret20, hhl_20, verbose = TRUE)
portfolio_llh_20 = Return.portfolio(ret20, llh_20, verbose = TRUE)
portfolio_lhl_20 = Return.portfolio(ret20, lhl_20, verbose = TRUE)
portfolio_hll_20 = Return.portfolio(ret20, hll_20, verbose = TRUE)
portfolio_lll_20 = Return.portfolio(ret20, lll_20, verbose = TRUE)


#Calculate factor return

HML1_5 <- portfolio_hh12_5$returns - portfolio_ll12_5$returns
HML2_5 <- portfolio_hh13_5$returns - portfolio_ll13_5$returns
HML3_5 <- portfolio_hh23_5$returns - portfolio_ll23_5$returns

HML1_10 <- portfolio_hh12_10$returns - portfolio_ll12_10$returns
HML2_10 <- portfolio_hh13_10$returns - portfolio_ll13_10$returns
HML3_10 <- portfolio_hh23_10$returns - portfolio_ll23_10$returns

HML1_20 <- portfolio_hh12_20$returns - portfolio_ll12_20$returns
HML2_20 <- portfolio_hh13_20$returns - portfolio_ll13_20$returns
HML3_20 <- portfolio_hh23_20$returns - portfolio_ll23_20$returns


HML4_5 <- portfolio_hhh_5$returns - portfolio_lll_5$returns
HML5_5 <- (portfolio_hhh_5$returns + portfolio_lhh_5$returns + portfolio_hlh_5$returns + portfolio_hhl_5$returns)/4 - (portfolio_lll_5$returns + portfolio_llh_5$returns + portfolio_lhl_5$returns + portfolio_hll_5$returns)/4
HML6_5 <- (portfolio_lhh_5$returns + portfolio_hlh_5$returns + portfolio_hhl_5$returns)/3 - (portfolio_llh_5$returns + portfolio_lhl_5$returns + portfolio_hll_5$returns)/3


HML4_10 <- portfolio_hhh_10$returns - portfolio_lll_10$returns
HML5_10 <- (portfolio_hhh_10$returns + portfolio_lhh_10$returns + portfolio_hlh_10$returns + portfolio_hhl_10$returns)/4 - (portfolio_lll_10$returns + portfolio_llh_10$returns + portfolio_lhl_10$returns + portfolio_hll_10$returns)/4
HML6_10 <- (portfolio_lhh_10$returns + portfolio_hlh_10$returns + portfolio_hhl_10$returns)/3 - (portfolio_llh_10$returns + portfolio_lhl_10$returns + portfolio_hll_10$returns)/3


HML4_20 <- portfolio_hhh_20$returns - portfolio_lll_20$returns
HML5_20 <- (portfolio_hhh_20$returns + portfolio_lhh_20$returns + portfolio_hlh_20$returns + portfolio_hhl_20$returns)/4 - (portfolio_lll_20$returns + portfolio_llh_20$returns + portfolio_lhl_20$returns + portfolio_hll_20$returns)/4
HML6_20 <- (portfolio_lhh_20$returns + portfolio_hlh_20$returns + portfolio_hhl_20$returns)/3 - (portfolio_llh_20$returns + portfolio_lhl_20$returns + portfolio_hll_20$returns)/3


charts.PerformanceSummary(HML1_5, main = "HML1 5years")
charts.PerformanceSummary(HML2_5, main = "HML2 5years")
charts.PerformanceSummary(HML3_5, main = "HML3 5years")
charts.PerformanceSummary(HML1_10, main = "HML1 10years")
charts.PerformanceSummary(HML2_10, main = "HML2 10years")
charts.PerformanceSummary(HML3_10, main = "HML3 10years")
charts.PerformanceSummary(HML1_20, main = "HML1 20years")
charts.PerformanceSummary(HML2_20, main = "HML2 20years")
charts.PerformanceSummary(HML3_20, main = "HML3 20years")

charts.PerformanceSummary(HML4_5, main = "HML4 5years")
charts.PerformanceSummary(HML5_5, main = "HML5 5years")
charts.PerformanceSummary(HML6_5, main = "HML6 5years")
charts.PerformanceSummary(HML4_10, main = "HML4 10years")
charts.PerformanceSummary(HML5_10, main = "HML5 10years")
charts.PerformanceSummary(HML6_10, main = "HML6 10years")
charts.PerformanceSummary(HML4_20, main = "HML4 20years")
charts.PerformanceSummary(HML5_20, main = "HML5 20years")
charts.PerformanceSummary(HML6_20, main = "HML6 20years")


#Crawling S&P500 data
symbols = 'SPY'
getSymbols(symbols)

prices = do.call(cbind,
                 lapply(symbols, function(x)Cl(get(x))))
prices <- prices[endpoints(prices, on = 'months'),]

ret_spy_5 = Return.calculate(prices)
ret_spy_5 = ret_spy_5['2016-07::2020-12']

colnames(ret_spy_5) <- 'SPY'
charts.PerformanceSummary(ret_spy_5, main = "S&P500")

ret_spy_10 = Return.calculate(prices)
ret_spy_10 = ret_spy_10['2010-07::2020-12']

colnames(ret_spy_10) <- 'SPY'
charts.PerformanceSummary(ret_spy_10, main = "S&P500")

ret_spy_20 = Return.calculate(prices)
ret_spy_20 = ret_spy_20['2000-07::2020-12']

colnames(ret_spy_20) <- 'SPY'
charts.PerformanceSummary(ret_spy_20, main = "S&P500")


factors2_5 <- cbind(HML1_5,HML2_5,HML3_5,ret_spy_5)
names(factors2_5) <- c("HML1","HML2","HML3","SPY")
charts.PerformanceSummary(factors2_5, main = "2 Factor Model Backtesting 5years")

factors2_10 <- cbind(HML1_10,HML2_10,HML3_10,ret_spy_10)
names(factors2_10) <- c("HML1","HML2","HML3","SPY")
charts.PerformanceSummary(factors2_10, main = "2 Factor Model Backtesting 10years")

factors2_20 <- cbind(HML1_20,HML2_20,HML3_20,ret_spy_20)
names(factors2_20) <- c("HML1","HML2","HML3","SPY")
charts.PerformanceSummary(factors2_20, main = "2 Factor Model Backtesting 20years")


factors3_5 <- cbind(HML4_5,HML5_5,HML6_5,ret_spy_5)
names(factors3_5) <- c("HML4","HML5","HML6","SPY")
charts.PerformanceSummary(factors3_5, main = "3 Factor Model Backtesting 5years")

factors3_10 <- cbind(HML4_10,HML5_10,HML6_10,ret_spy_10)
names(factors3_10) <- c("HML4","HML5","HML6","SPY")
charts.PerformanceSummary(factors3_10, main = "3 Factor Model Backtesting 10years")

factors3_20 <- cbind(HML4_20,HML5_20,HML6_20,ret_spy_20)
names(factors3_20) <- c("HML4","HML5","HML6","SPY")
charts.PerformanceSummary(factors3_20, main = "3 Factor Model Backtesting 20years")


#T-Test using Newey and West adjustment using 5 lags
result2_5 <- as.data.frame(c(1:2))
for (i in 1:4){
  test2_5 <- coeftest(lm(factors2_5[,i] ~ 1), vcov. = NeweyWest(lm(factors2_5[,i] ~ 1), lag=5, prewhite = F, adjust = T))
  result2_5[1,i] <- test2_5[,1]
  result2_5[2,i] <- test2_5[,3]}
names(result2_5) <- c("HML1_5","HML2_5","HML3_5","SPY_5")
rownames(result2_5) <- c("return", "t-stat")

result2_5

result2_10 <- as.data.frame(c(1:2))
for (i in 1:4){
  test2_10 <- coeftest(lm(factors2_10[,i] ~ 1), vcov. = NeweyWest(lm(factors2_10[,i] ~ 1), lag=5, prewhite = F, adjust = T))
  result2_10[1,i] <- test2_10[,1]
  result2_10[2,i] <- test2_10[,3]}
names(result2_10) <- c("HML1_10","HML2_10","HML3_10","SPY_10")
rownames(result2_10) <- c("return", "t-stat")

result2_10

result2_20 <- as.data.frame(c(1:2))
for (i in 1:4){
  test2_20 <- coeftest(lm(factors2_20[,i] ~ 1), vcov. = NeweyWest(lm(factors2_20[,i] ~ 1), lag=5, prewhite = F, adjust = T))
  result2_20[1,i] <- test2_20[,1]
  result2_20[2,i] <- test2_20[,3]}
names(result2_20) <- c("HML1_20","HML2_20","HML3_20","SPY_20")
rownames(result2_20) <- c("return", "t-stat")

result2_20

result3_5 <- as.data.frame(c(1:2))
for (i in 1:4){
  test3_5 <- coeftest(lm(factors3_5[,i] ~ 1), vcov. = NeweyWest(lm(factors3_5[,i] ~ 1), lag=5, prewhite = F, adjust = T))
  result3_5[1,i] <- test3_5[,1]
  result3_5[2,i] <- test3_5[,3]}
names(result3_5) <- c("HML4_5","HML5_5","HML6_5","SPY_5")
rownames(result3_5) <- c("return", "t-stat")

result3_5

result3_10 <- as.data.frame(c(1:2))
for (i in 1:4){
  test3_10 <- coeftest(lm(factors3_10[,i] ~ 1), vcov. = NeweyWest(lm(factors3_10[,i] ~ 1), lag=5, prewhite = F, adjust = T))
  result3_10[1,i] <- test3_10[,1]
  result3_10[2,i] <- test3_10[,3]}
names(result3_10) <- c("HML4_10","HML5_10","HML6_10","SPY_10")
rownames(result3_10) <- c("return", "t-stat")

result3_10

result3_20 <- as.data.frame(c(1:2))
for (i in 1:4){
  test3_20 <- coeftest(lm(factors3_20[,i] ~ 1), vcov. = NeweyWest(lm(factors3_20[,i] ~ 1), lag=5, prewhite = F, adjust = T))
  result3_20[1,i] <- test3_20[,1]
  result3_20[2,i] <- test3_20[,3]}
names(result3_20) <- c("HML4_20","HML5_20","HML6_20","SPY_20")
rownames(result3_20) <- c("return", "t-stat")

result3_20


#if your memory is too small to operate this code, then use this!
memory.size(max = TRUE)
memory.size(max = FALSE)
memory.limit(size = NA)
memory.limit(size = 100000)