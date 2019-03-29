library(timetk)
library(tidyquant)
library(tibbletime)
library(xts)
library(tidyverse)
library(stringr)
library(scales)
alldata<- read.csv("E:/shu/monday/stock16_19.csv",header=T,sep=",",stringsAsFactors = F)

#remove na column
na_flag <- apply(is.na(alldata), 2, sum)
data19 <- alldata[,which(na_flag == 0)]

#set index of objects
data19$るら <- as.Date(data19$るら,format="%Y/%m/%d")
data19 <- as.xts(x =data19[,2:ncol(data19)],order.by = data19$るら)

#split data with time
data16 <-window(data19, start = "2016-01-01", end ="2016-12-31")
data17 <- window(data19,start="2017-01-01",end="2017-12-31")
data18 <- window(data19,start="2018-01-01",end="2018-12-31")

#new stock price
newprice <- window(data19,"2019-03-15")

#setting loop into calculate
tesdata <- vector("list",length=3)
tesdata[[1]] <-data16 
tesdata[[2]] <-data17 
tesdata[[3]] <-data18 
for (i in 1:3){
  
  #calculated
  maximum<-apply(tesdata[[i]],2,max)
  minmum <- apply(tesdata[[i]],2,min)
  meanvalue <- apply(tesdata[[i]],2,mean)
  maxminavg <- apply(tesdata[[i]],2,function(x)((max(x)+min(x))/2))
  ##select rf value
  if(i==1){rf <- 0.010897}
  else if(i==2){rf <- 0.01035}
  else{rf <- 0.01035}
  #calculate Era
  rangedata <- apply(tesdata[[i]],2,function(x)(x-lag(x))/lag(x))
  rangedata <- as.data.frame(na.omit(rangedata))
  beta <- apply(rangedata[,1:ncol(rangedata)-1],2,
                function(x)cov(x,rangedata[,ncol(rangedata)])/var(rangedata[,ncol(rangedata)]))
  Erm <- mean(rangedata[,ncol(rangedata)])
  Era<- as.data.frame(beta)
  Era <- apply(Era,2,function(x)Era[,1]*(Erm-rf)+rf)
  Era <- as.data.frame(Era)
  
  #prediction stock price
  tesdata[[i]] <- data.frame(maximum,maxminavg,meanvalue,minmum)
  tesdata[[i]]<- tesdata[[i]][-nrow(tesdata[[i]]),]
  tesdata[[i]]<- lapply(tesdata[[i]],function(x)tesdata[[i]]+Era[1:nrow(Era),])
  tesdata[[i]]<- as.data.frame(tesdata[[i]][1])
  colnames(tesdata[[i]]) <- c("程蔼基","い基","キА基","程基")
}

#fixed list table
fina16<- rownames_to_column(tesdata[[1]], "布嘿")
fina17<- rownames_to_column(tesdata[[2]], "布嘿")
fina18<- rownames_to_column(tesdata[[3]], "布嘿")

#filter
tumax17 <- as.data.frame(apply(data17,2,max))
tumax17 <- rownames_to_column(tumax17,"布嘿")
tumax17 <- tumax17[-nrow(tumax17),]
tumax18 <- as.data.frame(apply(data18,2,max))
tumax18 <- rownames_to_column(tumax18,"布嘿")
tumax18 <- tumax18[-nrow(tumax18),]
finatb <- filter(fina18,fina16[,3]<tumax17[,2]&fina17[,3]<tumax18[,2])
a <- as.data.frame(str_split_fixed(finatb$布嘿, "Μ絃基...", 2))
a <-as.data.frame( a[,-1])
b <- as.data.frame(gsub("\\.", "",a[,1]))
colnames(b)[1] <- "布嘿"
finatb["布嘿"] <- b["布嘿"]
rm(a,b)
eps <- read.csv("E:/shu/monday/EPS.csv",header=T,sep=",",stringsAsFactors = F)
ocf <- read.csv("E:/shu/monday/Operating Cash Flow.csv",header=T,sep=",",stringsAsFactors = F)
a<- as.data.frame(sapply(eps[1],function(x){gsub(" ","",x)})) 
eps["そ"] <- a["そ"]
colnames(eps)[1] <- "布嘿"
b<- as.data.frame(sapply(ocf[1],function(x){gsub(" ","",x)})) 
ocf["そ"] <- b["そ"]
colnames(ocf)[1] <- "布嘿"
rm(a,b)
finatb <- merge(finatb,eps,by="布嘿")
finatb <- merge(finatb,ocf,by="布嘿")
finatb <- na.omit(finatb)
finatb <- finatb[,-c(6:15)]

#remove strange word 
newprice <- newprice[,-ncol(newprice)]%>%t()
newprice <- as.data.frame(newprice)
newprice <- rownames_to_column(newprice,"布嘿")
a<- as.data.frame(gsub("\\.", "",newprice[,1]))
a <- as.data.frame(gsub("[Μ絃基]", "", a[,1]))
colnames(a) <- "布嘿"
newprice["布嘿"] <- a["布嘿"]
rm(a)

#merge newprice to prediction data
finatb<- merge(finatb,newprice, by = "布嘿")
diffmax <-finatb[2]-finatb[6]
finatb <- cbind(finatb,程蔼基畉=diffmax[,1])
maxrange <-finatb[,7] /finatb[,6]
maxrange <- as.data.frame(maxrange)
colnames(maxrange)[1] <- "基畉ゑ"
finatb <-as.data.frame( cbind(finatb,基畉ゑ=maxrange[,1]))
finatb <- arrange(finatb,desc(基畉ゑ))
write.table(finatb, file = "E:/shu/monday/finaltable.csv",row.names=FALSE, na="",col.names=T, sep=",")