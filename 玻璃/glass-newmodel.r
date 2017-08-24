library(ggplot2)
library(lubridate) # date management
library(zoo) #date management
library(corrplot) #covariance visualization
library(glmnet) #lasso regression
library(car) #vif detection
library(reshape2) #mdataframe reshape
#load company report####
load("I:/work/genial-flow/report.rda")
#company netin
index <- grep('����', report$�������)
glass <- report[index,]
report_clean <- function(name, time, y){
  library(lubridate) # date management
  if(class(time)[1] != 'POSIXlt') return('Wrong: required POSIXlt for time')
  day(time) <- 27
  name <- as.factor(name)
  #make diff################
  diff_name <- c(F, abs(diff(as.numeric(name))) < 0.5)
  year <- as.numeric(format(time, format='%Y'))
  diff_year <- c(F, diff(year) < 0.5)
  index <- as.numeric(diff_name & diff_year)
  dy <- c(y[1], diff(y))
  y <- dy*index + y*(1-index)
  #seasonal average################
  dmonth <- c(as.numeric(format(time, format='%Y%m')[1]),
              diff(as.numeric(format(time, format='%Y%m'))))
  month <- as.numeric(format(time,format='%m'))
  dseason <- dmonth/3*index + month*(1-index)/3
  y <- rep(y/dseason, dseason)
  name <- rep(name, dseason)
  time <- rep(time, dseason)
  seasonal_index <- rep(dseason, dseason)
  
  index4 <- which(seasonal_index == 4)
  index3 <- which(seasonal_index == 3)
  index2 <- which(seasonal_index == 2)
  index_1 <- c(index2[c(1:length(index2))%%2 == 1],
               index3[c(1:length(index3))%%3 == 2],
               index4[c(1:length(index4))%%4 == 3])
  index_2 <- c(index3[c(1:length(index3))%%3 == 1],
               index4[c(1:length(index4))%%4 == 2])
  index_3 <- c(index4[c(1:length(index4))%%4 == 1])
  
  time[index_1] <- time[index_1] - months(3)
  time[index_2] <- time[index_2] - months(6)
  time[index_3] <- time[index_3] - months(9)
  
  de_y <- data.frame(name, time, y)
  return(de_y)
}

netin <- report_clean(glass$corp_name, glass$report_period, glass$������)
netin <- netin[order(netin$time),]
netin <- aggregate(netin$y, by=list(as.yearqtr(netin$time)), mean)
names(netin) <- c('time', 'netin')
netin
#chain factor ####
setwd("I:/work/genial-flow/����")
chain <- read.csv('��������1.csv')
names(chain)[1] <- 'time'
chain$time <- as.POSIXlt(chain$time, format = '%Y-%m-%d')

#ave���������ݼ��е�ָ�갴�»򼾶�ƽ��
#Ĭ��ʱ���ǩ�ڵ�һ�У���ʽΪPOSIXlt������������Ϣ
Ave <- function(dat, t=1, type='season', ac = F){
  library(zoo)
  df <- list()
  if(type == 'season'){time <- as.yearqtr(dat[,t])}
  if(type == 'month'){time <- format(dat[,t], format='%Y-%m')}
  dat <- cbind(dat[,t],dat[,-t])
  if(ac){
    for(i in 2: ncol(dat)){
      tmp <- aggregate(dat[,i],by=list(time),max, na.rm = T)
      year <- as.numeric(format(tmp[,1], format='%Y'))
      d_year <- as.numeric(c(F, diff(year) < 0.5))
      
      df[[i-1]] <- c(tmp[1,2], diff(tmp[,2]))*d_year + tmp[,2]*(1-d_year)}
  }else{
    for(i in 2: ncol(dat)){
      df[[i-1]] <- aggregate(dat[,i],by=list(time),sum, na.rm = T)[,2] /
        aggregate(dat[,i], by=list(time),
                  FUN = function(x) max(c(1,sum(abs(x) > 0.1, na.rm = T))))[,2]}
  }
  df <- as.data.frame(matrix(unlist(df),nrow=length(df[[1]])))
  df <- cbind(unique(time), df)
  names(df) <- c('time',names(dat)[-1])
  return(df)
}

chain_season <- Ave(chain[, -grep('��[����]', names(chain))], ac=F)
tmp <- Ave(chain[, c(1,grep('��[����]', names(chain)))], ac=T)
chain_season <- merge(chain_season, tmp, by='time')

#�������ݲ�ȫ�����в���
chain_season <- chain_season[ ,-grep('��', names(chain_season))]
tmp <- read.csv('��������2-����.csv')
names(tmp)[1] <- 'time'
tmp$time <- as.POSIXlt(tmp$time, format = '%Y-%m-%d')
tmp <- Ave(tmp)
#�ĵش���۸�ƽ��
tmp$�����г��� <- rowMeans(tmp[,3:6])
tmp <- tmp[,-c(3:6)]
chain_season <- merge(chain_season, tmp, by='time')
#������������ǰ�����ȣ����в���
chain_season <- chain_season[ ,-grep('��Ʒ������', names(chain_season))]
tmp <- read.csv('���ز���������(��).csv')
names(tmp)[1] <- 'time'
tmp$time <- as.POSIXlt(tmp$time, format = '%Y-%m-%d')
#��ǰ��������,����ֹ����2��30
tmp$time <- tmp$time -days(3) + months(6)
tmp <- Ave(tmp)
chain_season <- merge(chain_season, tmp, by='time')
#����ʯָ����Ч���޳�
chain_season <- chain_season[,-grep('����', names(chain_season))]
#������������2015 2016ȱʧ���޳�
chain_season <- chain_season[,-grep('����', names(chain_season))]
#��������ƽ��������ƽ��
index <- grep('�ֻ�ƽ����.ƽ����..��������', names(chain_season))
chain_season$��������ƽ���� <- rowMeans(chain_season[,index])
chain_season <- chain_season[,-index]

#merge X and Y ####
y_lag1 <- data.frame(time=netin$time + 0.25, y_lag1=netin$netin)
X <- merge(y_lag1, chain_season)
dat_glass <- merge(netin, X, by='time')
t <- dat_glass[,1]
dat_glass <- dat_glass[,-1]

# #model#####
# Y <- dat_glass$netin
# Y_lag1 <- dat_glass$y_lag1
# X <- dat_glass[-c(1,2)]
# vif=15

Model <- function(Y,Y_lag1,X, must=c(),vif=10, method=c('aic','adj.r2')){
  sst <- var(Y)
  r2 <- c()
  for(i in 1:ncol(X)){
    lm0 <- lm(Y~., data = as.data.frame(cbind(Y,Y_lag1, X[,i])))
    tmp <- 1- var(lm0$residuals)/sst
    r2 <- c(r2, tmp)}
  
  index <- must
  if(method == 'aic'){
  aic0 <- extractAIC(lm(Y~., data = as.data.frame(cbind(Y,Y_lag1, X[,index]))))[2]
  for(i in order(r2, decreasing = T)){
    if(length(index) == 0){index <- i}
    if(i %in% index) {next}
    lm0 <- lm(x_new~., data = data.frame(x_new=X[,i], X[,index]))
    tmp <- var(X[,i])/var(lm0$residuals)
    cat('vif',' ',i,' ',tmp,' ')
    if(tmp > vif){next}
    lm1 <- lm(Y~., data = as.data.frame(cbind(Y,Y_lag1, X[,c(index,i)])))
    aic1 <- extractAIC(lm1)[2]
    if(aic1 < aic0){
      index <- c(index,i)
      aic0 <- aic1}}
  }
  if(method == 'adj.r2'){
    lm0 <- lm(Y~., data = as.data.frame(cbind(Y,Y_lag1, X[,index])))
    r2_0 <- summary(lm0)$adj.r.squared
    
    for(i in order(r2, decreasing = T)){
      if(length(index) == 0){index <- i}
      if(i %in% index) {next}
      lm0 <- lm(x_new~., data = data.frame(x_new=X[,i], X[,index]))
      tmp <- var(X[,i])/var(lm0$residuals)
      if(tmp > vif){next}
      lm1 <- lm(Y~., data = as.data.frame(cbind(Y,Y_lag1, X[,c(index,i)])))
      r2_1 <- summary(lm1)$adj.r.squared
      if(r2_1 > r2_0){
        index <- c(index,i)
        r2_0 <- r2_1}}
  }
  
  
  model <- lm(Y~., data = as.data.frame(cbind(Y,Y_lag1, X[,index])))
  return(list(model=model,index=index))
    
}

m <- Model(dat_glass$netin,
           dat_glass$y_lag1,
           dat_glass[-c(1,2)],
           vif = 15,
           method = 'adj.r2'
           )

mean(abs(m$model$residuals/dat_glass$netin))
summary(m$model)
vif(m$model)

index <- which(names(dat_glass) %in% names(m$model$model))
names(dat_glass)[index]
lm1 <- lm(netin~., dat_glass[,c(1,index[-2])])
summary(lm1)

# sink('������ҵ����ģ��.txt')
# summary(lm1)
# lm1$coefficients
# sink()

df <- data.frame(time=t,
                 actual=dat_glass$netin,
                 pred=lm1$fitted.values)
# df <- melt(df, id='time')
# ggplot(df, aes(time, value, color=variable)) + geom_line()
# write.csv(df, '������ҵģ�����.csv')



index <- which(names(dat_glass) %in% names(lm1$model))[-1]
predict(lm1,X[X$time =='2017 Q2',index], interval = 'confidence', se.fit = T)

