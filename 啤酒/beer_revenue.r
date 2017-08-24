library(ggplot2)
library(lubridate) # date management
library(zoo) #date management
library(corrplot) #covariance visualization
library(glmnet) #lasso regression
library(car) #vif detection
library(reshape2) #mdataframe reshape
#load company report####
#dataframe����������ҵ������ָ��
#�޸Ĵӱ�������ȡ��ָ��
load("I:/work/genial-flow/report.rda")
index <- grep('ơ��', report$��������)
beer <- report[index,]
report_clean <- function(name, time, y){
  #report_clean ��������˾��������������Ȳ���ָ��ı仯�����ڼ�����ȱ�������ƽ��������
  #Ĭ�ϱ�����Ϊÿ����ĩ�������Ὣ����31�Ÿ�Ϊ30��
  #�������� name ��˾�� time �����գ�POSIXlt�����������գ� y �����Ĳ���ָ�꣨numeric��
  #������� data.frame�� name time y
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
  
  index_1 <- index_1[!is.na(index_1)]
  index_2 <- index_1[!is.na(index_2)]
  index_3 <- index_1[!is.na(index_3)]
  
  time[index_1] <- time[index_1] - months(3)
  time[index_2] <- time[index_2] - months(6)
  time[index_3] <- time[index_3] - months(9)
  
  de_y <- data.frame(name, time, y)
  return(de_y)
}

revenue <- report_clean(beer$corp_name, beer$report_period, beer$Ӫҵ������)
revenue[revenue == 0] <- NA
revenue <- revenue[order(revenue$time),]
revenue <- aggregate(revenue$y, by=list(as.yearqtr(revenue$time)), mean, na.rm = T)
names(revenue) <- c('time', 'revenue')
# revenue

#load chain factors######
#����·��������ҵ���ļ���
#�������ݣ����Excel����Ԥ����
#ע��ʱ���еĸ�ʽ����/���ָ� or ��-���ָ
setwd("I:/work/genial-flow/ơ��/")
chain <- read.csv('ơ��1.csv')
names(chain)[1] <- 'time'
chain$time <- as.POSIXlt(chain$time, format = '%Y-%m-%d')

Ave <- function(dat, t=1, type='season', ac = F){
  #ave���������ݼ��е�ָ�갴�»򼾶�ƽ��
  #Ĭ��ʱ���ǩ�ڵ�һ�У���ʽΪPOSIXlt������������Ϣ
  library(zoo)
  df <- list()
  if(type == 'season'){time <- as.yearqtr(dat[,t])}
  if(type == 'month'){time <- format(dat[,t], format='%Y-%m')}
  dat <- dat[,c(t,c(1:ncol(dat))[-t])]
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

chain_season <- Ave(chain, ac=F)

#ɾ��ȱʧ�ı���
chain_season <- chain_season[chain_season$time < 2017.5 & chain_season$time >= 2013,]
chain_season[chain_season == 0] <- NA
na1 <- apply(chain_season, 2, function(x){sum(is.na(x))>0.5})
# names(chain_season)[which(na1)]
chain_season <- chain_season[,!na1]


#merge X and Y ####
X <- chain_season
# X[X==0] <- NA
# na1 <- apply(X, 1, function(x){sum(is.na(x))>0.5})
# X <- X[!na1,]
# X$season <- as.factor(sub('2','1', quarters(X$time)))
X_lag1 <- X
X_lag1$time <- X_lag1$time + 0.25 
names(X_lag1)[-1] <- paste0(names(X)[-1],'_lag1')

X_all <- merge(X, X_lag1)
y_lag1 <- data.frame(time=revenue$time + 0.25, y_lag1=revenue$revenue)
X_all <- merge(y_lag1,X_all)
X_all$season <- as.factor(sub('3','1', quarters(X_all$time)))

dat_beer <- merge(revenue,X_all)
t <- dat_beer[,1]
dat_beer <- dat_beer[,-1]

#model#####

#model����ͨ��marginal-r2ɸѡ�Ա���������vif�����������
#mustΪ�ֶ��趨�ı������ģ�͵ı����к�
#���$modeΪģ�ͣ�$indexΪ��ѡģ�͵��Ա����к�
Model <- function(Y,X, must=c(),vif=15, method=c('aic','adj.r2')){
  sst <- var(Y)
  r2 <- c()
  for(i in 1:ncol(X)){
    lm0 <- lm(Y~., data = as.data.frame(cbind(Y,X[,i])))
    tmp <- 1- var(lm0$residuals)/sst
    r2 <- c(r2, tmp)}
  
  index <- must
  if(method == 'aic'){
    aic0 <- extractAIC(lm(Y~., data = as.data.frame(cbind(Y,X[,index]))))[2]
    for(i in order(r2, decreasing = T)){
      if(length(index) == 0){index <- i}
      if(i %in% index) {next}
      lm1 <- lm(Y~., data = as.data.frame(cbind(Y, X[,c(index,i)])))
      if(max(vif(lm1)) > vif){next}
      aic1 <- extractAIC(lm1)[2]
      if(aic1 < aic0){
        index <- c(index,i)
        aic0 <- aic1}}
  }
  if(method == 'adj.r2'){
    lm0 <- lm(Y~., data = as.data.frame(cbind(Y,X[,index])))
    r2_0 <- summary(lm0)$adj.r.squared
    for(i in order(r2, decreasing = T)){
      if(length(index) == 0){index <- i}
      if(i %in% index) {next}
      lm1 <- lm(Y~., data = as.data.frame(cbind(Y, X[,c(index,i)])))
      if(max(vif(lm1)) > vif){next}
      r2_1 <- summary(lm1)$adj.r.squared
      if(r2_1 > r2_0){
        index <- c(index,i)
        r2_0 <- r2_1}}
  }
  
  
  model <- lm(Y~., data = as.data.frame(cbind(Y, X[,index])))
  return(list(model=model,index=index))
}

m <- Model(dat_beer$revenue,
           dat_beer[-1],
           vif=5,
           must = ncol(dat_beer)-1,
           method = 'aic'
)

mean(abs(m$model$residuals/dat_beer$revenue))
summary(m$model)
vif(m$model)

df <- data.frame(time=t,
                 actual=dat_beer$revenue,
                 pred=m$model$fitted.values)
df <- melt(df, id='time')
ggplot(df, aes(time, value, color=variable)) + geom_line()

# tmp <- dat_beer[,m$index+1]
# names(tmp) <- ''
# corr <- cor(tmp)
# corrplot.mixed(corr, diag = 'n', tl.pos = 'lt')

index <- m$index[-c(6,7)]
lm1 <- lm(revenue~., data = dat_beer[,c(0,index)+1])
mean(abs(lm1$residuals/dat_beer$revenue))
summary(lm1)
vif(lm1)

pred <- predict(lm1,X_all[X_all$time == '2017 Q2',index+1],se.fit = T,interval = 'confidence')
df <- data.frame(time=t,
                 actual=dat_beer$revenue,
                 pred=lm1$fitted.values)
# df <- melt(df, id='time')
# ggplot(df, aes(time, value, color=variable)) + geom_line()
# write.csv(df, file = 'ơ����ҵӪ��.csv')
# sink('ơ����ҵӪ��ģ��.txt')
# summary(lm1)
# lm1$coefficients
# print('��������:')
# pred$fit
# print('��׼��:')
# pred$se.fit
# sink()

