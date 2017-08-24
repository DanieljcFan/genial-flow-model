library(ggplot2)
library(lubridate) # date management
library(zoo) #date management
library(corrplot) #covariance visualization
library(glmnet) #lasso regression
library(car) #vif detection
library(reshape2) #mdataframe reshape
#load company report
load("I:/work/genial-flow/report.rda")
index <- grep('ˮ��', report$�������)
cement <- report[index,]
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

netin <- report_clean(cement$corp_name, cement$report_period, cement$������)
netin[netin == 0] <- NA
netin <- netin[order(netin$time),]
netin <- aggregate(netin$y, by=list(as.yearqtr(netin$time)), mean, na.rm = T)
names(netin) <- c('time', 'netin')
netin

setwd("I:/work/genial-flow/ˮ��")
chain <- read.csv('ˮ��1.csv')
names(chain)[1] <- 'time'
chain$time <- as.POSIXlt(chain$time, format = '%Y/%m/%d')

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
chain_season <- Ave(chain[, -grep('��[����]', names(chain))], ac=F)
tmp <- Ave(chain[, c(1,grep('��[����]', names(chain)))], ac=T)
chain_season <- merge(chain_season, tmp, by='time')

#��ʡ��������
index <- grep('����.Ҥ��ֽ�Ҥˮ������', names(chain_season))
chain_season$Ҥ��ֽ�Ҥˮ�����ϲ��� <- rowSums(chain_season[,index])
chain_season <- chain_season[,-index]
#��ʡ��������
index <- grep('����.ԤӦ��������׮', names(chain_season))
chain_season$ԤӦ��������׮���� <- rowSums(chain_season[,index])
chain_season <- chain_season[,-index]
#��ʡ��������
index <- grep('����.ˮ����������', names(chain_season))
chain_season$ˮ���������˲��� <- rowSums(chain_season[,index])
chain_season <- chain_season[,-index]
#��ʡ��������
index <- grep('����.ˮ���������ˮ��', names(chain_season))
chain_season$ˮ���������ˮ�ܲ��� <- rowSums(chain_season[,index])
chain_season <- chain_season[,-index]
#��ʡ��������
index <- grep('����.��Ʒ������', names(chain_season))
chain_season$��Ʒ���������� <- rowSums(chain_season[,index])
chain_season <- chain_season[,-index]
#��ʡ��������
index <- grep('����.ˮ������', names(chain_season))
chain_season$ˮ�����ϲ��� <- rowSums(chain_season[,index])
chain_season <- chain_season[,-index]
#��ʡ��������
index <- grep('����.ˮ��', names(chain_season))
chain_season$ˮ����� <- rowSums(chain_season[,index])
chain_season <- chain_season[,-index]

#ɾ��2017Q2ȱʧ�ı���
chain_season[chain_season == 0] <- NA
index <- which(is.na(chain_season[chain_season$time == '2017 Q2',]))
chain_season <- chain_season[,-index]

# #ɾ����ͬ��ɽ����ú��
# chain_season <- chain_season[,-grep('ɽ��|��ͬ', names(chain_season))]
# #ɾ������ˮ����
# chain_season <- chain_season[,-grep('����', names(chain_season))]
# #ˮ�������
# index <- grep('����.�ۼ�ֵ|����.�ۼ�ֵ', names(chain_season))
# chain_season$ˮ������� <- chain_season[,index[2]]/chain_season[,index[1]]#��/��
# chain_season <- chain_season[,-index]
# #ɾ��ˮ��۸�ָ��
# chain_season <- chain_season[,-grep('ˮ��۸�ָ��', names(chain_season))]
# #����������С��Ӻ�
# index <- c(6:9)
# chain_season$����ˮ����С�� <- rowSums(chain_season[,index])
# chain_season <- chain_season[,-index]
# #ɾ������ˮ������
# chain_season <- chain_season[,-grep('��������.ˮ��.����ֵ', names(chain_season))]
# #ɾ��Ҥ��ֽ�Ҥˮ�����ϲ���  
# chain_season <- chain_season[,-grep('Ҥ��ֽ�Ҥˮ�����ϲ���', names(chain_season))]
# chain_season <- chain_season[year(chain_season$time) >= 2010, ]
# chain_season[chain_season == 0] <- NA


#merge X and Y ####
y_lag1 <- data.frame(time=netin$time + 0.25, y_lag1=netin$netin)
X <- merge(y_lag1, chain_season)
X[X==0] <- NA
na1 <- apply(X, 1, function(x){sum(is.na(x))>0.5})
X <- X[!na1,]
dat_cement <- merge(netin, X, by='time')
t <- dat_cement[,1]
dat_cement <- dat_cement[,-1]

#model#####
# Y <- dat_cement$netin
# Y_lag1 <- dat_cement$y_lag1
# X <- dat_cement[-c(1,2)]
# vif=15

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

m <- Model(dat_cement$netin,
           dat_cement[-1],
           method = 'aic'
)

mean(abs(m$model$residuals/dat_cement$netin))
summary(m$model)
vif(m$model)

df <- data.frame(time=t,
                 actual=dat_cement$netin,
                 pred=m$model$fitted.values)
df <- melt(df, id='time')
ggplot(df, aes(time, value, color=variable)) + geom_line()

sink('ˮ����ҵ����.txt')
summary(m$model)
print(m$model$coefficients)
sink()

predict(m$model,X[X == '2017 Q2',m$index+1], se.fit = T, interval = 'confidence')
df <- data.frame(time=t,
                 actual=dat_cement$netin,
                 pred=m$model$fitted.values)
# df <- melt(df, id='time')
# ggplot(df, aes(time, value, color=variable)) + geom_line()
write.csv(df, file = 'ˮ����ҵģ�����.csv')


#######

dat_cement <- merge(netin, chain_season)
na1 <- apply(dat_cement, 1, function(x){sum(is.na(x))>0.5})
t <- dat_cement[,1]
dat_cement <- dat_cement[,-1]

tmp <- dat_cement[!na1,-1]
names(tmp) <- ''
corr <- cor(tmp)
corrplot.mixed(corr, diag = 'n', tl.pos = 'lt')
names(dat_cement)[-1]

lm0 <- lm(netin~., dat_cement)
summary(lm0)
lm_a <- step(lm0)
summary(lm_a)
vif(lm_a)

# sink('ˮ����ҵ����.txt')
# summary(lm_a)
# print(lm_a$coefficients)
# sink()

index <- which(names(chain_season) %in% names(lm_a$model))
pre <- predict(lm_a,chain_season[chain_season$time == '2017 Q2',index])

df <- data.frame(time=t[!na1],
                 actual=dat_cement$netin[!na1],
                 pred=lm_a$fitted.values)
# df <- melt(df, id='time')
# ggplot(df, aes(time, value, color=variable)) + geom_line()
df <- rbind(df,c('', '', ''))
# write.csv(df, file = 'ˮ����ҵģ�����.csv')


