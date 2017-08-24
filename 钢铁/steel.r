library(ggplot2)
library(lubridate) # date management
library(zoo) #date management
library(corrplot)
library(reshape2)
library(car)
setwd("I:/work/genial-flow/����/")
load('steel.rda')

#report_clean ��������˾��������������Ȳ���ָ��ı仯�����ڼ�����ȱ�������ƽ��������
#Ĭ�ϱ�����Ϊÿ����ĩ�������Ὣ����31�Ÿ�Ϊ30��
#�������� name ��˾�� time �����գ�POSIXlt�����������գ� y �����Ĳ���ָ�꣨numeric��
#������� data.frame�� name time y
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

netin <- report_clean(steel$corp_name, steel$report_period, steel$������)
netin <- netin[order(netin$time),]
netin <- aggregate(netin$y, by=list(as.yearqtr(netin$time)), mean)
names(netin) <- c('time', 'netin')

###
# name <- steel$corp_name
# time <- steel$report_period
# y <- steel$������
###

chain <- read.csv('��������3.csv')
names(chain)[1] <- 'time'
chain$time <- as.POSIXlt(chain$time, format = '%Y-%m-%d')
#str(chain)

#ɾ��ȫ��Ϊ�յ���
na1 <- apply(chain, 2, function(x) sum(is.na(x)))
chain <- chain[,-which(na1 == nrow(chain))]



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

chain_season <- Ave(chain[, -grep('[��]', names(chain))], ac=F)

#���ݴ���
#�ϸ֣���ɽ���ݲ��㣬������ɽ
#����ʯ���Ĵ������Ӻ�
chain_season$����ʯ���� <- rowSums(chain_season[,40:43])
#��ģ���Ϲ�ҵ��ֵ ���룬ȥ����
#����ֲĿ�����
chain_season$��� <- rowSums(chain_season[,51:55])
#�������Ƹּ۸�ƽ��
chain_season$�������Ƹ� <- rowMeans(chain_season[57:59])

chain_season <- chain_season[,-c(21,40:43,48:55,57:59)]
#������������
tmp <- read.csv('������������.csv')
names(tmp)[1] <- 'time'
tmp$time <- as.POSIXlt(paste0(tmp$time,'-01'), format='%Y-%m-%d')
#str(tmp)
tmp <- Ave(tmp, ac= T)
tmp$���β�ҵ <- rowSums(tmp[,2:4])
chain_season <- merge(chain_season, tmp[,c(1,5)], by='time')
names(chain_season)
#�ۼ�ֵ
tmp <- Ave(chain[,c(1,grep('[��]', names(chain)))], ac=T)
#�̶��ʲ�����ɫ�������ѡҵ����ɫ����ұ����ѹ�Ӽӹ�ҵ����ǰ���꣬���м���
chain_season <- merge(chain_season, tmp[,-c(2:3)], by='time')
#������ǰ����Ĺ̶��ʲ�
tmp <- read.csv('�����̶��ʲ�Ͷ��.csv')
names(tmp)[1] <- 'time'
tmp$time <- as.POSIXlt(tmp$time, format='%Y-%m-%d')-days(1)+years(2)
tmp <- Ave(tmp, ac=T)
chain_season <- merge(chain_season, tmp, by='time')

chain_season[chain_season == 0] <- NA 
na1 <- apply(chain_season, 2, function(x) sum(is.na(x))>0.5)
chain_season <- chain_season[,!na1]

save(chain_season,netin, file = 'steel_XY.rda')

#merge X and Y ####
y_lag1 <- data.frame(time=netin$time + 0.25, y_lag1=netin$netin)
X <- merge(y_lag1, chain_season)
dat_steel <- merge(netin, X, by='time')
na1 <- apply(dat_steel, 1, function(x){sum(is.na(x))>0.5})
t <- dat_steel[,1]
dat_steel <- dat_steel[!na1,-1]

# #model#####
# Y <- dat_steel$netin
# Y_lag1 <- dat_steel$y_lag1
# X <- dat_steel[-c(1,2)]
# vif=15

Model <- function(Y,Y_lag1,X, must=c(),vif=15, method=c('aic','adj.r2')){
  library(car)
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
      lm1 <- lm(Y~., data = as.data.frame(cbind(Y,Y_lag1, X[,c(index,i)])))
      if(max(vif(lm1)) > vif){next}
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
      lm1 <- lm(Y~., data = as.data.frame(cbind(Y,Y_lag1, X[,c(index,i)])))
      if(max(vif(lm1)) > vif){next}
      r2_1 <- summary(lm1)$adj.r.squared
      if(r2_1 > r2_0 & summary(lm1)$coefficients[(length(index+1)),4] < 0.1){
        index <- c(index,i)
        r2_0 <- r2_1}}
  }
  
  
  model <- lm(Y~., data = as.data.frame(cbind(Y,Y_lag1, X[,index])))
  return(list(model=model,index=index))
  
}

m <- Model(dat_steel$netin,
           dat_steel$y_lag1,
           dat_steel[-c(1,2)],
           vif = 15,
           method = 'aic'
)

mean(abs(m$model$residuals/dat_steel$netin))
summary(m$model)
vif(m$model)

index <- which(names(dat_steel) %in% names(m$model$model) )
lm1 <- lm(netin~., data = dat_steel[,c(1,index[-3])])
summary(lm1)


df <- data.frame(time=t[!na1],
                 actual=dat_steel$netin,
                 pred=m$model$fitted.values)
df <- melt(df, id='time')
ggplot(df, aes(time, value, color=variable)) + geom_line()










# #####
# dat_steel <- merge(netin, chain_season, by='time')
# # for(i in 2:ncol(dat_steel)){
# #   print(qplot(c(1:length(dat_steel[,i])), dat_steel[,i], geom = 'point', main = names(dat_steel)[i]))
# # }
# 
# dat_steel[dat_steel == 0] <- NA
# na1 <- apply(dat_steel,2, function(x){sum(is.na(x))})
# #ȥ����ȱʧֵ�ı���������������ұ��
# dat_steel <- dat_steel[, -which(na1>0.5)[-17]]
# t <- dat_steel[,1]
# dat_steel <- dat_steel[,-1]
# #ɾ��Myspicϸ��ָ��
# dat_steel <- dat_steel[,-c(3,4,6:10)]
# 
# sst <- var(dat_steel$netin)
# #�����۸���ѡһ
# for(i in c(2,3,10,11)){
#   lm1 <- lm(netin~., data = dat_steel[,c(1,i)])
#   print(1 - var(lm1$residuals)/sst)
# }
# #�޳�3,11
# 
# #�ϸּ۸��ѡһ
# for(i in c(6,14)){
#   lm1 <- lm(netin~., data = dat_steel[,c(1,i)])
#   print(1 - var(lm1$residuals)/sst)
# }
# #�޳�6
# 
# #����ʯ��ѡ�������ڹ��⣩
# for(i in c(8,9,12,13)){
#   lm1 <- lm(netin~., data = dat_steel[,c(1,i)])
#   print(1 - var(lm1$residuals)/sst)
# }
# #�޳�8,9
# 
# dat_steel <- dat_steel[,-c(3,6,8,9,11)]
# 
# 
# 
# lm0 <- lm(netin~., data = dat_steel)
# lm_a <- step(lm0)
# lm_b <- step(lm0, scope = list(criterion = "BIC"))
# summary(lm_b)
# vif(lm_b)
# 
# sink('������ҵ����ģ��.txt')
# summary(lm_b)
# sink()
# 
# tmp <- dat_steel[-1]
# names(tmp)
# names(tmp) <- ''
# corr <- cor(tmp)
# corrplot.mixed(corr, tl.pos = 'n', diag = 'n')
# 
# 
# #�޳��������ı���
# index <- which(names(dat_steel) %in% names(lm_a$model))
# tmp <- index[-c(9,18,21,22,24)]
# lm1 <- lm(netin~., dat_steel[,tmp])
# print(1- var(lm1$residuals)/sst)
# summary(lm1)
# vif(lm1)
# 
# sink('������ҵ����ģ��.txt')
# summary(lm1)
# print(lm1$coefficients)
# sink()
# 
# df <- data.frame(time=t[-1],
#                  actual=dat_steel$netin[-1],
#                  pred=lm1$fitted.values)
# # df <- melt(df, id='time')
# # ggplot(df, aes(time, value, color=variable)) + geom_line()
# write.csv(df, file = '������ҵģ�����.csv')
# 
# index <- which(names(chain_season) %in% names(lm_a$model))
# predict(lm1,chain_season[chain_season$time == '2017 Q2',index])
# 
# 
# mean(abs(lm1$residuals/lm1$model$netin))
# 
# 
