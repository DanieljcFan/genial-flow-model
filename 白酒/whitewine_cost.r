library(ggplot2)
library(lubridate) # date management
library(zoo) #date management
library(corrplot) #covariance visualization
library(glmnet) #lasso regression
library(car) #vif detection
library(reshape2) #mdataframe reshape
#load company report####
#dataframe重命名：行业、财务指标
#修改从报表中提取的指标
load("E:/FJC/report.rda")
index <- grep('白酒', report$申万三级)
whitewine <- report[index,]
report_clean <- function(name, time, y){
  #report_clean 函数处理公司季报，计算各季度财务指标的变化。对于季报残缺不齐的用平均法补齐
  #默认报告期为每季度末，输出后会将所有31号改为30号
  #函数输入 name 公司名 time 报表日（POSIXlt，包含年月日） y 需计算的财务指标（numeric）
  #函数输出 data.frame： name time y
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
  index_2 <- index_2[!is.na(index_2)]
  index_3 <- index_3[!is.na(index_3)]
  
  time[index_1] <- time[index_1] - months(3)
  time[index_2] <- time[index_2] - months(6)
  time[index_3] <- time[index_3] - months(9)
  
  de_y <- data.frame(name, time, y)
  return(de_y)
}

cost <- report_clean(whitewine$corp_name, whitewine$report_period, whitewine$营业总成本)
cost[cost == 0] <- NA
cost <- cost[order(cost$time),]
cost <- aggregate(cost$y, by=list(as.yearqtr(cost$time)), mean, na.rm = T)
names(cost) <- c('time', 'cost')
# cost

#load chain factors######
#更改路径至该行业的文件夹
#读入数据，需对Excel表做预处理
#注意时间列的格式（‘/’分割 or ‘-’分割）
setwd("E:/FJC/白酒/")
chain <- read.csv('白酒1.csv')
names(chain)[1] <- 'time'
chain$time <- as.POSIXlt(chain$time, format = '%Y-%m-%d')

Ave <- function(dat, t=1, type='season', ac = F){
  #ave函数对数据集中的指标按月或季度平均
  #默认时间标签在第一列，格式为POSIXlt，包含年月信息
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
chain_season <- Ave(chain[, -grep('累[积计]', names(chain))], ac=F)
tmp <- Ave(chain[, c(1,grep('累[积计]', names(chain)))], ac=T)
chain_season <- merge(chain_season, tmp, by='time')

#补充变量
tmp <- read.csv('白酒-农作物价格.csv')
names(tmp)[1] <- 'time'
tmp$time <- as.POSIXlt(tmp$time, format='%Y-%m-%d')
tmp <- Ave(tmp)
chain_season <- merge(chain_season, tmp, by='time')

# #各省产量加总
# index <- grep('产量.白酒', names(chain_season))
# chain_season$白酒产量 <- rowSums(chain_season[,index])
# chain_season <- chain_season[,-index]
# 
# #补充变量
# tmp <- read.csv('白酒收入.csv')
# names(tmp)[1] <- 'time'
# tmp$time <- as.POSIXlt(tmp$time, format='%Y/%m/%d')
# tmp1 <- Ave(tmp[,-grep('累[积计]', names(tmp))])
# chain_season <- merge(chain_season, tmp1, by='time')
# tmp <- Ave(tmp[,c(1,grep('累[积计]', names(tmp)))], ac=T)
# chain_season <- merge(chain_season, tmp)


#删除缺失的变量
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
X_lag4 <- X
X_lag4$time <- X_lag4$time + 1 
names(X_lag4)[-1] <- paste0(names(X)[-1],'_lag4')

X_all <- merge(merge(X, X_lag1),X_lag4)
y_lag1 <- data.frame(time=cost$time + 0.25, y_lag1=cost$cost)
y_lag2 <- data.frame(time=cost$time + 0.5, y_lag2=cost$cost)
y_lag3 <- data.frame(time=cost$time + 0.75, y_lag3=cost$cost)
y_lag4 <- data.frame(time=cost$time + 1, y_lag4=cost$cost)
X_all <- merge(y_lag1,merge(y_lag2,merge(y_lag3,merge(y_lag4,X_all))))
X_all$season <- as.factor(quarters(X_all$time))

dat_whitewine <- merge(cost,X_all)
t <- dat_whitewine[,1]
dat_whitewine <- dat_whitewine[,-1]

#model#####

#model函数通过marginal-r2筛选自变量，利用vif限制自相关性
#must为手动设定的必须加入模型的变量列号
#输出$mode为模型，$index为入选模型的自变量列号
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

m <- Model(dat_whitewine$cost,
           dat_whitewine[-1],
           vif=5,
           must = ncol(dat_whitewine)-1,
           method = 'aic'
)

mean(abs(m$model$residuals/dat_whitewine$cost))
summary(m$model)
vif(m$model)

df <- data.frame(time=t,
                 actual=dat_whitewine$cost,
                 pred=m$model$fitted.values)
df <- melt(df, id='time')
ggplot(df, aes(time, value, color=variable)) + geom_line()

# tmp <- dat_whitewine[,m$index+1]
# names(tmp) <- ''
# corr <- cor(tmp)
# corrplot.mixed(corr, diag = 'n', tl.pos = 'lt')

index <- m$index[-c(4,6)]
lm1 <- lm(cost~., data = dat_whitewine[,c(0,index)+1])
mean(abs(lm1$residuals/dat_whitewine$cost))
summary(lm1)
vif(lm1)

pred <- predict(lm1,X_all[X_all$time == '2017 Q2',index+1],se.fit = T,interval = 'confidence')
df <- data.frame(time=t,
                 actual=dat_whitewine$cost,
                 pred=lm1$fitted.values)
# df <- melt(df, id='time')
# ggplot(df, aes(time, value, color=variable)) + geom_line()
# write.csv(df, file = '白酒行业成本.csv')
# sink('白酒行业成本模型.txt')
# summary(lm1)
# lm1$coefficients
# print('置信区间:')
# pred$fit
# print('标准差:')
# pred$se.fit
# sink()

