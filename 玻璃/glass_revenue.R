library(ggplot2)
library(lubridate) # date management
library(zoo) #date management
library(corrplot) #covariance visualization
library(glmnet) #lasso regression
library(car) #vif detection
library(reshape2) #mdataframe reshape
#load company report####
load("I:/work/genial-flow/report.rda")
#company revenue
index <- grep('玻璃', report$申万二级)
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

revenue <- report_clean(glass$corp_name, glass$report_period, glass$营业总收入)
revenue <- revenue[order(revenue$time),]
revenue <- aggregate(revenue$y, by=list(as.yearqtr(revenue$time)), mean)
names(revenue) <- c('time', 'revenue')
revenue
#chain factor ####
setwd("I:/work/genial-flow/玻璃")
chain <- read.csv('玻璃数据1.csv')
names(chain)[1] <- 'time'
chain$time <- as.POSIXlt(chain$time, format = '%Y-%m-%d')

#ave函数对数据集中的指标按月或季度平均
#默认时间标签在第一列，格式为POSIXlt，包含年月信息
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

chain_season <- Ave(chain[, -grep('累[积计]', names(chain))], ac=F)
tmp <- Ave(chain[, c(1,grep('累[积计]', names(chain)))], ac=T)
chain_season <- merge(chain_season, tmp, by='time')

#纯碱数据不全，另行补充
chain_season <- chain_season[ ,-grep('碱', names(chain_season))]
tmp <- read.csv('玻璃数据2-纯碱.csv')
names(tmp)[1] <- 'time'
tmp$time <- as.POSIXlt(tmp$time, format = '%Y-%m-%d')
tmp <- Ave(tmp)
#四地纯碱价格平均
tmp$纯碱市场价 <- rowMeans(tmp[,3:6])
tmp <- tmp[,-c(3:6)]
chain_season <- merge(chain_season, tmp, by='time')
#房屋销售需提前两季度，另行补充
chain_season <- chain_season[ ,-grep('商品房销售', names(chain_season))]
tmp <- read.csv('房地产开发销售(月).csv')
names(tmp)[1] <- 'time'
tmp$time <- as.POSIXlt(tmp$time, format = '%Y-%m-%d')
#提前两个季度,并防止出现2月30
tmp$time <- tmp$time -days(3) + months(6)
tmp <- Ave(tmp)
chain_season <- merge(chain_season, tmp, by='time')
#白玉石指标无效，剔除
chain_season <- chain_season[,-grep('白云', names(chain_season))]
#丰田汽车数据2015 2016缺失，剔除
chain_season <- chain_season[,-grep('丰田', names(chain_season))]
#浮法玻璃平均价三地平均
index <- grep('现货平均价.平方米..浮法玻璃', names(chain_season))
chain_season$浮法玻璃平均价 <- rowMeans(chain_season[,index])
chain_season <- chain_season[,-index]

#补充指标
tmp <- read.csv('玻璃.csv')
names(tmp)[1] <- 'time'
tmp$time <- as.POSIXlt(tmp$time, format = '%Y-%m-%d')
tmp1 <- Ave(tmp[,-grep('累[积计]', names(tmp))])
chain_season <- merge(chain_season, tmp1)
tmp <- Ave(tmp[, c(1,grep('累[积计]', names(tmp)))], ac=T)
chain_season <- merge(chain_season, tmp)

chain_season[chain_season == 0] <- NA
na1 <- apply(chain_season,2, function(x){sum(is.na(x))})
chain_season <- chain_season[, -which(na1>0.5)]


#merge X and Y ####
y_lag4 <- data.frame(time=revenue$time + 1, y_lag4=revenue$revenue)
X <- merge(y_lag4, chain_season)
dat_glass <- merge(revenue, X, by='time')
t <- dat_glass[,1]
dat_glass <- dat_glass[,-1]

# #model#####
# Y <- dat_glass$revenue
# X <- dat_glass[-c(1,2)]
# vif=15

Model <- function(Y,X, must=c(),vif=15, method=c('aic','adj.r2')){
  library(car)
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

m <- Model(dat_glass$revenue,
           dat_glass[-1],
           vif = 15,
           method = 'aic'
)

mean(abs(m$model$residuals/dat_glass$revenue))
summary(m$model)
vif(m$model)

df <- data.frame(time=t,
                 actual=dat_glass$revenue,
                 pred=m$model$fitted.values)
df <- melt(df, id='time')
ggplot(df, aes(time, value, color=variable)) + geom_line()

index <- m$index
lm1 <- lm(revenue~., data = dat_glass[,c(0,index)+1])
mean(abs(lm1$residuals/dat_glass$revenue))
summary(lm1)
vif(lm1)

pred <- predict(lm1,X[X$time == '2017 Q2',index+1],se.fit = T,interval = 'confidence')
df <- data.frame(time=t,
                 actual=dat_glass$revenue,
                 pred=lm1$fitted.values)
# df <- melt(df, id='time')
# ggplot(df, aes(time, value, color=variable)) + geom_line()
# write.csv(df, file = '玻璃行业营收.csv')
# 
# sink('玻璃行业营收模型.txt')
# summary(lm1)
# lm1$coefficients
# print('置信区间:')
# pred$fit
# print('标准差:')
# pred$se.fit
# sink()

