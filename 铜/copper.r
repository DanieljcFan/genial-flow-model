library(ggplot2)
library(lubridate) # date management
library(zoo) #date management
library(corrplot) #covariance visualization
library(glmnet) #lasso regression
library(car) #vif detection
library(reshape2) #mdataframe reshape
#load company report####
load("I:/work/genial-flow/report.rda")
index <- grep('铜', report$申万三级)
copper <- report[index,]
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
  index_2 <- index_1[!is.na(index_2)]
  index_3 <- index_1[!is.na(index_3)]
  
  time[index_1] <- time[index_1] - months(3)
  time[index_2] <- time[index_2] - months(6)
  time[index_3] <- time[index_3] - months(9)
  
  de_y <- data.frame(name, time, y)
  return(de_y)
}

netin <- report_clean(copper$corp_name, copper$report_period, copper$净利润)
netin[netin == 0] <- NA
netin <- netin[order(netin$time),]
netin <- aggregate(netin$y, by=list(as.yearqtr(netin$time)), mean, na.rm = T)
names(netin) <- c('time', 'netin')
netin
#chain factor####
setwd("I:/work/genial-flow/铜")
chain <- read.csv('铜数据1.csv')
names(chain)[1] <- 'time'
chain$time <- as.POSIXlt(chain$time, format = '%Y-%m-%d')

#ave函数对数据集中的指标按月或季度平均
#默认时间标签在第一列，格式为POSIXlt，包含年月信息
Ave <- function(dat, t=1, type='season', ac = F){
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
#空调、电冰箱产量加总
index <- grep('空调|电冰箱', names(chain_season))
chain_season$家电产量 <- rowSums(chain_season[,index])
chain_season <- chain_season[,-index]
#粗炼费，精炼费加总
index <- grep('中国铜冶炼厂', names(chain_season))
chain_season$冶炼费 <- rowSums(chain_season[,index])
chain_season <- chain_season[,-index]
#出口铜材，铜制品加总
index <- c(8,9)
chain_season$出口铜材 <- rowSums(chain_season[,index])
chain_season <- chain_season[,-index]
#进口精细铜加总
index <- c(14:19)
chain_season$进口精炼铜 <- rowSums(chain_season[,index])
chain_season <- chain_season[,-index]
#进口铜材加总
index <- c(8:13)
chain_season$进口铜材 <- rowSums(chain_season[,index])
chain_season <- chain_season[,-index]
#删除COMEX.铜.库存
chain_season <- chain_season[ ,-grep('COMEX', names(chain_season))]
#全球铜矿产量，电力行业收入一年一报，删除
index <- c(5,8)
chain_season <- chain_season[,-index]

#废铜价格平均
tmp <- read.csv('铜数据4.csv')
names(tmp)[1] <- 'time'
tmp$time <- as.POSIXlt(tmp$time, format = '%Y-%m-%d')
tmp <- Ave(tmp)
tmp$废铜价格 <- rowMeans(tmp[,-1])
tmp <- tmp[,c(1,length(tmp))]
chain_season <- merge(chain_season, tmp)

#发电量
tmp <- read.csv('铜数据3.csv')
names(tmp)[1] <- 'time'
tmp$time <- as.POSIXlt(tmp$time, format = '%Y-%m-%d')
tmp <- Ave(tmp)
chain_season <- merge(chain_season, tmp)
#焦煤价格
tmp <- read.csv('国内炼焦煤价格.csv')
names(tmp)[1] <- 'time'
tmp$time <- as.POSIXlt(tmp$time, format = '%Y-%m-%d')
tmp <- Ave(tmp)
chain_season <- merge(chain_season, tmp)
#阴极铜价格
tmp <- read.csv('阴极铜.csv')
names(tmp)[1] <- 'time'
tmp$time <- as.POSIXlt(tmp$time, format = '%Y-%m-%d')
tmp <- Ave(tmp)
chain_season <- merge(chain_season, tmp)
#铜矿数据
tmp <- read.csv('铜矿数据.csv')
names(tmp)[1] <- 'time'
tmp$time <- as.POSIXlt(tmp$time, format = '%Y-%m-%d')
tmp <- Ave(tmp)
chain_season <- merge(chain_season, tmp)


#merge X and Y ####
y_lag1 <- data.frame(time=netin$time + 0.25, y_lag1=netin$netin)
X <- merge(y_lag1, chain_season)
dat_copper <- merge(netin, X, by='time')
na1 <- apply(dat_copper, 1, function(x){sum(is.na(x))>0.5})
t <- dat_copper[!na1,1]
dat_copper <- dat_copper[!na1,-1]

#model#####
# Y <- dat_cement$netin
# Y_lag1 <- dat_cement$y_lag1
# X <- dat_cement[-c(1,2)]
# vif=15

Model <- function(Y,Y_lag1,X, must=c(),vif=15, method=c('aic','adj.r2')){
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

m <- Model(dat_copper$netin,
           dat_copper$y_lag1,
           dat_copper[-c(1,2)],
           vif = 15,
           method = 'aic'
)

mean(abs(m$model$residuals/dat_copper$netin))
summary(m$model)
vif(m$model)

df <- data.frame(time=t,
                 actual=dat_copper$netin,
                 pred=m$model$fitted.values)
df <- melt(df, id='time')
ggplot(df, aes(time, value, color=variable)) + geom_line()



########
dat_copper <- merge(netin, chain_season, by='time')
t <- dat_copper[,1]
dat_copper <- dat_copper[,-1]

dat_copper[dat_copper == 0] <- NA
na1 <- apply(dat_copper,1, function(x){sum(is.na(x))>0.5})

tmp <- dat_copper[,-1]
names(tmp) <- ''
corr <- cor(tmp)
corrplot.mixed(corr, diag = 'n', tl.pos = 'lt')

lm0 <- lm(netin~., data = dat_copper)
lm_a <- step(lm0)
summary(lm_a)
vif(lm_a)
index <- which(names(dat_copper) %in% names(lm_a$model))
names(dat_copper)[index]
lm1 <- lm(netin~., dat_copper[,index[-c(3:5,11,13)]]) 
summary(lm1)
vif(lm1)

# sst <- var(dat_copper$netin)
# r2 <- c()
# for(i in 1:ncol(dat_copper)){
#   tmp <- 0
#   if(i %in% index[-c(2,5:11)]){r2 <- c(r2,tmp);next}
#   lm_t <- lm(netin~., dat_copper[,c(i,index[-c(2,5:11)])])
#   tmp <- 1-var(lm_t$residuals)/sst
#   r2 <- c(r2, tmp)
# }



# sink('铜利润模型.txt')
# summary(lm1)
# lm1$coefficients
# sink()

df <- data.frame(time=t[!na1],
                 actual=dat_copper$netin[!na1],
                 pred=lm_a$fitted.values)
# df <- melt(df, id='time')
# ggplot(df, aes(time, value, color=variable)) + geom_line()
# write.csv(df, file = '铜行业模型拟合.csv')

index <- which(names(chain_season) %in% names(lm1$model))
predict(lm1,chain_season[chain_season$time == '2017 Q2',index],se.fit = T, interval = 'confidence')

