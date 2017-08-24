library(ggplot2)
library(lubridate) # date management
library(zoo) #date management
library(corrplot) #covariance visualization
library(glmnet) #lasso regression
library(car) #vif detection
library(reshape2) #mdataframe reshape
#load company report
load("I:/work/genial-flow/report.rda")
index <- grep('水泥', report$申万二级)
cement <- report[index,]
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

netin <- report_clean(cement$corp_name, cement$report_period, cement$净利润)
netin[netin == 0] <- NA
netin <- netin[order(netin$time),]
netin <- aggregate(netin$y, by=list(as.yearqtr(netin$time)), mean, na.rm = T)
names(netin) <- c('time', 'netin')
netin

setwd("I:/work/genial-flow/水泥")
chain <- read.csv('水泥1.csv')
names(chain)[1] <- 'time'
chain$time <- as.POSIXlt(chain$time, format = '%Y/%m/%d')

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

#各省产量加总
index <- grep('产量.窑外分解窑水泥熟料', names(chain_season))
chain_season$窑外分解窑水泥熟料产量 <- rowSums(chain_season[,index])
chain_season <- chain_season[,-index]
#各省产量加总
index <- grep('产量.预应力混凝土桩', names(chain_season))
chain_season$预应力混凝土桩产量 <- rowSums(chain_season[,index])
chain_season <- chain_season[,-index]
#各省产量加总
index <- grep('产量.水泥混凝土电杆', names(chain_season))
chain_season$水泥混凝土电杆产量 <- rowSums(chain_season[,index])
chain_season <- chain_season[,-index]
#各省产量加总
index <- grep('产量.水泥混凝土排水管', names(chain_season))
chain_season$水泥混凝土排水管产量 <- rowSums(chain_season[,index])
chain_season <- chain_season[,-index]
#各省产量加总
index <- grep('产量.商品混凝土', names(chain_season))
chain_season$商品混凝土产量 <- rowSums(chain_season[,index])
chain_season <- chain_season[,-index]
#各省产量加总
index <- grep('产量.水泥熟料', names(chain_season))
chain_season$水泥熟料产量 <- rowSums(chain_season[,index])
chain_season <- chain_season[,-index]
#各省产量加总
index <- grep('产量.水泥', names(chain_season))
chain_season$水泥产量 <- rowSums(chain_season[,index])
chain_season <- chain_season[,-index]

#删除2017Q2缺失的变量
chain_season[chain_season == 0] <- NA
index <- which(is.na(chain_season[chain_season$time == '2017 Q2',]))
chain_season <- chain_season[,-index]

# #删除大同和山西的煤价
# chain_season <- chain_season[,-grep('山西|大同', names(chain_season))]
# #删除出口水泥量
# chain_season <- chain_season[,-grep('出口', names(chain_season))]
# #水泥产销比
# index <- grep('产量.累计值|销量.累计值', names(chain_season))
# chain_season$水泥产销比 <- chain_season[,index[2]]/chain_season[,index[1]]#销/产
# chain_season <- chain_season[,-index]
# #删除水泥价格指数
# chain_season <- chain_season[,-grep('水泥价格指数', names(chain_season))]
# #进口数量四小项加和
# index <- c(6:9)
# chain_season$进口水泥四小项 <- rowSums(chain_season[,index])
# chain_season <- chain_season[,-index]
# #删除进口水泥数量
# chain_season <- chain_season[,-grep('进口数量.水泥.当月值', names(chain_season))]
# #删除窑外分解窑水泥熟料产量  
# chain_season <- chain_season[,-grep('窑外分解窑水泥熟料产量', names(chain_season))]
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

sink('水泥行业利润.txt')
summary(m$model)
print(m$model$coefficients)
sink()

predict(m$model,X[X == '2017 Q2',m$index+1], se.fit = T, interval = 'confidence')
df <- data.frame(time=t,
                 actual=dat_cement$netin,
                 pred=m$model$fitted.values)
# df <- melt(df, id='time')
# ggplot(df, aes(time, value, color=variable)) + geom_line()
write.csv(df, file = '水泥行业模型拟合.csv')


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

# sink('水泥行业利润.txt')
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
# write.csv(df, file = '水泥行业模型拟合.csv')


