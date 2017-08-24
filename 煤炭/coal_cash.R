library(ggplot2)
library(corrplot)
setwd("I:/work/genial-flow/煤炭/")
coal <- read.csv('煤炭开采行业财务数据.csv')
coal$报告期 <- as.POSIXlt(coal$报告期, format='%Y/%m/%d')
#report_clean 函数处理公司季报，计算各季度财务指标的变化。对于季报残缺不齐的用平均法补齐
#默认报告期为每季度末，输出后会将所有31号改为30号
#函数输入 name 公司名 time 报表日（POSIXlt，包含年月日） y 需计算的财务指标（numeric）
#函数输出 data.frame： name time y
report_clean <- function(name, time, y){
  library(lubridate) # date management
  if(class(time)[1] != 'POSIXlt') return('Wrong: required POSIXlt for time')
  day(time) <- 30
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

cash <- report_clean(coal$公司名称, coal$报告期, coal$经营活动现金净流量)
cash <- cash[order(cash$time),]
cash <- aggregate(cash$y, by=list(cash$time), mean)
names(cash) <- c('time', 'cash')
cash[,1] <- as.yearqtr(cash[,1])

netin <- report_clean(coal$公司名称, coal$报告期, coal$净利润)
netin <- netin[order(netin$time),]
netin <- aggregate(netin$y, by=list(netin$time), mean)
names(netin) <- c('time', 'netin')
netin[,1] <- as.yearqtr(netin[,1])




df <- merge(cash,netin)
df <- melt(df, id='time')
ggplot(df, aes(time, value, color=variable)) + geom_line()


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

chain <- read.csv('产业链因子0808.csv')
names(chain)[1] <- 'time' 
chain$time <- as.POSIXlt(chain$time, format='%Y/%m/%d')

#12-15 原煤产量
chain$原煤产量 <- rowSums(chain[,12:15])
chain <- chain[,-c(12:15)]
#3螺纹钢删掉
chain <- chain[,-3]
chain_season <- Ave(chain)


tmp <- read.csv('煤炭数据5-尿素.csv')
names(tmp)[1] <- 'time'
tmp$time <- as.POSIXlt(tmp$time, format='%Y/%m/%d')
tmp <- Ave(tmp)
tmp$尿素平均价 <- rowMeans(tmp[,2:6])

chain_season <- merge(chain_season, tmp[,c(1,7)], by='time')


p_coal <- read.csv('煤炭数据6.csv')
names(p_coal)[1] <- 'time'
p_coal$time <- as.POSIXlt(p_coal$time, format = '%Y/%m/%d')
p_coal <- Ave(p_coal)
#检验之后选用9，"秦皇岛港.平仓价.山西优混.Q5500K."
# p_coal <- merge(cash[,1:2], p_coal, by='time')
# p_coal <- p_coal[,-1]
# 
# sst <- var(p_coal$ave)
# r_sq <- rep(0, ncol(p_coal)-1)
# for(i in 2:ncol(p_coal)){
# r_sq[i-1] <- 1-var(lm(ave~., p_coal[,c(1,i)])$residuals)/sst
# }
# names(r_sq) <- names(p_coal)[-1]
# 
# r_sq[order(r_sq, decreasing = T)]
p_coal <- p_coal[,c(1,9)]
chain_season <- merge(chain_season, p_coal, by='time')

#merge X and Y ####
netin_lag1 <- data.frame(time=netin$time + 0.25, netin_lag1=netin$netin)
X <- merge(netin_lag1, chain_season)
y_lag1 <- data.frame(time=cash$time + 1, y_lag1=cash$cash)
X <- merge(y_lag1, X)

dat_coal <- merge(cash, X, by='time')
na1 <- apply(dat_coal, 1, function(x){sum(is.na(x))>0.5})
t <- dat_coal[,1]
dat_coal <- dat_coal[!na1,-1]

#model#####
# Y <- dat_coal$cash
# Y_lag1 <- dat_coal$y_lag1
# X <- dat_coal[-c(1,2)]

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
      if(r2_1 > r2_0){
        index <- c(index,i)
        r2_0 <- r2_1}}
  }
  
  
  model <- lm(Y~., data = as.data.frame(cbind(Y,Y_lag1, X[,index])))
  return(list(model=model,index=index))
  
}

m <- Model(dat_coal$cash,
           dat_coal$y_lag1,
           dat_coal[-c(1,2)],
           vif = 25,
           method = 'adj.r2'
)

mean(abs(m$model$residuals/dat_coal$cash))
summary(m$model)
vif(m$model)

df <- data.frame(time=t,
                 actual=dat_coal$cash,
                 pred=m$model$fitted.values)
df <- melt(df, id='time')
ggplot(df, aes(time, value, color=variable)) + geom_line()






lm0 <- lm(cash~., dat_coal)
lm_a <- step(lm0)
summary(lm0)
summary(lm_a)
vif(lm_a)

df <- data.frame(time=t[!na1],
                 actual=dat_coal$cash[!na1],
                 pred=lm_a$fitted.values)
df <- melt(df, id='time')
ggplot(df, aes(time, value, color=variable)) + geom_line()



sink('煤炭企业经营活动现金流.txt')
summary(lm_a)
print(lm_a$coefficients)
sink()



df <- data.frame(time=t[!na1],
                 actual=dat_coal$cash[!na1],
                 pred=lm_a$fitted.values)
write.csv(df, file = '煤炭行业现金流模型拟合.csv')


index <- which(names(X) %in% names(lm_a$model)[-1])
predict(lm_a,X[X$time == '2017 Q2',index])
