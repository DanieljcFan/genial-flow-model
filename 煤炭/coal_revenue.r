library(ggplot2)
library(corrplot)
library(reshape2)
setwd("I:/work/genial-flow/ú̿/")
#revenue #####
coal <- read.csv('ú̿������ҵ��������.csv')
coal$������ <- as.POSIXlt(coal$������, format='%Y/%m/%d')
#report_clean ��������˾��������������Ȳ���ָ��ı仯�����ڼ�����ȱ�������ƽ��������
#Ĭ�ϱ�����Ϊÿ����ĩ�������Ὣ����31�Ÿ�Ϊ30��
#�������� name ��˾�� time �����գ�POSIXlt�����������գ� y �����Ĳ���ָ�꣨numeric��
#������� data.frame�� name time y
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

revenue <- report_clean(coal$��˾����, coal$������, coal$Ӫҵ������)
revenue <- revenue[order(revenue$time),]
revenue <- aggregate(revenue$y, by=list(revenue$time), mean)
names(revenue) <- c('time', 'revenue')
revenue[,1] <- as.yearqtr(revenue[,1])


#chain factor#######

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

chain <- read.csv('��ҵ������0808.csv')
names(chain)[1] <- 'time' 
chain$time <- as.POSIXlt(chain$time, format='%Y/%m/%d')

#12-15 ԭú����
chain$ԭú���� <- rowSums(chain[,12:15])
chain <- chain[,-c(12:15)]
#3���Ƹ�ɾ��
chain <- chain[,-3]
chain_season <- Ave(chain)


tmp <- read.csv('ú̿����5-����.csv')
names(tmp)[1] <- 'time'
tmp$time <- as.POSIXlt(tmp$time, format='%Y/%m/%d')
tmp <- Ave(tmp)
tmp$����ƽ���� <- rowMeans(tmp[,2:6])

chain_season <- merge(chain_season, tmp[,c(1,7)], by='time')


p_coal <- read.csv('ú̿����6.csv')
names(p_coal)[1] <- 'time'
p_coal$time <- as.POSIXlt(p_coal$time, format = '%Y/%m/%d')
p_coal <- Ave(p_coal)
#����֮��ѡ��9��"�ػʵ���.ƽ�ּ�.ɽ���Ż�.Q5500K."
# p_coal <- merge(netin[,1:2], p_coal, by='time')
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
# p_coal <- p_coal[,c(1,9)]
chain_season <- merge(chain_season, p_coal, by='time')

#��ӳɱ�����
tmp <- read.csv('ú-�ɱ�.csv')
names(tmp)[1] <- 'time'
tmp$time <- as.POSIXlt(tmp$time, format='%Y-%m-%d')
tmp <- Ave(tmp)
#���͡��޷�ֹܡ����ͼ۸�ú̿�����˼۲�����
tmp <- tmp[,-grep('����|�޷�ֹ�|����|ú̿�����˼�', names(tmp))]
chain_season <- merge(chain_season,tmp)

#�������ɱ�
tmp <- read.csv('ú̿�ɱ�--��·��·�˷�.csv')
names(tmp)[1] <- 'time'
tmp$time <- as.POSIXlt(tmp$time, format='%Y-%m-%d')
tmp <- Ave(tmp)
#���з���۸�.��·���˲�����
tmp <- tmp[,-grep('���з���۸�.��·����', names(tmp))]
chain_season <- merge(chain_season,tmp)


#�������
tmp <- read.csv('��Ҫ��ҵ��Ʒ��������.csv')
names(tmp)[1] <- 'time'
tmp$time <- as.POSIXlt(tmp$time, format='%Y/%m/%d')
tmp <- Ave(tmp)
#�й�ʯ��.����.�ϳ���άԭ�ϼ��ۺ���.�ۼ�ֵ �й�ʯ��.����.�ϳ���.�ۼ�ֵ ���²���ʱ
tmp <- tmp[,-grep('�й�ʯ��.����', names(tmp))]
chain_season <- merge(chain_season,tmp)

#ú̿����
tmp <- read.csv('ú̿Ӫ��-ú������.csv')
names(tmp)[1] <- 'time'
tmp$time <- as.POSIXlt(tmp$time, format='%Y-%m-%d')
tmp <- Ave(tmp, ac=T)
# tmp <- tmp[,-grep('�й�ʯ��.����', names(tmp))]
chain_season <- merge(chain_season,tmp)


names(chain_season)


#merge X and Y ####
y_lag1 <- data.frame(time=revenue$time + 1, y_lag1=revenue$revenue)
X <- merge(y_lag1, chain_season)
X[X==0] <- NA
na1 <- apply(X, 1, function(x){sum(is.na(x))>0.5})
X <- X[!na1,]

dat_coal <- merge(revenue, X, by='time')
t <- dat_coal[,1]
dat_coal <- dat_coal[,-1]

#model#####
# Y <- dat_coal$revenue
# Y_lag1 <- dat_coal$y_lag1
# X <- dat_coal[-c(1,2)]
# vif=15

#model����ͨ��marginal-r2ɸѡ�Ա���������vif�����������
#mustΪ�ֶ��趨�ı������ģ�͵ı����к�
#���$modeΪģ�ͣ�$indexΪ��ѡģ�͵��Ա����к�
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

m <- Model(dat_coal$revenue,
           dat_coal[-1],
           vif = 10,
           method = 'aic'
)

mean(abs(m$model$residuals/dat_coal$revenue))
summary(m$model)
vif(m$model)

df <- data.frame(time=t,
                 actual=dat_coal$revenue,
                 pred=m$model$fitted.values)
df <- melt(df, id='time')
ggplot(df, aes(time, value, color=variable)) + geom_line()

predict(m$model,X[X == '2017 Q2',m$index+1], se.fit = T, interval = 'confidence')



index <- m$index[-c(2,3,5,7,8)]
lm1 <- lm(revenue~., dat_coal[,c(0,index)+1])
summary(lm1)
mean(abs(lm1$residuals/dat_coal$revenue))

vif(lm1)

# sink('ú̿��ҵӪ��ģ��.txt')
# summary(lm1)
# lm1$coefficients
# sink()

df <- data.frame(time=t,
                 actual=dat_coal$revenue,
                 pred=lm1$fitted.values)
# df <- melt(df, id='time')
# ggplot(df, aes(time, value, color=variable)) + geom_line()
write.csv(df, file = 'ú̿��ҵӪ��.csv')
predict(lm1,X[X$time == '2017 Q2',index+1],se.fit = T,interval = 'confidence')



