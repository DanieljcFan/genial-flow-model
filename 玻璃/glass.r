library(ggplot2)
library(lubridate) # date management
library(zoo) #date management
library(corrplot) #covariance visualization
library(glmnet) #lasso regression
library(car) #vif detection
library(reshape2) #mdataframe reshape
#load company report
load("I:/work/genial-flow/report.rda")

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

#company netin####
index <- grep('[����]', report$�������)
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

dat_glass <- merge(netin, chain_season, by='time')
t <- dat_glass[,1]
dat_glass <- dat_glass[,-1]

##�޳�ʯӢɰ����
#dat_glass <- dat_glass[,-grep('ʯӢɰ', names(dat_glass))]
#ʯӢɰ������ȱʧ
dat_glass[dat_glass == 0] <- NA
na1 <- apply(dat_glass,1, function(x){sum(is.na(x))>0.5})

X <- as.matrix(dat_glass[!na1,-1])
Y <- dat_glass[!na1,1]
# model <- cv.glmnet(X,Y,family="binomial",type.measure="deviance")
model <- cv.glmnet(X,Y)
# ����CV����ͼ��ѡ�����lambdaֵ
plot(model)
model$lambda.1se
# ��ȡ����ģ��
model.final <- model$glmnet.fit
# ȡ�ü��ģ�͵Ĳ���ϵ��
model.coef <- coef(model$glmnet.fit, s = model$lambda.1se)
# ȡ��ԭʼģ�͵Ĳ���ϵ��
all.coef <- coef(model$glmnet.fit, s =  min(model.final$lambda))
#�ֶ��趨lambda
model.coef <- coef(model$glmnet.fit, s = model$lambda[85])


coef <- as.matrix(model.coef)
index <- c(1, which(names(dat_glass) %in% rownames(coef)[abs(coef) > 0.01]))
# dy <- c(dat_glass$netin[-1],NA)[!na1]
#hard inout
index <- index[-c(11,12)]
lm1 <- lm(netin~., data = dat_glass[!na1,index])
summary(lm1)

lm_a <- step(lm1)
summary(lm_a)
vif(lm_a)
sink('������ҵ����ģ��.txt')
summary(lm_a)
print(lm_a$coefficients)
sink()

df <- data.frame(time=t[!na1],
                 actual=dat_glass$netin[!na1],
                 pred=lm_a$fitted.values)
# df <- melt(df, id='time')
# ggplot(df, aes(time, value, color=variable)) + geom_line()
write.csv(df, file = '������ҵģ�����.csv')

index <- which(names(chain_season) %in% names(lm_a$model))
predict(lm_a,chain_season[chain_season$time == '2017 Q2',index])

# mean(abs(lm_a$residuals/lm_a$model$netin)[lm_a$model$netin > 0])

