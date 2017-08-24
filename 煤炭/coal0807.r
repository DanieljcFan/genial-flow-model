library(ggplot2)
library(corrplot)
setwd("I:/work/genial-flow/ú̿/")
coal <- read.csv('ú̿������ҵ��������.csv')
coal$������ <- as.POSIXlt(coal$������, format='%Y/%m/%d')
coal$year <- as.numeric(format(coal$������, format='%Y'))

str(coal)

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

netin <- report_clean(coal$��˾����, coal$������, coal$������)
# write.csv(netin, file = 'netin.csv')


#calculation about net income########

netin <- netin[order(netin$time),]
ave <- aggregate(netin$y, by=list(netin$time), mean)
total <- aggregate(netin$y, by=list(netin$time), sum)

netin <- merge(ave, total, by='Group.1')
names(netin) <- c('time', 'ave', 'sum')
# write.csv(merge(ave, total, by='Group.1'),file = '������.csv')

#show average###########
coal <- coal[order(coal$������),]
num <- aggregate(coal$������, by=list(coal$������), length)


ggplot(as.data.frame(ave),aes(Group.1,x)) + 
  geom_point() + labs(x='', title = '��ҵ������ƽ��ֵ')
ggplot(as.data.frame(total),aes(Group.1,x)) + 
  geom_point() + labs(x='', title = '��ҵ��������ֵ')


#chain read in and clean########
chain <- read.csv('��ҵ��������ӻ���.csv', header = T)
chain$ʱ�� <- as.POSIXlt(chain$ʱ��, format='%Y/%m/%d')
for(i in 2:ncol(chain)){
  chain[,i] <- as.numeric(as.character(chain[,i]))
}

#ave���������ݼ��е�ָ�갴�»򼾶�ƽ��
#Ĭ��ʱ���ǩ�ڵ�һ�У���ʽΪPOSIXlt������������Ϣ
Ave <- function(dat, t=1, type='season'){
  library(zoo)
  df <- list()
  if(type == 'season'){time <- as.yearqtr(dat[,t])}
  if(type == 'month'){time <- format(dat[,t], format='%Y-%m')}
  dat <- cbind(dat[,t],dat[,-t])
  for(i in 2: ncol(dat)){
    df[[i-1]] <- aggregate(dat[,i],by=list(time),sum, na.rm = T)[,2] /
      aggregate(dat[,i], by=list(time),
                FUN = function(x) max(c(1,sum(abs(x) > 0.1, na.rm = T))))[,2]
  }
  
  df <- as.data.frame(matrix(unlist(df),nrow=length(df[[1]])))
  df <- cbind(unique(time), df)
  names(df) <- c('time',names(dat)[-1])
  return(df)
  }

chain_season <- Ave(chain)
chain_month <- Ave(chain, type = 'month')

#power coal######
power_coal <- read.csv('����ú�۸�ָ��.csv', header = T)
power_coal$ʱ�� <- as.POSIXlt(power_coal$ʱ��, format = '%Y/%m/%d')

p_coal <-
  aggregate(power_coal$�ػʵ���.ƽ�ּ�.��ͬ�Ż�.Q5800K., 
            by=list(format(power_coal$ʱ��, format='%Y-%m')),
            sum)[,2] /
  aggregate(power_coal$�ػʵ���.ƽ�ּ�.��ͬ�Ż�.Q5800K., 
            by=list(format(power_coal$ʱ��, format='%Y-%m')),
            FUN = function(x) max(c(1,sum(abs(x) > 0.1))))[,2]
price_coal <- data.frame(time = unique(format(power_coal$ʱ��, format='%Y-%m')),
                         price = p_coal)
price_coal_d <- data.frame(time = unique(format(power_coal$ʱ��, format='%Y-%m'))[-length(price_coal)],
                           price = p_coal[-1])
########
dat_coal <- merge(price_coal_d, chain_month, by = 'time')
dat_coal[dat_coal == 0] <- NA
for(i in 2:ncol(dat_coal)){
  print(qplot(dat_coal$time, dat_coal[,i], geom = 'point', main = names(dat_coal)[i]))
  
}

for(i in vari[-c(3,9,10,12,13,15,16)]){
  print(qplot(dat_coal$time, dat_coal[,i], geom = 'point', main = names(dat_coal)[i]))
  
}




dat_coal <- dat_coal[,-1]

dat_coal_scale <- dat_coal
for(i in 1:ncol(dat_coal_scale)){
  dat_coal_scale[,i] <- scale(dat_coal_scale[,i])
}
# corr <- cor(dat_coal_scale[,which(names(dat_coal_scale) %in% names(lm_s$model))])
# which(abs(corr) > 0.8)
# corrplot.mixed(corr, tl.pos = 'n', diag = 'n')


lm_s <- lm(price~., data = dat_coal_scale[,-c(19:23)])
lm_s <- step(lm_s, scope = list(criterion = "BIC"))
summary(lm_s)


vari <- which(names(dat_coal_scale) %in% names(lm_s$model))
lm_1 <- lm(price~., data = dat_coal_scale[,vari[-c(3,9,10,12,13,15,16)]])
summary(lm_1)
which(names(dat_coal_scale) %in% names(lm_1$model))


lm1 <- lm(price~., data = dat_coal)
lm_aic <- step(lm1)
summary(lm_aic)

#######
y_for <- data.frame(as.character(ave$Group.1)[-nrow(ave)], ave$x[-1])
names(y_for) <- c('season', 'netincome')

dat_chain <- merge(y_for, chain_season, by.x = 'season', by.y = 'ʱ��')
dat_chain <- dat_chain[,-1]
dim(dat_chain)

explain_power <- data.frame(names(dat_chain),value=0)

for(i in 2:ncol(dat_chain)){
  lm1 <- lm(netincome~., data = dat_chain[,c(1,i)])
  explain_power[i,2] <- 1 - var(lm1$residuals)/var(dat_chain$netincome)
}

explain_power[order(explain_power$value, decreasing = T),]

lm1 <- lm(netincome~., data = 
            dat_chain[,c(1, head(order(explain_power$value, decreasing = T),n=20))])
lm_bic <- step(lm1, scope = list(criterion = "BIC"))

summary(lm_bic)


#0808#####
#model for coal price#####
x <- read.csv('��ҵ������0808.csv')
names(x)[1] <- 'ʱ��' 
x$ʱ�� <- as.POSIXlt(x$ʱ��, format='%Y/%m/%d')

for(i in 2:ncol(x)){
  x[,i] <- as.numeric(x[,i])
}
names(x)
str(x)
#12-14 ԭú����
x$ԭú���� <- rowSums(x[,12:15])
x <- x[,-c(12:15)]
str(x)
#3���Ƹ�ɾ��
x <- x[,-3]

x1 <- read.csv('ú̿����5-����.csv')
x1$ʱ�� <- as.POSIXlt(x1$ʱ��, format='%Y/%m/%d')
x1$����ƽ���� <- rowMeans(x1[,2:6])
str(x1)

x <- Ave(x, type = 'month')
x1 <- Ave(x1, type = 'month')

x <- merge(x, x1[,c(1,7)], by='time')

dat_coal <- merge(price_coal_d, x, by = 'time')

# for(i in 2:ncol(dat_coal)){
#   print(qplot(dat_coal$time, dat_coal[,i], geom = 'point', main = names(dat_coal)[i]))
#   
# }

dat_coal[dat_coal == 0] <- NA
dat_coal <- dat_coal[,-1]

#complete observation
na1 <- apply(dat_coal, 1, function(x){sum(is.na(x)) < 0.1})
dat_coal <- dat_coal[na1,]

dat_coal_s <- dat_coal
for(i in 1:ncol(dat_coal_s)){
  dat_coal_s[,i] <- scale(dat_coal_s[,i])
}

# corr <- cor(dat_coal_s)
# corrplot.mixed(corr, tl.pos = 'n', diag = 'n')


lm0 <- lm(price~., data = dat_coal_s)
lm_a <- step(lm0)
lm_b <- step(lm0, scope = list(criterion = "BIC"))

summary(lm0)
summary(lm_a)
summary(lm_b)
#hard input
index <- which(names(dat_coal_s) %in% names(lm_b$model))
lm1 <- lm(price~., data = dat_coal[,index[-c(2,4,6,9)]])
summary(lm1)

sink('����ú�۸�2.txt')
summary(lm1)
print(lm1$coefficients)
sink()
#model for netincome ####
x <- read.csv('��ҵ������0808.csv')
names(x)[1] <- 'ʱ��' 
x$ʱ�� <- as.POSIXlt(x$ʱ��, format='%Y/%m/%d')

for(i in 2:ncol(x)){
  x[,i] <- as.numeric(x[,i])
}
names(x)
str(x)
#12-14 ԭú����
x$ԭú���� <- rowSums(x[,12:15])
x <- x[,-c(12:15)]
str(x)
#3���Ƹ�ɾ��
x <- x[,-3]

x1 <- read.csv('ú̿����5-����.csv')
x1$ʱ�� <- as.POSIXlt(x1$ʱ��, format='%Y/%m/%d')
x1$����ƽ���� <- rowMeans(x1[,2:6])
str(x1)

x <- Ave(x, type = 'season')
x1 <- Ave(x1, type = 'season')

x <- merge(x, x1[,c(1,7)], by='time')

netin[,1] <- as.yearqtr(netin[,1])

p_coal <- read.csv('ú̿����6.csv')
str(p_coal)
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
p_coal <- p_coal[,c(1,9)]

x <- merge(x, p_coal, by='time' )
dat_netin <- merge(netin[,1:2], x, by='time')
t <- dat_netin[,1]
dat_netin <- dat_netin[,-1]
na1 <- apply(dat_netin,1, function(x){sum(is.na(x))>0.5})

lm0 <- lm(ave~., dat_netin)
lm_a <- step(lm0)
summary(lm0)
summary(lm_a)

sink('ú̿��ҵ������.txt')
summary(lm_a)
print(lm_a$coefficients)
sink()

df <- data.frame(time=t[!na1],
                 actual=dat_netin$ave[!na1],
                 pred=lm_a$fitted.values)
# df <- melt(df, id='time')
# ggplot(df, aes(time, value, color=variable)) + geom_line()
write.csv(df, file = 'ú̿��ҵģ�����.csv')

index <- which(names(x) %in% names(lm_a$model)[-1])
predict(lm_a,x[x$time == '2017 Q2',index])





