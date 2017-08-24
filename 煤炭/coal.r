library(ggplot2)
library(corrplot)
setwd("C:/Users/dell/Desktop/暖流")
coal <- read.csv('煤炭开采行业财务数据.csv')
coal$报告期 <- as.POSIXct(coal$报告期, format='%Y/%m/%d')
coal$year <- as.numeric(format(coal$报告期, format='%Y'))

str(coal)

# #make diff################
# diff_name <- c(F, abs(diff(as.numeric(coal$公司名称))) < 0.5)
# 
# coal$year <- as.numeric(format(coal$报告期, format='%Y'))
# diff_year <- c(F, diff(coal$year) < 0.5)
# 
# index <- as.numeric(diff_name & diff_year)
# income <- c(coal$净利润[1], diff(coal$净利润))
# income <- income*index + coal$净利润*(1-index)
# 
# # check <- cbind(coal[,c(1,3)], coal$year, coal$净利润, income)
# # write.csv(check, file='check.csv')
# 
# 
# #seasonal average################
# 
# diff_month <- c(as.numeric(format(coal$报告期[1], format='%Y%m')),
#                 diff(as.numeric(format(coal$报告期, format='%Y%m'))))
# month <- as.numeric(format(coal$报告期,format='%m'))
# 
# diff_season <- diff_month/3*index + month*(1-index)/3
# #diff_season <- apply(data.frame(diff_month, month),1,min)/3*index + month*(1-index)/3
# 
# # check <- data.frame(coal[,c(1,3)], diff_season)
# # write.csv(check, file='check.csv')
# 
# income_seasonal <- rep(income/diff_season, diff_season)
# name_seasonal <- rep(coal$公司名称, diff_season)
# month_seasonal <- rep(as.numeric(format(coal$报告期, format='%Y%m')), diff_season)
# seasonal_index <- rep(diff_season, diff_season)
# 
# index4 <- which(seasonal_index == 4)
# index3 <- which(seasonal_index == 3)
# index2 <- which(seasonal_index == 2)
# 
# index_1 <- c(index2[c(1:length(index2))%%2 == 1],
#              index3[c(1:length(index3))%%3 == 2],
#              index4[c(1:length(index4))%%4 == 3])
# index_2 <- c(index3[c(1:length(index3))%%3 == 1],
#              index4[c(1:length(index4))%%4 == 2])
# index_3 <- c(index4[c(1:length(index4))%%4 == 1])
# 
# month_seasonal[index_1] <- month_seasonal[index_1] - 3
# month_seasonal[index_2] <- month_seasonal[index_2] - 6
# month_seasonal[index_3] <- month_seasonal[index_3] - 9
# 
# # average <- rep((diff_season > 1), diff_season)
# month_seasonal <- paste(floor(month_seasonal/100),'Q', 
#                         floor(month_seasonal%%100/3), sep = '')
# 
# net_income <- data.frame(name_seasonal,
#                          month_seasonal,
#                          income_seasonal)
# names(net_income) <- c('name', 'time', 'NetIncome')
# write.csv(net_income, file = 'net_income.csv')


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

netin <- report_clean(coal$公司名称, coal$报告期, coal$净利润)
write.csv(netin, file = 'netin.csv')


#calculation about net income########

netin <- netin[order(netin$time),]
ave <- aggregate(netin$y, by=list(netin$time), mean)
total <- aggregate(netin$y, by=list(netin$time), sum)

write.csv(merge(ave, total, by='Group.1'),file = '净利润.csv')

#show average###########
coal <- coal[order(coal$报告期),]
num <- aggregate(coal$净利润, by=list(coal$报告期), length)


ggplot(as.data.frame(ave),aes(Group.1,x)) + 
  geom_point() + labs(x='', title = '行业净利润平均值')
ggplot(as.data.frame(total),aes(Group.1,x)) + 
  geom_point() + labs(x='', title = '行业净利润总值')

#production chain########
chain <- read.csv('产业链相关因子汇总.csv', header = T)
chain$时间 <- as.POSIXlt(chain$时间, format='%Y/%m/%d')
for(i in 2:ncol(chain)){
  chain[,i] <- as.numeric(as.character(chain[,i]))
}

#chain season######
chain_season <- list()
season <- paste0(format(chain$时间, format='%Y'),
                quarters(chain$时间))
for(i in 2:ncol(chain)){
  chain_season[[i-1]] <- aggregate(chain[,i], 
                                by=list(season),
                                sum)[,2] /
    aggregate(chain[,i], 
              by=list(season),
              FUN = function(x) max(c(1,sum(abs(x) > 0.1))))[,2]
}

chain_season <- as.data.frame(matrix(unlist(chain_season),nrow=length(chain_season[[2]])))
chain_season <- cbind(unique(season), chain_season)
names(chain_season) <- names(chain)
#chain month######
chain_month <- list()
month <- format(chain$时间, format='%Y-%m')
for(i in 2:ncol(chain)){
  chain_month[[i-1]] <- aggregate(chain[,i], 
                                   by=list(month),
                                   sum)[,2] /
    aggregate(chain[,i], 
              by=list(month),
              FUN = function(x) max(c(1,sum(abs(x) > 0.1))))[,2]
}

chain_month <- as.data.frame(matrix(unlist(chain_month),nrow=length(chain_month[[2]])))
chain_month <- cbind(unique(month), chain_month)
names(chain_month) <- names(chain)
#power coal######
power_coal <- read.csv('动力煤价格指标.csv', header = T)
power_coal$时间 <- as.POSIXlt(power_coal$时间, format = '%Y/%m/%d')

p_coal <-
aggregate(power_coal$秦皇岛港.平仓价.大同优混.Q5800K., 
          by=list(format(power_coal$时间, format='%Y-%m')),
          sum)[,2] /
  aggregate(power_coal$秦皇岛港.平仓价.大同优混.Q5800K., 
            by=list(format(power_coal$时间, format='%Y-%m')),
            FUN = function(x) max(c(1,sum(abs(x) > 0.1))))[,2]
price_coal <- data.frame(time = unique(format(power_coal$时间, format='%Y-%m')),
                         price = p_coal)
price_coal_d <- data.frame(time = unique(format(power_coal$时间, format='%Y-%m'))[-length(price_coal)],
                           price = p_coal[-1])
########
dat_coal <- merge(price_coal_d, chain_month, by.x = 'time', by.y = '时间')
dat_coal[dat_coal == 0] <- NA
for(i in 2:ncol(dat_coal)){
  print(qplot(dat_coal$time, dat_coal[,i], geom = 'point', main = names(dat_coal)[i]))
  
}
  
dat_coal <- dat_coal[,-1]

dat_coal_scale <- dat_coal
for(i in 1:ncol(dat_coal_scale)){
dat_coal_scale[,i] <- scale(dat_coal_scale[,i])
}
corr <- cor(dat_coal_scale[,which(names(dat_coal_scale) %in% names(lm_s$model))])
which(abs(corr) > 0.8)
corrplot.mixed(corr, tl.pos = 'n', diag = 'n')


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

dat_chain <- merge(y_for, chain_season, by.x = 'season', by.y = '时间')
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

