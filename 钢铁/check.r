setwd("I:/work/genial-flow/¸ÖÌú/")
load('steel_XY.rda')

dat_check <- merge(netin, chain_season)
tmp <- dat_check
tmp$time <- tmp$time + 1
names(tmp)[-1] <- paste0(names(tmp)[-1],'_lag1')
dat_check <- merge(dat_check, tmp)
t <- dat_check[,1]
dat_check <- dat_check[,-1]
names(dat_check)
corr <- cor(dat_check)
sort(abs(corr[,1]),decreasing = T)


s <- dat_check$MySpicÖ¸Êı.¶ÆĞ¿
df <- data.frame(time=t,
                 actual=dat_check$netin,
                 pred=s)
df <- melt(df, id='time')
ggplot(df, aes(time, value, color=variable)) + geom_line()


qplot(t,s, geom = 'line')
