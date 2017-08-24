setwd("I:/work/genial-flow/钢铁/")
load('steel_XY.rda')

#merge X and Y ####
y_lag1 <- data.frame(time=netin$time + 0.25, y_lag1=netin$netin)
X <- merge(y_lag1, chain_season)
dat_steel <- merge(netin, X, by='time')
na1 <- apply(dat_steel, 1, function(x){sum(is.na(x))>0.5})
t <- dat_steel[,1]
dat_steel <- dat_steel[!na1,-1]

#group & prin####
group <- read.csv('变量分组.csv',header = F)
group[,1] <- sub('[0-9]*','', as.character(group[,1]))
group[,2] <- as.character(group[,2])
group <- group[-which(group[,2] ==''), ]

map <- function(group, dat){
  part <- list()
  for(i in 1:length(unique(group[,2]))){
    vari <- group[group[,2]==unique(group[,2])[i],1]
    tmp <- dat[,which(names(dat) %in% vari)]
    part[[i]] <- data.frame(tmp)
    names(part)[i] <- unique(group[,2])[i]}
  return(part)
}

part <- map(group, dat_steel)

lapply(part, names)

Prin <- function(df, p=2){
  X <- as.matrix(df)
  pcdat = princomp(X,cor = T)
  pca <- X %*% pcdat$loadings
  prin <- pca[,1:min(p,ncol(X))]
  param <- as.matrix(pcdat$loadings[,1:min(p,ncol(X))])
  return(list(data.frame(prin),
              data.frame(param)))
}

tmp <- lapply(part, Prin, p=1)
prin <- lapply(tmp, function(x) x[[1]])
param <-  lapply(tmp, function(x) x[[2]])

tmp <- data.frame(matrix(unlist(prin), nrow=nrow(prin[[1]])))
# colnames(tmp) <- paste(rep(names(prin), lapply(prin, ncol)),
#                        unlist(lapply(lapply(prin, ncol), function(x) seq(1:x))),
#                        sep = '.')
# corr <- cor(tmp)
# corrplot.mixed(corr, diag = 'n', tl.pos = 'lt')
dat_prin <- cbind(dat_steel[,1:2],tmp)
#model####
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

m <- Model(dat_prin$netin,
           dat_prin$y_lag1,
           dat_prin[-c(1,2)],
           vif = 15,
           method = 'aic')

mean(abs(m$model$residuals/dat_steel$netin))
summary(m$model)
vif(m$model)

index <- which(names(dat_prin) %in% names(m$model$model)[-1])

# lm0 <- lm(netin~., dat_prin)
# summary(lm0)
# lm_a <- step(lm0)
# summary(lm_a)
# vif(lm_a)



#predict####
pred <- chain_season[chain_season$time =='2017 Q2',]
map <- function(group, dat){
  part <- list()
  for(i in 1:length(unique(group[,2]))){
    vari <- group[group[,2]==unique(group[,2])[i],1]
    tmp <- dat[,which(names(dat) %in% vari)]
    part[[i]] <- data.frame(tmp)
    names(part)[i] <- unique(group[,2])[i]}
  return(part)
}
pred <- map(group, pred)







predict(lm_a,X[X$time == '2017 Q2',index])

#raw model####
m <- Model(dat_steel$netin,
           dat_steel$y_lag1,
           dat_steel[-c(1,2)],
           vif = 15,
           method = 'aic'
)

mean(abs(m$model$residuals/dat_steel$netin))
summary(m$model)
vif(m$model)


index <- which(names(dat_steel) %in% names(m$model$model))
names(dat_steel)[index]


# tmp <- grep('期货结算价.活跃合约..螺纹钢', names(dat_steel))
# index <- c(tmp,index[-c(2,3,5)])
lm1 <- lm(netin~., dat_steel[,c(1,index[-c(2,3,5)])])
summary(lm1)

df <- data.frame(time=t,
                 actual=dat_steel$netin,
                 pred=lm1$fitted.values)
# df <- melt(df, id='time')
# ggplot(df, aes(time, value, color=variable)) + geom_line()
write.csv(df, file = '钢铁行业模型拟合.csv')


index <- which(names(dat_steel) %in% names(lm1$model))[-1]
predict(lm1,X[X$time =='2017 Q2',index], interval = 'confidence', se.fit = T)

