library(zoo)
setwd('E:/FJC/指标提取/')
load("indexvalue.Rdata")
indexvalue$报告期 <- as.POSIXlt(indexvalue$报告期, format = '%Y-%m-%d')


factor <- read.csv('factor.csv')

lapply(unique(factor$corp_name), function(x) {dir.create(paste0(x,'/'))})

index <- lapply(factor$factor,  function(x){length(grep(x,names(indexvalue))) >= 1})
index <- do.call(rbind, index)
write.csv(factor[!index,], file = 'error.csv', row.names = F)
factor <- factor[index,]


Extract <- function(input,wd=''){
  corp_name <- input[1]
  factor <- input[2]
  cat(corp_name, ' ')
  tmp <- indexvalue[grep(corp_name, indexvalue$公司名称),c(1, 2, grep(factor, names(indexvalue)))]
  tmp <- tmp[as.yearqtr(tmp$报告期) >= 2016,]
  write.csv(tmp, file = paste0(wd, corp_name, '/',factor,'.csv'), row.names = F)
}

apply(factor,1,Extract)

#手动提取
corp_name <- '中国供销集团'
factor <- '存货周转率2'
cat(corp_name, ' ')
tmp <- indexvalue[grep(corp_name, indexvalue$公司名称),c(1, 2, grep(factor, names(indexvalue)))]
tmp <- tmp[as.yearqtr(tmp$报告期) >= 2016,]
write.csv(tmp, file = paste0(corp_name, '/',factor,'.csv'), row.names = F)

#补充
patch <- read.csv('patch.csv')
levels(patch$factor) <- sub(' ','', levels(patch$factor))

index <- lapply(patch$factor,  function(x){length(grep(x,names(indexvalue))) >= 1})
index <- do.call(rbind, index)
print(patch[!index,])
patch <- patch[index,]
lapply(unique(patch$corp_name), function(x) {dir.create(paste0('patch/',x,'/'))})



apply(patch,1,Extract, wd='patch/')


