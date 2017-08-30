library(zoo)
setwd('E:/FJC/ָ����ȡ/')
load("indexvalue.Rdata")
indexvalue$������ <- as.POSIXlt(indexvalue$������, format = '%Y-%m-%d')


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
  tmp <- indexvalue[grep(corp_name, indexvalue$��˾����),c(1, 2, grep(factor, names(indexvalue)))]
  tmp <- tmp[as.yearqtr(tmp$������) >= 2016,]
  write.csv(tmp, file = paste0(wd, corp_name, '/',factor,'.csv'), row.names = F)
}

apply(factor,1,Extract)

#�ֶ���ȡ
corp_name <- '�й���������'
factor <- '�����ת��2'
cat(corp_name, ' ')
tmp <- indexvalue[grep(corp_name, indexvalue$��˾����),c(1, 2, grep(factor, names(indexvalue)))]
tmp <- tmp[as.yearqtr(tmp$������) >= 2016,]
write.csv(tmp, file = paste0(corp_name, '/',factor,'.csv'), row.names = F)

#����
patch <- read.csv('patch.csv')
levels(patch$factor) <- sub(' ','', levels(patch$factor))

index <- lapply(patch$factor,  function(x){length(grep(x,names(indexvalue))) >= 1})
index <- do.call(rbind, index)
print(patch[!index,])
patch <- patch[index,]
lapply(unique(patch$corp_name), function(x) {dir.create(paste0('patch/',x,'/'))})



apply(patch,1,Extract, wd='patch/')


