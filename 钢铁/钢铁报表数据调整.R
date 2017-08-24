setwd("I:/work/genial-flow/钢铁/")
load("gt.Rdata")
steel <- gt
rm(gt)
# 鞍钢时间格式调整
index <- which(!is.na(as.numeric(steel$report_period)))
tmp <- steel$report_period[index]
tmp <- as.POSIXct(as.numeric(tmp)*86400, origin = "1970-01-01")
steel$report_period <- as.POSIXlt(steel$report_period, format = '%Y-%m-%d')

steel$report_period[index] <- tmp

#调整列名
en <- which(1:length(names(steel)) %in% grep('[A-Za-z]',names(steel)))
name <- names(steel)[-en]
index <- regexpr('[\u4e00-\u9fa5]+$' , name)
names(steel)[-en] <- substring(name, index, index + attr(index, 'match.length'))

save(steel, file = 'steel.rda')
