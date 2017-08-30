setwd("E:/FJC/")
load("data.Rdata")
#时间格式调整
index <- which(!is.na(as.numeric(data$report_period)))
tmp <- data$report_period[index]
tmp <- as.POSIXct(as.numeric(tmp)*86400, origin = "1970-01-01")
data$report_period <- as.POSIXlt(data$report_period, format = '%Y-%m-%d')
data$report_period[index] <- tmp
which(is.na(data$report_period))


#调整列名
en <- which(1:length(names(data)) %in% grep('[A-Za-z]',names(data)))
name <- names(data)[-en]
index <- regexpr('[\u4e00-\u9fa5]+$' , name)
names(data)[-en] <- substring(name, index, index + attr(index, 'match.length'))
names(data)
#另存为report.rda
report <- data
save(report, file = 'report.rda')
