#导出企业营收，用以计算集中度
setwd("E:/FJC/")
load("report.rda")
index <- grep('橡胶', report$申万二级)
tmp <- report[index,]
write.csv(tmp[,c(2,4,22)] ,file='橡胶-营业总收入.csv',row.names=F)

index <- grep('纺织服装', report$申万一级)
tmp <- report[index,]
write.csv(tmp[,c(2,4,22)] ,file='纺织服装-营业总收入.csv',row.names=F)

index <- grep('石油化工', report$申万二级)
tmp <- report[index,]
write.csv(tmp[,c(2,4,22)] ,file='石油化工-营业总收入.csv',row.names=F)

index <- grep('啤酒', report$申万三级)
tmp <- report[index,]
write.csv(tmp[,c(2,4,22)] ,file='啤酒-营业总收入.csv',row.names=F)

index <- grep('白色家电', report$申万二级)
tmp <- report[index,]
write.csv(tmp[,c(2,4,22)] ,file='白色家电-营业总收入.csv',row.names=F)

index <- grep('包装印刷', report$申万二级)
tmp <- report[index,]
write.csv(tmp[,c(2,4,22)] ,file='包装印刷-营业总收入.csv',row.names=F)
