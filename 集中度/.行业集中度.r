#导出企业营收，用以计算集中度
setwd("E:/FJC/")
load("report.rda")
setwd("E:/FJC/集中度/")
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

index <- grep('白酒', report$申万三级)
tmp <- report[index,]
write.csv(tmp[,c(2,4,22)] ,file='白酒-营业总收入.csv',row.names=F)

index <- grep('白色家电', report$申万二级)
tmp <- report[index,]
write.csv(tmp[,c(2,4,22)] ,file='白色家电-营业总收入.csv',row.names=F)

index <- grep('包装印刷', report$申万二级)
tmp <- report[index,]
write.csv(tmp[,c(2,4,22)] ,file='包装印刷-营业总收入.csv',row.names=F)

index <- grep('电机', report$申万二级)
tmp <- report[index,]
write.csv(tmp[,c(2,4,22)] ,file='电机-营业总收入.csv',row.names=F)

index <- grep('造纸', report$申万二级)
tmp <- report[index,]
write.csv(tmp[,c(2,4,22)] ,file='造纸-营业总收入.csv',row.names=F)

index <- grep('彩电', report$申万三级)
tmp <- report[index,]
write.csv(tmp[,c(2,4,22)] ,file='彩电-营业总收入.csv',row.names=F)

index <- grep('一般零售', report$申万二级)
tmp <- report[index,]
write.csv(tmp[,c(2,4,22)] ,file='一般零售-营业总收入.csv',row.names=F)

index <- grep('塑料', report$申万二级)
tmp <- report[index,]
write.csv(tmp[,c(2,4,22)] ,file='塑料-营业总收入.csv',row.names=F)

index <- grep('火电', report$申万三级)
tmp <- report[index,]
write.csv(tmp[,c(2,4,22)] ,file='火电-营业总收入.csv',row.names=F)

index <- grep('铝', report$申万三级)
tmp <- report[index,]
write.csv(tmp[,c(2,4,22)] ,file='铝-营业总收入.csv',row.names=F)
