#������ҵӪ�գ����Լ��㼯�ж�
setwd("E:/FJC/")
load("report.rda")
index <- grep('��', report$�������)
tmp <- report[index,]
write.csv(tmp[,c(2,4,22)] ,file='��-Ӫҵ������.csv',row.names=F)

index <- grep('��֯��װ', report$����һ��)
tmp <- report[index,]
write.csv(tmp[,c(2,4,22)] ,file='��֯��װ-Ӫҵ������.csv',row.names=F)

index <- grep('ʯ�ͻ���', report$�������)
tmp <- report[index,]
write.csv(tmp[,c(2,4,22)] ,file='ʯ�ͻ���-Ӫҵ������.csv',row.names=F)

index <- grep('ơ��', report$��������)
tmp <- report[index,]
write.csv(tmp[,c(2,4,22)] ,file='ơ��-Ӫҵ������.csv',row.names=F)

index <- grep('��ɫ�ҵ�', report$�������)
tmp <- report[index,]
write.csv(tmp[,c(2,4,22)] ,file='��ɫ�ҵ�-Ӫҵ������.csv',row.names=F)

index <- grep('��װӡˢ', report$�������)
tmp <- report[index,]
write.csv(tmp[,c(2,4,22)] ,file='��װӡˢ-Ӫҵ������.csv',row.names=F)
