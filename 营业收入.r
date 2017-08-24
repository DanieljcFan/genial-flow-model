#data from������ report.rda (data.RData)
#data to: �ϼ���
#��ȡ�����е�Ӫҵ���룬���Լ�����ҵ���ж�

setwd("I:work/genial-flow/")
load('report.rda') 

name <- c('����', 'ú̿', '����', 'ͭ', 'ˮ��')

# field <- '����'
# level <- '�������'
# content <- c('corp_name', 'report_period', 'Ӫҵ������')
extract <- function(field, level='�������', content=c('corp_name', 'report_period', 'Ӫҵ������'), df=report){
  level <- which(names(df) == level)
  if(length(level) == 0){return('error: inexist level')}
  tmp <- which(names(df) %in% content)
  if(length(tmp) != length(content)){return('error: inexist content (one or more)')
    }else{content <- tmp}
  index <- grep(field, df[ ,level])
  return(df[index,tmp])
}

income <- lapply(name, extract)
#ͭ��������ҵ
income[[4]] <- extract('ͭ', level = '��������')
#��������
lapply(income, nrow)
names(income) <- name

for(i in 1:length(income)){
  write.csv(income[[i]], file = paste0(name[i],'-��Ӫҵ����','.csv'))
}

