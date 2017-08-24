#data from：曹哲 report.rda (data.RData)
#data to: 邢家铭
#提取报表中的营业收入，用以计算行业集中度

setwd("I:work/genial-flow/")
load('report.rda') 

name <- c('钢铁', '煤炭', '玻璃', '铜', '水泥')

# field <- '钢铁'
# level <- '申万二级'
# content <- c('corp_name', 'report_period', '营业总收入')
extract <- function(field, level='申万二级', content=c('corp_name', 'report_period', '营业总收入'), df=report){
  level <- which(names(df) == level)
  if(length(level) == 0){return('error: inexist level')}
  tmp <- which(names(df) %in% content)
  if(length(tmp) != length(content)){return('error: inexist content (one or more)')
    }else{content <- tmp}
  index <- grep(field, df[ ,level])
  return(df[index,tmp])
}

income <- lapply(name, extract)
#铜是三级产业
income[[4]] <- extract('铜', level = '申万三级')
#检验行数
lapply(income, nrow)
names(income) <- name

for(i in 1:length(income)){
  write.csv(income[[i]], file = paste0(name[i],'-总营业收入','.csv'))
}

